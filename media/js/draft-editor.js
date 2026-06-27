class DraftHistory {
  constructor(bodyEl, titleEl, maxStates = 120) {
    this.bodyEl = bodyEl;
    this.titleEl = titleEl;
    this.maxStates = maxStates;
    this.undoStack = [];
    this.redoStack = [];
    this.lastState = null;
    this.isApplying = false;
    this.debounceTimer = null;
    this.refreshImageBlocks = null;
  }

  getSelectionState() {
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) return null;
    const range = sel.getRangeAt(0);
    if (!this.bodyEl.contains(range.commonAncestorContainer) && !(this.titleEl && this.titleEl.contains(range.commonAncestorContainer))) return null;
    return this._serializeRange(range, this.bodyEl);
  }

  _serializeRange(range, root) {
    try {
      const start = this._serializePoint(range.startContainer, range.startOffset, root);
      const end = this._serializePoint(range.endContainer, range.endOffset, root);
      if (!start || !end) return null;
      return { start, end };
    } catch (_) { return null; }
  }

  _serializePoint(node, offset, root) {
    const path = [];
    let cur = node;
    while (cur && cur !== root) {
      const parent = cur.parentNode;
      if (!parent) return null;
      let idx = 0;
      let sibling = cur.previousSibling;
      while (sibling) { idx++; sibling = sibling.previousSibling; }
      path.unshift(idx);
      cur = parent;
    }
    return { path, offset };
  }

  _deserializeRange(state, root) {
    try {
      const startNode = this._resolvePath(state.start.path, root);
      const endNode = this._resolvePath(state.end.path, root);
      if (!startNode || !endNode) return null;
      const range = document.createRange();
      range.setStart(startNode, Math.min(state.start.offset, this._maxOffset(startNode)));
      range.setEnd(endNode, Math.min(state.end.offset, this._maxOffset(endNode)));
      return range;
    } catch (_) { return null; }
  }

  _resolvePath(path, root) {
    let node = root;
    for (let i = 0; i < path.length; i++) {
      if (!node.childNodes) return null;
      node = node.childNodes[path[i]];
      if (!node) return null;
    }
    return node;
  }

  _maxOffset(node) {
    return node.nodeType === Node.TEXT_NODE ? (node.textContent || '').length : (node.childNodes ? node.childNodes.length : 0);
  }

  snapshot(label = 'edit') {
    return {
      bodyHtml: this.bodyEl.innerHTML,
      titleText: this.titleEl ? this.titleEl.innerText : '',
      selection: this.getSelectionState(),
      label
    };
  }

  push(label = 'edit') {
    if (this.isApplying) return;
    const state = this.snapshot(label);
    if (this.lastState && this.lastState.bodyHtml === state.bodyHtml && this.lastState.titleText === state.titleText) return;
    this.undoStack.push(state);
    if (this.undoStack.length > this.maxStates) this.undoStack.shift();
    this.redoStack = [];
    this.lastState = state;
  }

  debouncedPush(label = 'edit', delay = 400) {
    clearTimeout(this.debounceTimer);
    this.debounceTimer = setTimeout(() => this.push(label), delay);
  }

  restore(state) {
    if (!state) return;
    this.isApplying = true;
    this.bodyEl.innerHTML = state.bodyHtml;
    if (this.titleEl) this.titleEl.innerText = state.titleText;
    this.bodyEl.querySelectorAll('pre.code-block').forEach(pre => this._ensureCodeBlockActions(pre));
    if (this.refreshImageBlocks) this.refreshImageBlocks();
    if (state.selection) {
      const range = this._deserializeRange(state.selection, this.bodyEl);
      if (range) {
        const sel = window.getSelection();
        sel.removeAllRanges();
        sel.addRange(range);
      }
    }
    this.isApplying = false;
  }

  _ensureCodeBlockActions(pre) {
    if (!pre || pre.querySelector('.de-code-block-actions')) return;
    const code = pre.querySelector('code');
    const codeHtml = code ? code.innerHTML : pre.innerHTML;
    const theme = pre.getAttribute('data-theme') || 'light';
    pre.innerHTML = `<div class="de-code-block-actions" contenteditable="false"><button class="de-code-theme-toggle" type="button" title="Toggle theme">◐</button></div><code>${codeHtml || '<br>'}</code>`;
    pre.setAttribute('data-theme', theme);
    pre.querySelector('.de-code-theme-toggle').addEventListener('click', (e) => {
      e.preventDefault(); e.stopPropagation();
      const current = pre.getAttribute('data-theme') || 'light';
      const next = current === 'dark' ? 'light' : 'dark';
      pre.setAttribute('data-theme', next);
    });
  }

  undo() {
    if (this.undoStack.length <= 1) return false;
    const current = this.snapshot('current');
    this.redoStack.push(current);
    this.undoStack.pop();
    const prev = this.undoStack[this.undoStack.length - 1];
    this.restore(prev);
    return true;
  }

  redo() {
    if (!this.redoStack.length) return false;
    const next = this.redoStack.pop();
    this.undoStack.push(next);
    this.restore(next);
    return true;
  }

  canUndo() { return this.undoStack.length > 1; }
  canRedo() { return this.redoStack.length > 0; }
}

class DraftEditor {
  constructor(opts) {
    this.opts = Object.assign({
      titleEl: null,
      bodyEl: null,
      statusEl: null,
      saveEndpoint: '/editor/api/save',
      uploadEndpoint: '/editor/api/upload',
      fetchMetaEndpoint: '/editor/api/fetch-meta',
      loadDraftEndpoint: '/editor/api/load-draft',
      onChange: null,
      onSave: null,
      initialMode: 'draft',
      initialTargetPath: '',
    }, opts);

    this.titleEl = typeof this.opts.titleEl === 'string' ? document.querySelector(this.opts.titleEl) : this.opts.titleEl;
    this.bodyEl = typeof this.opts.bodyEl === 'string' ? document.querySelector(this.opts.bodyEl) : this.opts.bodyEl;
    this.statusEl = typeof this.opts.statusEl === 'string' ? document.querySelector(this.opts.statusEl) : this.opts.statusEl;

    this.activeDraftTargetPath = this.opts.initialTargetPath;
    this.activeSourceMode = this.opts.initialMode;
    this.savedRange = null;
    this.dirty = false;
    this.tags = [];
    this.history = new DraftHistory(this.bodyEl, this.titleEl);
    this.history.refreshImageBlocks = () => this._refreshImageBlocks();

    this.slashState = { open: false, query: '', index: 0, triggerBlock: null };
    this.insertState = { open: false, referenceBlock: null };
    this.activeBlockForPlus = null;

    this.SLASH_ITEMS = [
      { id: 'h1', label: 'Heading 1', desc: '#', icon: 'H1', action: () => this.transformCurrentBlock('h1') },
      { id: 'h2', label: 'Heading 2', desc: '##', icon: 'H2', action: () => this.transformCurrentBlock('h2') },
      { id: 'h3', label: 'Heading 3', desc: '###', icon: 'H3', action: () => this.transformCurrentBlock('h3') },
      { id: 'quote', label: 'Quote', desc: '>', icon: '"', action: () => this.transformCurrentBlock('blockquote') },
      { id: 'code', label: 'Code block', desc: '```', icon: '{ }', action: () => this.insertCodeBlockPrompt() },
      { id: 'source', label: 'Source block', desc: '#+BEGIN_SRC', icon: 'SRC', action: () => this.insertSourceBlockPrompt() },
      { id: 'ditaa', label: 'Ditaa diagram', desc: 'diagram', icon: 'DIA', action: () => this.insertDitaaDiagram() },
      { id: 'plantuml', label: 'PlantUML diagram', desc: 'uml', icon: 'UML', action: () => this.insertPlantumlDiagram() },
      { id: 'ascii', label: 'ASCII diagram', desc: 'ascii', icon: 'ASC', action: () => this.insertAsciiDiagram() },
      { id: 'ul', label: 'Bulleted list', desc: '-', icon: '•', action: () => this.transformCurrentBlock('ul') },
      { id: 'ol', label: 'Numbered list', desc: '1.', icon: '1.', action: () => this.transformCurrentBlock('ol') },
      { id: 'table', label: 'Table', desc: '|', icon: '▦', action: () => this.insertTable() },
      { id: 'equation', label: 'Equation', desc: 'TeX', icon: '∑', action: () => this.insertEquation() },
      { id: 'image', label: 'Image', desc: 'URL/upload', icon: '🖼', action: () => this.promptImageUrl() },
      { id: 'upload', label: 'Upload image', desc: 'file', icon: '↑', action: () => this.uploadInput.click() },
      { id: 'video', label: 'Video', desc: 'YouTube/Vimeo', icon: '▶', action: () => this.insertVideo() },
      { id: 'embed', label: 'Link card', desc: 'URL', icon: '🔗', action: () => this.insertLinkCard() },
      { id: 'hr', label: 'Divider', desc: '---', icon: '—', action: () => this.insertDivider() },
    ];

    this.CONTINUOUS_BLOCKS = ['p', 'blockquote', 'ul', 'ol', 'pre'];

    this.CODE_LANGUAGES = [
      ['text', 'Plain text'], ['bash', 'Bash'], ['c', 'C'], ['cpp', 'C++'], ['css', 'CSS'],
      ['elixir', 'Elixir'], ['go', 'Go'], ['graphql', 'GraphQL'], ['haskell', 'Haskell'],
      ['html', 'HTML'], ['java', 'Java'], ['javascript', 'JavaScript'], ['json', 'JSON'],
      ['latex', 'LaTeX'], ['markdown', 'Markdown'], ['python', 'Python'], ['ruby', 'Ruby'],
      ['rust', 'Rust'], ['scheme', 'Scheme'], ['sql', 'SQL'], ['typescript', 'TypeScript'],
      ['yaml', 'YAML'], ['__other__', 'Other...']
    ];
    this.CODE_SYNTAX_THEMES = [['default', 'Default'], ['dark', 'Dark'], ['monokai', 'Monokai'], ['one-dark', 'One Dark'], ['atom-dark', 'Atom Dark'], ['dracula', 'Dracula'], ['okaidia', 'Okaidia'], ['tomorrow', 'Tomorrow Night']];

    this._injectUi();
    this._bindEvents();
  }

  setStatus(text, bad) {
    if (!this.statusEl) return;
    this.statusEl.textContent = text;
    this.statusEl.style.color = bad ? '#9d2e2e' : '#6d6a64';
  }

  // ---------- Utilities ----------
  static escHtml(v) {
    return String(v || '')
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  static escOrg(v) {
    return String(v || '').replace(/[\r\n]+/g, ' ').trim();
  }

  static isPlaceholderCaption(text, src) {
    if (!text) return true;
    const t = text.toLowerCase();
    if (t.includes('placeholder')) return true;
    const filename = String(src || '').split('/').pop().split('?')[0].toLowerCase();
    if (filename && t === filename) return true;
    if (filename && t === filename.replace(/\.[^.]+$/, '')) return true;
    if (/^(image|img|picture|photo|screenshot|untitled)$/i.test(t)) return true;
    return false;
  }

  static pickNodeText(node) {
    return (node.textContent || '').replace(/\u00a0/g, ' ');
  }

  static toEmbedUrl(raw) {
    try {
      const u = new URL(raw);
      if (u.hostname.includes('youtube.com')) {
        const id = u.searchParams.get('v');
        return id ? `https://www.youtube.com/embed/${id}` : raw;
      }
      if (u.hostname.includes('youtu.be')) {
        const id = u.pathname.replace(/^\//, '');
        return id ? `https://www.youtube.com/embed/${id}` : raw;
      }
      if (u.hostname.includes('vimeo.com')) {
        const id = u.pathname.split('/').filter(Boolean).pop();
        return id ? `https://player.vimeo.com/video/${id}` : raw;
      }
    } catch (_) {}
    return raw;
  }

  isSelectionInBody() {
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) return false;
    return this.bodyEl.contains(sel.getRangeAt(0).commonAncestorContainer);
  }

  rememberSelection() {
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0 || !this.isSelectionInBody()) return;
    this.savedRange = sel.getRangeAt(0).cloneRange();
  }

  restoreSelection() {
    if (!this.savedRange) return false;
    const sel = window.getSelection();
    if (!sel) return false;
    sel.removeAllRanges();
    sel.addRange(this.savedRange);
    return true;
  }

  // ---------- Block model (medium-draft inspired) ----------
  isBlock(el) { return el && el.parentNode === this.bodyEl; }
  getBlocks() { return Array.from(this.bodyEl.children).filter(el => this.isBlock(el)); }

  getCurrentBlock() {
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) return null;
    let node = sel.getRangeAt(0).commonAncestorContainer;
    if (node.nodeType === Node.TEXT_NODE) node = node.parentElement;
    while (node && node !== this.bodyEl && node.parentElement !== this.bodyEl) {
      node = node.parentElement;
    }
    return node && node !== this.bodyEl ? node : null;
  }

  getBlockType(block) {
    if (!block) return 'p';
    const tag = block.tagName.toLowerCase();
    if (tag === 'figure') {
      if (block.classList.contains('image-block')) return 'image';
      if (block.classList.contains('video-embed')) return 'video';
      if (block.classList.contains('embed-card')) return 'embed';
      if (block.classList.contains('table-block')) return 'table';
    }
    if (tag === 'div' && block.classList.contains('math')) return 'math';
    return tag;
  }

  isTextBlock(type) { return ['p', 'h1', 'h2', 'h3', 'blockquote'].includes(type); }
  isVoidBlock(type) { return ['hr', 'image', 'video', 'embed'].includes(type); }
  isContinuous(type) { return this.CONTINUOUS_BLOCKS.includes(type); }

  setEmptyState(block) {
    const type = this.getBlockType(block);
    const empty = (block.textContent || '').trim() === '' && !this.isVoidBlock(type);
    block.classList.toggle('is-empty', empty);
  }

  refreshBlockStates() {
    this.getBlocks().forEach(b => this.setEmptyState(b));
  }

  _refreshImageBlocks() {
    if (!this.bodyEl) return;
    this.bodyEl.querySelectorAll('figure.image-block').forEach(fig => {
      this._addImageResizeHandle(fig);
      this._observeImageSize(fig);
    });
  }

  createParagraph(html = '') {
    const p = document.createElement('p');
    p.innerHTML = html || '<br>';
    return p;
  }

  // Insert a new block after the pivot block (like medium-draft addNewBlockAt)
  addNewBlockAt(pivot, newBlock, focus = true) {
    if (pivot && pivot.nextSibling) {
      this.bodyEl.insertBefore(newBlock, pivot.nextSibling);
    } else if (pivot) {
      this.bodyEl.appendChild(newBlock);
    } else {
      this.bodyEl.appendChild(newBlock);
    }
    this.refreshBlockStates();
    if (focus) this.placeCaretAtEnd(newBlock);
    this._markDirty();
    this._refreshImageBlocks();
  }

  insertBlockBefore(reference, block, focus = true) {
    this.bodyEl.insertBefore(block, reference);
    this.refreshBlockStates();
    if (focus) this.placeCaretAtEnd(block);
    this._markDirty();
    this._refreshImageBlocks();
  }

  removeBlock(block) {
    const prev = block.previousElementSibling;
    block.remove();
    this.refreshBlockStates();
    if (prev && this.isBlock(prev)) this.placeCaretAtEnd(prev);
    this._markDirty();
  }

  placeCaretAtEnd(node) {
    node.focus();
    const sel = window.getSelection();
    const range = document.createRange();
    if (node.tagName.toLowerCase() === 'pre') {
      const code = node.querySelector('code') || node;
      range.selectNodeContents(code);
    } else {
      range.selectNodeContents(node);
    }
    range.collapse(false);
    sel.removeAllRanges();
    sel.addRange(range);
  }

  _placeCaretInCell(cell) {
    if (!cell) return;
    cell.focus();
    const sel = window.getSelection();
    const range = document.createRange();
    range.selectNodeContents(cell);
    range.collapse(false);
    sel.removeAllRanges();
    sel.addRange(range);
  }

  placeCaretAtStart(node) {
    node.focus();
    const sel = window.getSelection();
    const range = document.createRange();
    range.selectNodeContents(node);
    range.collapse(true);
    sel.removeAllRanges();
    sel.addRange(range);
  }

  getRangeHtml(range) {
    const div = document.createElement('div');
    div.appendChild(range.cloneContents());
    return div.innerHTML;
  }

  splitTextBlock(block) {
    const sel = window.getSelection();
    if (!sel.rangeCount) return;
    const range = sel.getRangeAt(0);
    const caretRange = range.cloneRange();
    caretRange.selectNodeContents(block);
    caretRange.setEnd(range.startContainer, range.startOffset);
    const beforeHtml = this.getRangeHtml(caretRange);
    const afterRange = range.cloneRange();
    afterRange.selectNodeContents(block);
    afterRange.setStart(range.endContainer, range.endOffset);
    const afterHtml = this.getRangeHtml(afterRange);

    const newBlock = this.createParagraph(afterHtml || '');
    block.innerHTML = beforeHtml || '<br>';
    this.addNewBlockAt(block, newBlock, true);
    this.setEmptyState(block);
  }

  mergeWithPrevious(block) {
    const prev = block.previousElementSibling;
    if (!prev || !this.isTextBlock(this.getBlockType(prev)) || !this.isTextBlock(this.getBlockType(block))) return false;
    const wasEmpty = prev.innerHTML === '<br>';
    prev.innerHTML = wasEmpty ? block.innerHTML : (prev.innerHTML + ' ' + block.innerHTML);
    block.remove();
    this.refreshBlockStates();
    this.placeCaretAtStart(prev);
    this._markDirty();
    return true;
  }

  // Change block type while preserving text (like medium-draft resetBlockWithType)
  transformCurrentBlock(newType) {
    const block = this.getCurrentBlock();
    if (!block) return;
    const currentType = this.getBlockType(block);
    if (currentType === newType && this.isTextBlock(newType)) return;

    let newBlock;
    if (newType === 'p') newBlock = this.createParagraph(block.innerHTML);
    else if (newType === 'h1') { newBlock = document.createElement('h1'); newBlock.innerHTML = block.innerHTML || '<br>'; }
    else if (newType === 'h2') { newBlock = document.createElement('h2'); newBlock.innerHTML = block.innerHTML || '<br>'; }
    else if (newType === 'h3') { newBlock = document.createElement('h3'); newBlock.innerHTML = block.innerHTML || '<br>'; }
    else if (newType === 'blockquote') { newBlock = document.createElement('blockquote'); newBlock.innerHTML = block.innerHTML || '<br>'; }
    else if (newType === 'ul') {
      newBlock = document.createElement('ul');
      const li = document.createElement('li');
      li.innerHTML = block.innerHTML || '<br>';
      newBlock.appendChild(li);
    }
    else if (newType === 'ol') {
      newBlock = document.createElement('ol');
      const li = document.createElement('li');
      li.innerHTML = block.innerHTML || '<br>';
      newBlock.appendChild(li);
    }
    else return;

    this.bodyEl.replaceChild(newBlock, block);
    this.setEmptyState(newBlock);
    this.refreshBlockStates();
    this.placeCaretAtEnd(newBlock);
    this._markDirty();
  }

  ensureTrailingParagraph() {
    const blocks = this.getBlocks();
    if (blocks.length === 0) {
      this.bodyEl.appendChild(this.createParagraph());
      return;
    }
    const last = blocks[blocks.length - 1];
    const lastType = this.getBlockType(last);
    if (!this.isTextBlock(lastType) && lastType !== 'pre') {
      this.bodyEl.appendChild(this.createParagraph());
    }
  }

  // ---------- UI injection ----------
  _injectUi() {
    this.uploadInput = document.createElement('input');
    this.uploadInput.type = 'file';
    this.uploadInput.accept = 'image/*';
    this.uploadInput.hidden = true;
    this.bodyEl.parentElement.appendChild(this.uploadInput);

    this.slashMenu = document.createElement('div');
    this.slashMenu.className = 'de-slash-menu';
    document.body.appendChild(this.slashMenu);

    this.insertMenu = document.createElement('div');
    this.insertMenu.className = 'de-insert-menu';
    document.body.appendChild(this.insertMenu);

    this.blockPlus = document.createElement('button');
    this.blockPlus.className = 'de-plus-float';
    this.blockPlus.type = 'button';
    this.blockPlus.setAttribute('aria-label', 'Insert block');
    this.blockPlus.textContent = '+';
    document.body.appendChild(this.blockPlus);

    this.bubble = document.createElement('div');
    this.bubble.className = 'de-bubble';
    this.bubble.innerHTML = `
      <button type="button" data-cmd="bold"><b>B</b></button>
      <button type="button" data-cmd="italic"><i>I</i></button>
      <button type="button" data-action="link">Link</button>
      <button type="button" class="de-soft" data-action="h2">H2</button>
      <button type="button" data-action="quote">Q</button>
      <button type="button" data-action="code">{ }</button>
      <button type="button" class="de-hl" data-action="highlight">HL</button>
      <button type="button" data-action="clear-hl">HL-</button>
      <button type="button" data-action="font-size-up" title="Increase font size">A+</button>
      <button type="button" data-action="font-size-down" title="Decrease font size">A-</button>
      <input class="de-inline-color" type="color" value="#ffea55" title="Highlight color">
      <input class="de-text-color" type="color" value="#1c1b19" title="Text color">
      <button type="button" data-action="clear-color">Clr</button>
    `;
    document.body.appendChild(this.bubble);
    this.inlineColor = this.bubble.querySelector('.de-inline-color');
    this.textColor = this.bubble.querySelector('.de-text-color');

    this.inputModal = document.createElement('div');
    this.inputModal.className = 'de-input-modal';
    this.inputModal.hidden = true;
    this.inputModal.innerHTML = `
      <div class="de-input-card" role="dialog" aria-modal="true" aria-labelledby="de-input-title">
        <h3 id="de-input-title" class="de-input-title">Input</h3>
        <label id="de-input-label" class="de-input-label" for="de-input-field">Value</label>
        <input id="de-input-field" class="de-input-field" type="text" autocomplete="off">
        <div class="de-input-actions">
          <button class="de-btn" type="button" data-action="cancel">Cancel</button>
          <button class="de-btn primary" type="button" data-action="ok">OK</button>
        </div>
      </div>
    `;
    document.body.appendChild(this.inputModal);
    this.inputField = this.inputModal.querySelector('#de-input-field');
  }

  // ---------- Selection / bubble ----------
  exec(cmd, value) {
    if (!this.restoreSelection()) this.bodyEl.focus();
    document.execCommand(cmd, false, value || null);
    this.rememberSelection();
    this._markDirty();
    this._showBubble();
  }

  _showBubble() {
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0 || sel.isCollapsed || !this.isSelectionInBody()) {
      this.bubble.style.display = 'none';
      return;
    }
    this._deselectBlocks();
    this._hideBlockToolbar();
    this._hideImageToolbar();
    this._hideMultiToolbar();
    if (this.tableToolbar) this.tableToolbar.classList.remove('open');
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    this.bubble.style.display = 'flex';
    this.bubble.style.top = `${Math.max(8, rect.top - 58)}px`;
    this.bubble.style.left = `${Math.max(8, rect.left + rect.width / 2 - this.bubble.offsetWidth / 2)}px`;
    this.rememberSelection();
  }

  clearInlineHighlight() {
    if (!this.restoreSelection()) return;
    document.execCommand('hiliteColor', false, 'transparent');
    this.bodyEl.querySelectorAll('span').forEach((s) => {
      const bg = s.style.backgroundColor || '';
      if (bg && (bg.includes('rgba(0, 0, 0, 0)') || bg === 'transparent')) {
        s.style.backgroundColor = '';
      }
    });
    this._markDirty();
    this._showBubble();
  }

  clearTextColor() {
    this.exec('foreColor', '#1c1b19');
  }

  _wrapSelectionInline(styleText) {
    if (!this.restoreSelection()) return;
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0 || sel.isCollapsed || !this.isSelectionInBody()) return;
    const range = sel.getRangeAt(0);
    const span = document.createElement('span');
    span.setAttribute('style', styleText);
    try {
      range.surroundContents(span);
      sel.removeAllRanges();
      const r2 = document.createRange();
      r2.selectNodeContents(span);
      sel.addRange(r2);
    } catch (_) {
      const frag = range.extractContents();
      span.appendChild(frag);
      range.insertNode(span);
      sel.removeAllRanges();
      const r2 = document.createRange();
      r2.selectNodeContents(span);
      sel.addRange(r2);
    }
    this._markDirty();
    this._showBubble();
  }

  _getSelectionFontSizePx() {
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0 || sel.isCollapsed) return null;
    let node = sel.getRangeAt(0).commonAncestorContainer;
    if (node.nodeType === Node.TEXT_NODE) node = node.parentElement;
    if (!node) return null;
    const computed = window.getComputedStyle(node);
    const px = parseFloat(computed.fontSize);
    return isNaN(px) ? null : px;
  }

  _changeFontSize(delta) {
    const current = this._getSelectionFontSizePx();
    const base = current || 21;
    let next = Math.round(base + delta);
    next = Math.max(12, Math.min(72, next));
    this._wrapSelectionInline(`font-size:${next}px;`);
  }

  askInput(opts = {}) {
    return new Promise((resolve) => {
      const prior = document.activeElement;
      const title = this.inputModal.querySelector('#de-input-title');
      const label = this.inputModal.querySelector('#de-input-label');
      title.textContent = opts.title || 'Input';
      label.textContent = opts.label || 'Value';
      this.inputField.placeholder = opts.placeholder || '';
      this.inputField.value = opts.value || '';
      this.inputModal.hidden = false;

      const finish = (value) => {
        this.inputModal.hidden = true;
        this.inputField.value = '';
        this.inputField.placeholder = '';
        this.inputModal.removeEventListener('click', onBackdrop);
        this.inputField.removeEventListener('keydown', onKeyDown);
        if (prior && prior.focus) prior.focus();
        resolve(value);
      };
      const onOk = () => finish(this.inputField.value.trim());
      const onCancel = () => finish(null);
      const onBackdrop = (e) => { if (e.target === this.inputModal) finish(null); };
      const onKeyDown = (e) => {
        if (e.key === 'Enter') { e.preventDefault(); onOk(); }
        if (e.key === 'Escape') { e.preventDefault(); onCancel(); }
      };

      this.inputModal.addEventListener('click', onBackdrop);
      this.inputField.addEventListener('keydown', onKeyDown);
      this.inputModal.querySelector('[data-action="ok"]').onclick = onOk;
      this.inputModal.querySelector('[data-action="cancel"]').onclick = onCancel;
      setTimeout(() => this.inputField.focus(), 0);
    });
  }

  // ---------- Slash menu ----------
  _renderSlashMenu() {
    const items = this.SLASH_ITEMS.filter(it =>
      it.label.toLowerCase().includes(this.slashState.query) ||
      it.desc.toLowerCase().includes(this.slashState.query)
    );
    this.slashMenu.innerHTML = '';
    if (items.length === 0) { this._closeSlashMenu(); return; }
    items.forEach((it, idx) => {
      const btn = document.createElement('button');
      btn.className = 'de-slash-item' + (idx === this.slashState.index ? ' active' : '');
      btn.type = 'button';
      btn.innerHTML = `<span class="de-slash-icon">${DraftEditor.escHtml(it.icon)}</span><span class="de-slash-label">${DraftEditor.escHtml(it.label)}</span><span class="de-slash-desc">${DraftEditor.escHtml(it.desc)}</span>`;
      btn.addEventListener('click', (e) => {
        e.preventDefault(); e.stopPropagation();
        this._closeSlashMenu();
        it.action();
      });
      btn.addEventListener('mouseenter', () => { this.slashState.index = idx; this._renderSlashMenu(); });
      this.slashMenu.appendChild(btn);
    });
  }

  _openSlashMenu(block) {
    this.slashState = { open: true, query: '', index: 0, triggerBlock: block };
    this._renderSlashMenu();
    this._positionSlashMenu();
    this.slashMenu.classList.add('open');
  }

  _closeSlashMenu() {
    this.slashState.open = false;
    this.slashMenu.classList.remove('open');
  }

  _positionSlashMenu() {
    const sel = window.getSelection();
    if (!sel.rangeCount) return;
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    this.slashMenu.style.top = `${rect.bottom + 8}px`;
    this.slashMenu.style.left = `${Math.max(8, rect.left)}px`;
  }

  _handleSlashInput(block, text) {
    if (!this.slashState.open) return false;
    const match = text.match(/^\/(.*)$/);
    if (!match) { this._closeSlashMenu(); return false; }
    this.slashState.query = match[1].toLowerCase();
    this.slashState.index = 0;
    this._renderSlashMenu();
    this._positionSlashMenu();
    return true;
  }

  // ---------- Insert menu ----------
  _renderInsertMenu() {
    const items = [
      { id: 'text', label: 'Text', icon: '¶', action: () => this.addNewBlockAt(this.insertState.referenceBlock, this.createParagraph()) },
      { id: 'h2', label: 'H2', icon: 'H2', action: () => this.addNewBlockAt(this.insertState.referenceBlock, document.createElement('h2')) },
      { id: 'h3', label: 'H3', icon: 'H3', action: () => this.addNewBlockAt(this.insertState.referenceBlock, document.createElement('h3')) },
      { id: 'quote', label: 'Quote', icon: '"', action: () => { const b = document.createElement('blockquote'); b.innerHTML = '<br>'; this.addNewBlockAt(this.insertState.referenceBlock, b); } },
      { id: 'code', label: 'Code', icon: '{ }', action: () => this.insertCodeBlockAfter(this.insertState.referenceBlock) },
      { id: 'source', label: 'Source', icon: 'SRC', action: () => this.insertSourceBlockPrompt() },
      { id: 'ditaa', label: 'Ditaa', icon: 'DIA', action: () => this.insertDitaaDiagram() },
      { id: 'plantuml', label: 'PlantUML', icon: 'UML', action: () => this.insertPlantumlDiagram() },
      { id: 'ascii', label: 'ASCII', icon: 'ASC', action: () => this.insertAsciiDiagram() },
      { id: 'table', label: 'Table', icon: '▦', action: () => this.insertTable() },
      { id: 'equation', label: 'Equation', icon: '∑', action: () => this.insertEquation() },
      { id: 'image', label: 'Image', icon: '🖼', action: () => { this._closeInsertMenu(); this.promptImageUrl().then(() => {}); } },
      { id: 'upload', label: 'Upload', icon: '↑', action: () => { this._closeInsertMenu(); this.uploadInput.click(); } },
      { id: 'video', label: 'Video', icon: '▶', action: () => { this._closeInsertMenu(); this.insertVideo(); } },
      { id: 'embed', label: 'Card', icon: '🔗', action: () => { this._closeInsertMenu(); this.insertLinkCard(); } },
      { id: 'hr', label: 'Line', icon: '—', action: () => this.addNewBlockAt(this.insertState.referenceBlock, document.createElement('hr'), false) },
    ];
    this.insertMenu.innerHTML = '';
    items.forEach(it => {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.title = it.label;
      btn.textContent = it.icon;
      btn.addEventListener('click', (e) => { e.preventDefault(); e.stopPropagation(); it.action(); });
      this.insertMenu.appendChild(btn);
    });
  }

  _openInsertMenu(block, anchor) {
    this.insertState = { open: true, referenceBlock: block };
    this._renderInsertMenu();
    this.insertMenu.classList.add('open');
    const rect = anchor.getBoundingClientRect();
    const menuWidth = this.insertMenu.offsetWidth || 280;
    let left = rect.right + 8;
    if (left + menuWidth > window.innerWidth - 8) {
      left = Math.max(8, window.innerWidth - menuWidth - 8);
    }
    this.insertMenu.style.top = `${rect.top}px`;
    this.insertMenu.style.left = `${left}px`;
  }

  _closeInsertMenu() {
    this.insertState.open = false;
    this.insertMenu.classList.remove('open');
  }

  _positionBlockPlus(block) {
    if (!block || this.isVoidBlock(this.getBlockType(block))) {
      this._hideBlockPlus();
      return;
    }
    this.activeBlockForPlus = block;
    const bodyRect = this.bodyEl.getBoundingClientRect();
    const blockRect = block.getBoundingClientRect();
    const toggleSize = this.blockPlus.offsetWidth || 32;
    const gapFromBody = 12;
    let left = bodyRect.right + gapFromBody;
    left = Math.min(left, window.innerWidth - toggleSize - gapFromBody);
    this.blockPlus.style.top = `${Math.max(8, blockRect.top + Math.max(0, (blockRect.height - toggleSize) / 2))}px`;
    this.blockPlus.style.left = `${Math.round(left)}px`;
    this.blockPlus.style.right = 'auto';
    this.blockPlus.classList.add('visible');
  }

  _hideBlockPlus() {
    this.blockPlus.classList.remove('visible');
    this.activeBlockForPlus = null;
  }

  // ---------- Code blocks (Prism.js) ----------
  _codeBlockToolbarHtml() {
    const langOpts = this.CODE_LANGUAGES.map(([value, label]) => `<option value="${DraftEditor.escHtml(value)}">${DraftEditor.escHtml(label)}</option>`).join('');
    const syntaxOpts = this.CODE_SYNTAX_THEMES.map(([value, label]) => `<option value="${DraftEditor.escHtml(value)}">${DraftEditor.escHtml(label)}</option>`).join('');
    return `<div class="de-code-block-actions" contenteditable="false"><select class="de-code-lang-select" title="Language">${langOpts}</select><select class="de-code-syntax-theme-select" title="Syntax theme">${syntaxOpts}</select><button class="de-code-bg-toggle" type="button" title="Toggle background">◐</button></div>`;
  }

  _syncCodeBlockToolbar(pre) {
    const lang = pre.getAttribute('data-lang') || 'text';
    const syntaxTheme = pre.getAttribute('data-syntax-theme') || 'default';
    const bg = pre.getAttribute('data-bg') || 'light';
    const langSelect = pre.querySelector('.de-code-lang-select');
    const syntaxSelect = pre.querySelector('.de-code-syntax-theme-select');
    const bgToggle = pre.querySelector('.de-code-bg-toggle');
    if (langSelect) {
      const known = this.CODE_LANGUAGES.some(([v]) => v === lang);
      langSelect.value = known ? lang : '__other__';
    }
    if (syntaxSelect) syntaxSelect.value = syntaxTheme;
    if (bgToggle) bgToggle.textContent = bg === 'dark' ? '◑' : '◐';
  }

  _bindCodeBlockToolbar(pre) {
    const langSelect = pre.querySelector('.de-code-lang-select');
    const syntaxSelect = pre.querySelector('.de-code-syntax-theme-select');
    const bgToggle = pre.querySelector('.de-code-bg-toggle');
    const actions = pre.querySelector('.de-code-block-actions');
    if (langSelect) {
      langSelect.addEventListener('change', (e) => {
        e.stopPropagation();
        let lang = e.target.value;
        if (lang === '__other__') {
          lang = window.prompt('Language', pre.getAttribute('data-lang') || 'text');
        }
        this._setCodeBlockLang(pre, (lang || 'text').trim().toLowerCase());
      });
    }
    if (syntaxSelect) {
      syntaxSelect.addEventListener('change', (e) => {
        e.stopPropagation();
        this._setCodeBlockSyntaxTheme(pre, e.target.value);
        e.target.focus();
      });
    }
    if (bgToggle) {
      bgToggle.addEventListener('click', (e) => {
        e.preventDefault(); e.stopPropagation();
        this._toggleCodeBlockBg(pre);
      });
    }
    if (actions) {
      actions.addEventListener('mousedown', (e) => {
        e.stopPropagation();
      });
    }
  }

  _bindCodeBlockInput(pre) {
    const code = pre.querySelector('code');
    if (!code) return;
    let debounceTimer = null;
    code.addEventListener('input', () => {
      pre.dataset.raw = (code.textContent || '').replace(/\u200b/g, '');
      this._markDirty();
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(() => {
        this._highlightCodeBlock(pre);
      }, 400);
    });
    code.addEventListener('keydown', (e) => {
      const sel = window.getSelection();
      if (!sel.rangeCount) return;
      const range = sel.getRangeAt(0);
      if (e.key === 'ArrowDown' && this._isAtEndOfNode(code, range)) {
        e.preventDefault();
        const next = pre.nextElementSibling;
        if (next && this.isBlock(next)) {
          this.placeCaretAtStart(next);
          this._positionBlockPlus(next);
        } else {
          const p = this.createParagraph();
          this.bodyEl.appendChild(p);
          this.placeCaretAtEnd(p);
          this._positionBlockPlus(p);
        }
        return;
      }
      if (e.key === 'ArrowUp' && this._isAtStartOfNode(code, range)) {
        e.preventDefault();
        const prev = pre.previousElementSibling;
        if (prev && this.isBlock(prev)) {
          this.placeCaretAtEnd(prev);
          this._positionBlockPlus(prev);
        } else if (this.titleEl) {
          this.titleEl.focus();
        }
      }
    });
  }

  _highlightCodeBlock(pre) {
    const code = pre.querySelector('code');
    if (!code || !window.Prism) return;
    const sel = window.getSelection();
    const active = document.activeElement === code;
    let offset = 0;
    if (active && sel.rangeCount) {
      const r = document.createRange();
      r.selectNodeContents(code);
      r.setEnd(sel.getRangeAt(0).startContainer, sel.getRangeAt(0).startOffset);
      offset = r.toString().length;
    }
    const lang = (pre.getAttribute('data-lang') || 'text').trim().toLowerCase();
    const raw = pre.dataset.raw || '\n';
    code.className = 'language-' + DraftEditor.escHtml(lang);

    const doHighlight = () => {
      const grammar = Prism.languages[lang] || Prism.languages.text || {};
      code.innerHTML = Prism.highlight(raw, grammar, lang);
      if (active) {
        this._restoreTextOffset(code, offset);
        code.focus();
      }
    };

    if (lang === 'text' || Prism.languages[lang]) {
      doHighlight();
    } else if (Prism.plugins && Prism.plugins.autoloader && Prism.plugins.autoloader.loadLanguages) {
      Prism.plugins.autoloader.loadLanguages(lang, doHighlight);
    } else {
      doHighlight();
    }
  }

  _restoreTextOffset(root, targetOffset) {
    const sel = window.getSelection();
    const range = document.createRange();
    let remaining = targetOffset;
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    let node;
    while ((node = walker.nextNode())) {
      const len = node.textContent.length;
      if (remaining <= len) {
        range.setStart(node, Math.min(remaining, len));
        range.collapse(true);
        sel.removeAllRanges();
        sel.addRange(range);
        return;
      }
      remaining -= len;
    }
    range.selectNodeContents(root);
    range.collapse(false);
    sel.removeAllRanges();
    sel.addRange(range);
  }

  _setCodeBlockSyntaxTheme(pre, syntaxTheme) {
    const oldSyntaxTheme = pre.getAttribute('data-syntax-theme') || 'default';
    pre.classList.remove('syntax-theme-' + oldSyntaxTheme);
    syntaxTheme = (syntaxTheme || 'default').trim().toLowerCase();
    if (!this.CODE_SYNTAX_THEMES.some(([v]) => v === syntaxTheme)) syntaxTheme = 'default';
    pre.setAttribute('data-syntax-theme', syntaxTheme);
    pre.classList.add('syntax-theme-' + syntaxTheme);
    this._syncCodeBlockToolbar(pre);
    this._highlightCodeBlock(pre);
    this._markDirty();
  }

  _toggleCodeBlockBg(pre) {
    const current = pre.getAttribute('data-bg') || 'light';
    const next = current === 'dark' ? 'light' : 'dark';
    pre.setAttribute('data-bg', next);
    this._syncCodeBlockToolbar(pre);
    this._markDirty();
  }

  _setCodeBlockLang(pre, lang) {
    const oldLang = pre.getAttribute('data-lang') || 'text';
    pre.classList.remove('language-' + oldLang);
    lang = (lang || 'text').trim().toLowerCase();
    pre.setAttribute('data-lang', lang);
    pre.classList.add('language-' + lang);
    const code = pre.querySelector('code');
    if (code) code.className = 'language-' + DraftEditor.escHtml(lang);
    this._syncCodeBlockToolbar(pre);
    this._highlightCodeBlock(pre);
    this._markDirty();
  }

  insertCodeBlockAfter(reference, language = 'text', code = '', bg = 'light', syntaxTheme = 'default') {
    const pre = document.createElement('pre');
    pre.className = 'code-block';
    const lang = (language || 'text').trim().toLowerCase();
    const b = (bg || 'light').trim().toLowerCase();
    const st = (syntaxTheme || 'default').trim().toLowerCase();
    pre.setAttribute('data-lang', lang);
    pre.setAttribute('data-bg', b);
    pre.setAttribute('data-syntax-theme', st);
    pre.dataset.raw = code;
    pre.classList.add('syntax-theme-' + st, 'language-' + lang);
    pre.setAttribute('spellcheck', 'false');
    pre.innerHTML = this._codeBlockToolbarHtml() + '<code spellcheck="false" class="language-' + DraftEditor.escHtml(lang) + '" contenteditable="true">' + DraftEditor.escHtml(code || '\n') + '</code>';
    this._bindCodeBlockToolbar(pre);
    this._bindCodeBlockInput(pre);
    this.addNewBlockAt(reference, pre, false);
    const p = this.createParagraph();
    this.addNewBlockAt(pre, p, true);
    this._highlightCodeBlock(pre);
  }

  _ensureCodeBlockActions(pre) {
    if (!pre || pre.querySelector('.de-code-block-actions')) return;
    const code = pre.querySelector('code');
    let raw = pre.dataset.raw;
    if (raw === undefined) {
      raw = code ? (code.textContent || '') : (pre.textContent || '');
      raw = raw.replace(/\u200b/g, '');
    }
    const lang = (pre.getAttribute('data-lang') || 'text').trim().toLowerCase();
    let bg = pre.getAttribute('data-bg');
    let syntaxTheme = pre.getAttribute('data-syntax-theme');
    if (!bg || !syntaxTheme) {
      const oldTheme = (pre.getAttribute('data-theme') || 'light').trim().toLowerCase();
      if (!bg) bg = oldTheme === 'light' ? 'light' : 'dark';
      if (!syntaxTheme) syntaxTheme = oldTheme === 'monokai' ? 'monokai' : (oldTheme === 'light' ? 'default' : 'dark');
    }
    pre.setAttribute('data-lang', lang);
    pre.setAttribute('data-bg', bg);
    pre.setAttribute('data-syntax-theme', syntaxTheme);
    pre.removeAttribute('data-mode');
    pre.dataset.raw = raw;
    pre.classList.add('syntax-theme-' + syntaxTheme, 'language-' + lang);
    pre.setAttribute('spellcheck', 'false');
    pre.innerHTML = this._codeBlockToolbarHtml() + '<code spellcheck="false" class="language-' + DraftEditor.escHtml(lang) + '" contenteditable="true">' + DraftEditor.escHtml(raw || '\n') + '</code>';
    this._bindCodeBlockToolbar(pre);
    this._bindCodeBlockInput(pre);
    this._highlightCodeBlock(pre);
  }

  insertCodeBlockPrompt() {
    this.askInput({ title: 'Code Language', label: 'Language', placeholder: 'python, sh, js', value: 'text', okText: 'Insert' })
      .then((lang) => {
        lang = (lang || 'text').trim() || 'text';
        const block = this.getCurrentBlock();
        if (block && this.isTextBlock(this.getBlockType(block))) {
          block.innerHTML = '<br>';
          this.insertCodeBlockAfter(block, lang, `# write ${lang} code here`, 'light', 'default');
        } else {
          this.insertCodeBlockAfter(block, lang, `# write ${lang} code here`, 'light', 'default');
        }
      });
  }

  insertDivider() {
    const block = this.getCurrentBlock();
    const hr = document.createElement('hr');
    this.addNewBlockAt(block, hr, false);
    const p = this.createParagraph();
    this.addNewBlockAt(hr, p, true);
  }

  insertSourceBlockPrompt() {
    this.askInput({ title: 'Source Block Language', label: 'Language', placeholder: 'python, ruby, emacs-lisp', value: 'text', okText: 'Insert' })
      .then((lang) => {
        lang = (lang || 'text').trim() || 'text';
        const block = this.getCurrentBlock();
        if (block && this.isTextBlock(this.getBlockType(block))) block.innerHTML = '<br>';
        this.insertCodeBlockAfter(block, lang, `# write ${lang} code here`, 'light', 'default');
      });
  }

  insertDitaaDiagram() {
    const code = `+--------+   +--------+
|  Node  |-->|  Node  |
+--------+   +--------+`;
    const block = this.getCurrentBlock();
    if (block && this.isTextBlock(this.getBlockType(block))) block.innerHTML = '<br>';
    this.insertCodeBlockAfter(block, 'ditaa', code, 'light', 'default');
  }

  insertPlantumlDiagram() {
    const code = `@startuml
Alice -> Bob: Hello
Bob --> Alice: Hi
@enduml`;
    const block = this.getCurrentBlock();
    if (block && this.isTextBlock(this.getBlockType(block))) block.innerHTML = '<br>';
    this.insertCodeBlockAfter(block, 'plantuml', code, 'light', 'default');
  }

  insertAsciiDiagram() {
    const code = `    /\\
    /  \\
   /----\\
  /      \\
/________\\`;
    const block = this.getCurrentBlock();
    if (block && this.isTextBlock(this.getBlockType(block))) block.innerHTML = '<br>';
    this.insertCodeBlockAfter(block, 'text', code, 'light', 'default');
  }

  insertTable() {
    const figure = document.createElement('figure');
    figure.className = 'table-block';
    const table = document.createElement('table');
    const tbody = document.createElement('tbody');
    for (let r = 0; r < 3; r++) {
      const tr = document.createElement('tr');
      for (let c = 0; c < 3; c++) {
        const td = document.createElement('td');
        td.innerHTML = '<br>';
        td.contentEditable = 'true';
        tr.appendChild(td);
      }
      tbody.appendChild(tr);
    }
    table.appendChild(tbody);
    figure.appendChild(table);
    const block = this.getCurrentBlock();
    this.addNewBlockAt(block, figure, false);
    const p = this.createParagraph();
    this.addNewBlockAt(figure, p, true);
    this._ensureTableToolbar();
  }

  insertEquation() {
    this.askInput({ title: 'Equation', label: 'LaTeX', placeholder: 'E = mc^2', value: '', okText: 'Insert' })
      .then((latex) => {
        if (latex === null) return;
        latex = latex.trim();
        if (!latex) return;
        const block = this.getCurrentBlock();
        const isBlock = latex.includes('\\') || latex.includes('\n') || (block && this.isTextBlock(this.getBlockType(block)) && (block.textContent || '').trim() === '');
        if (isBlock) {
          if (block && this.isTextBlock(this.getBlockType(block))) block.innerHTML = '<br>';
          const div = document.createElement('div');
          div.className = 'math';
          div.dataset.latex = latex;
          div.textContent = latex;
          this.addNewBlockAt(block, div, false);
          const p = this.createParagraph();
          this.addNewBlockAt(div, p, true);
        } else {
          this.restoreSelection();
          const span = document.createElement('span');
          span.className = 'math';
          span.dataset.latex = latex;
          span.textContent = latex;
          this.insertHtmlAtCursor(span.outerHTML);
        }
      });
  }

  _ensureTableToolbar() {
    if (this.tableToolbar) return;
    const tb = document.createElement('div');
    tb.className = 'de-table-tools';
    const makeBtn = (action, label, title) => {
      const b = document.createElement('button');
      b.type = 'button'; b.dataset.action = action; b.title = title; b.textContent = label;
      return b;
    };
    tb.appendChild(makeBtn('add-row', '+R', 'Add row'));
    tb.appendChild(makeBtn('add-col', '+C', 'Add column'));
    tb.appendChild(makeBtn('remove-row', '-R', 'Remove row'));
    tb.appendChild(makeBtn('remove-col', '-C', 'Remove column'));
    document.body.appendChild(tb);
    tb.addEventListener('mousedown', (e) => e.preventDefault());
    tb.addEventListener('click', (e) => {
      const btn = e.target.closest('button');
      if (!btn) return;
      const table = this.bodyEl.querySelector(':scope > .is-selected table, :scope > .is-selected.table-block table');
      if (!table) return;
      const action = btn.dataset.action;
      if (action === 'add-row') {
        const tr = document.createElement('tr');
        const cols = table.querySelector('tr') ? table.querySelector('tr').children.length : 3;
        for (let i = 0; i < cols; i++) { const td = document.createElement('td'); td.innerHTML = '<br>'; td.contentEditable = 'true'; tr.appendChild(td); }
        table.querySelector('tbody') ? table.querySelector('tbody').appendChild(tr) : table.appendChild(tr);
      } else if (action === 'add-col') {
        table.querySelectorAll('tr').forEach(tr => { const td = document.createElement('td'); td.innerHTML = '<br>'; td.contentEditable = 'true'; tr.appendChild(td); });
      } else if (action === 'remove-row') {
        const rows = table.querySelectorAll('tr');
        if (rows.length > 1) rows[rows.length - 1].remove();
      } else if (action === 'remove-col') {
        table.querySelectorAll('tr').forEach(tr => { if (tr.children.length > 1) tr.lastElementChild.remove(); });
      }
      this._markDirty();
      this._positionTableToolbar(table.closest('figure.table-block') || table);
    });
    this.tableToolbar = tb;
  }

  _positionTableToolbar(el) {
    if (!this.tableToolbar || !el) return;
    const rect = el.getBoundingClientRect();
    this.tableToolbar.style.top = `${Math.max(8, rect.top - this.tableToolbar.offsetHeight - 10)}px`;
    this.tableToolbar.style.left = `${Math.max(8, Math.min(rect.left + rect.width / 2 - this.tableToolbar.offsetWidth / 2, window.innerWidth - this.tableToolbar.offsetWidth - 8))}px`;
  }

  insertImageFromUrl(url, caption) {
    const src = DraftEditor.escHtml(url);
    const rawCap = DraftEditor.escHtml(caption || '');
    const cap = DraftEditor.isPlaceholderCaption(rawCap, src) ? '' : rawCap;
    const fig = document.createElement('figure');
    fig.className = 'image-block fit-cover pos-center align-center';
    fig.style.width = '760px';
    fig.style.maxWidth = '100%';
    fig.style.height = '420px';
    fig.dataset.align = 'center';
    fig.dataset.filter = 'none';
    fig.dataset.fit = 'cover';
    fig.dataset.position = 'center';
    fig.innerHTML = `<img src="${src}" alt="${cap || 'image'}">${cap ? `<figcaption contenteditable="true">${cap}</figcaption>` : ''}`;
    const block = this.getCurrentBlock();
    this.addNewBlockAt(block, fig, false);
    const p = this.createParagraph();
    this.addNewBlockAt(fig, p, true);
    this._addImageResizeHandle(fig);
    this._observeImageSize(fig);
  }

  _addImageResizeHandle(fig) {
    if (!fig || fig.querySelector('.de-image-resize-handle')) return;
    const handle = document.createElement('div');
    handle.className = 'de-image-resize-handle';
    handle.setAttribute('contenteditable', 'false');
    fig.appendChild(handle);
    let startX, startY, startW, startH;
    const onMove = (ev) => {
      const dx = ev.clientX - startX;
      const dy = ev.clientY - startY;
      const bodyW = this.bodyEl.getBoundingClientRect().width;
      const newW = Math.max(160, Math.min(startW + dx, bodyW - 40));
      const newH = Math.max(120, startH + dy);
      fig.style.width = newW + 'px';
      fig.style.maxWidth = '100%';
      fig.style.height = newH + 'px';
      this._positionBlockToolbar(fig);
      this._positionImageToolbar(fig);
    };
    const onUp = () => {
      document.removeEventListener('mousemove', onMove);
      document.removeEventListener('mouseup', onUp);
      fig.dataset.width = parseInt(fig.style.width, 10) + '';
      fig.dataset.height = parseInt(fig.style.height, 10) + '';
      this._markDirty();
    };
    handle.addEventListener('mousedown', (ev) => {
      ev.preventDefault(); ev.stopPropagation();
      startX = ev.clientX;
      startY = ev.clientY;
      startW = fig.getBoundingClientRect().width;
      startH = fig.getBoundingClientRect().height;
      document.addEventListener('mousemove', onMove);
      document.addEventListener('mouseup', onUp);
    });
  }

  _observeImageSize(fig) {
    if (!fig || fig.dataset.resizeObserved || !('ResizeObserver' in window)) return;
    const ro = new ResizeObserver((entries) => {
      for (const entry of entries) {
        const w = Math.round(entry.contentRect.width);
        const h = Math.round(entry.contentRect.height);
        if (w) { fig.dataset.width = w + ''; fig.style.width = w + 'px'; fig.style.maxWidth = '100%'; }
        if (h) { fig.dataset.height = h + ''; fig.style.height = h + 'px'; }
      }
    });
    ro.observe(fig);
    fig.dataset.resizeObserved = '1';
  }

  _createBlockToolbar() {
    const tb = document.createElement('div');
    tb.className = 'de-block-tools';
    const makeBtn = (action, label, title) => {
      const b = document.createElement('button');
      b.type = 'button'; b.dataset.action = action; b.title = title; b.textContent = label;
      return b;
    };
    tb.appendChild(makeBtn('move-up', '↑', 'Move up'));
    tb.appendChild(makeBtn('move-down', '↓', 'Move down'));
    tb.appendChild(makeBtn('delete', '×', 'Delete'));
    document.body.appendChild(tb);
    tb.addEventListener('mousedown', (e) => e.preventDefault());
    tb.addEventListener('click', (e) => {
      const btn = e.target.closest('button');
      if (!btn) return;
      const action = btn.dataset.action;
      const block = this.bodyEl.querySelector(':scope > .is-selected');
      if (!block) return;
      if (action === 'move-up') {
        const prev = block.previousElementSibling;
        if (prev) { this.bodyEl.insertBefore(block, prev); this._positionBlockToolbar(block); this._markDirty(); }
      } else if (action === 'move-down') {
        const next = block.nextElementSibling;
        if (next && next.nextElementSibling) this.bodyEl.insertBefore(block, next.nextElementSibling);
        else if (next) this.bodyEl.appendChild(block);
        this._positionBlockToolbar(block); this._markDirty();
      } else if (action === 'delete') {
        const adj = block.nextElementSibling || block.previousElementSibling;
        block.remove(); this._hideBlockToolbar(); this._markDirty();
        if (adj) this.placeCaretAtEnd(adj);
        else this.ensureTrailingParagraph();
      }
    });
    return tb;
  }

  _applyBlockSetting(block, type, value) {
    this._markDirty();
    this._syncBlockToolbar(block);
    this._positionBlockToolbar(block);
  }

  _syncBlockToolbar(block) {
    if (!this.blockToolbar) return;
  }

  _selectBlock(block) {
    this._deselectBlocks();
    this._hideMultiToolbar();
    block.classList.add('is-selected');
    if (block.classList.contains('image-block')) {
      if (!this.imageToolbar) this.imageToolbar = this._createImageToolbar();
      this.imageToolbar.classList.add('open');
      this._syncImageToolbar(block);
      this._positionImageToolbar(block);
      if (this.blockToolbar) this.blockToolbar.classList.remove('open');
      if (this.tableToolbar) this.tableToolbar.classList.remove('open');
    } else if (block.classList.contains('table-block')) {
      this._ensureTableToolbar();
      this.tableToolbar.classList.add('open');
      this._positionTableToolbar(block);
      if (this.blockToolbar) this.blockToolbar.classList.remove('open');
      if (this.imageToolbar) this.imageToolbar.classList.remove('open');
    } else {
      if (!this.blockToolbar) this.blockToolbar = this._createBlockToolbar();
      this.blockToolbar.classList.add('open');
      this._syncBlockToolbar(block);
      this._positionBlockToolbar(block);
      if (this.imageToolbar) this.imageToolbar.classList.remove('open');
      if (this.tableToolbar) this.tableToolbar.classList.remove('open');
    }
  }

  _deselectBlocks() {
    this.bodyEl.querySelectorAll(':scope > .is-selected').forEach(b => b.classList.remove('is-selected'));
    this._hideBlockToolbar();
    this._hideImageToolbar();
    this._hideMultiToolbar();
    if (this.tableToolbar) this.tableToolbar.classList.remove('open');
  }

  _hideBlockToolbar() { if (this.blockToolbar) this.blockToolbar.classList.remove('open'); }

  _positionBlockToolbar(block) {
    if (!this.blockToolbar) return;
    const rect = block.getBoundingClientRect();
    this.blockToolbar.style.top = `${Math.max(8, rect.top - this.blockToolbar.offsetHeight - 10)}px`;
    this.blockToolbar.style.left = `${Math.max(8, Math.min(rect.left + rect.width / 2 - this.blockToolbar.offsetWidth / 2, window.innerWidth - this.blockToolbar.offsetWidth - 8))}px`;
  }

  _createImageToolbar() {
    const tb = document.createElement('div');
    tb.className = 'de-image-tools';
    const makeBtn = (action, label, title) => {
      const b = document.createElement('button');
      b.type = 'button'; b.dataset.action = action; b.title = title; b.textContent = label;
      return b;
    };
    const makeSelect = (name, options, title) => {
      const s = document.createElement('select');
      s.dataset.action = name; s.title = title;
      options.forEach(([value, label]) => {
        const opt = document.createElement('option');
        opt.value = value; opt.textContent = label;
        s.appendChild(opt);
      });
      return s;
    };
    const sep = () => {
      const d = document.createElement('span');
      d.className = 'de-sep';
      return d;
    };
    tb.appendChild(makeBtn('align-left', 'L', 'Align left'));
    tb.appendChild(makeBtn('align-center', 'C', 'Align center'));
    tb.appendChild(makeBtn('align-right', 'R', 'Align right'));
    tb.appendChild(sep());
    tb.appendChild(makeSelect('fit', [
      ['cover', 'Cover'], ['contain', 'Contain'], ['fill', 'Fill'], ['scale-down', 'Scale down'], ['none', 'None']
    ], 'Object fit'));
    tb.appendChild(makeSelect('position', [
      ['center', 'Center'], ['top', 'Top'], ['top-right', 'Top right'], ['right', 'Right'],
      ['bottom-right', 'Bottom right'], ['bottom', 'Bottom'], ['bottom-left', 'Bottom left'],
      ['left', 'Left'], ['top-left', 'Top left']
    ], 'Object position'));
    tb.appendChild(sep());
    tb.appendChild(makeSelect('filter', [
      ['none', 'Filter'], ['none', 'None'], ['grayscale', 'Grayscale'], ['sepia', 'Sepia'], ['invert', 'Invert'],
      ['brightness', 'Brightness'], ['contrast', 'Contrast'], ['saturate', 'Saturate'],
      ['hue-90', 'Hue 90°'], ['hue-180', 'Hue 180°'], ['hue-270', 'Hue 270°'],
      ['blur', 'Blur'], ['opacity', 'Opacity'], ['drop-shadow', 'Shadow'],
      ['warm', 'Warm'], ['cool', 'Cool'], ['vintage', 'Vintage'], ['blackwhite', 'B&W']
    ], 'Filter'));
    tb.appendChild(sep());
    tb.appendChild(makeBtn('replace-url', 'Link', 'Replace image URL'));
    tb.appendChild(makeBtn('replace-upload', 'Upload', 'Replace image by upload'));
    document.body.appendChild(tb);
    tb.addEventListener('mousedown', (e) => e.preventDefault());
    tb.addEventListener('click', (e) => {
      const btn = e.target.closest('button');
      if (!btn) return;
      const fig = this.bodyEl.querySelector(':scope > .is-selected.image-block');
      if (!fig) return;
      const action = btn.dataset.action;
      if (action === 'replace-url') {
        const img = fig.querySelector('img');
        const current = img ? (img.getAttribute('src') || '') : '';
        const url = window.prompt('Replace image URL', current);
        if (url && img) {
          img.setAttribute('src', url);
          img.setAttribute('alt', url.split('/').pop().split('?')[0] || 'image');
          this._markDirty();
        }
        return;
      }
      if (action === 'replace-upload') {
        this._replaceImageUpload(fig);
        return;
      }
      this._applyImageSetting(fig, 'align', action.replace('align-', ''));
    });
    tb.addEventListener('change', (e) => {
      const sel = e.target.closest('select');
      if (!sel) return;
      const fig = this.bodyEl.querySelector(':scope > .is-selected.image-block');
      if (!fig) return;
      const type = sel.dataset.action;
      this._applyImageSetting(fig, type, sel.value);
    });
    return tb;
  }

  _applyImageSetting(fig, type, value) {
    if (type === 'align') {
      fig.classList.remove('align-left', 'align-center', 'align-right');
      fig.classList.add(`align-${value}`);
      fig.dataset.align = value;
    } else if (type === 'fit') {
      fig.classList.remove('fit-cover', 'fit-contain', 'fit-fill', 'fit-scale-down', 'fit-none');
      fig.classList.add(`fit-${value}`);
      fig.dataset.fit = value;
    } else if (type === 'position') {
      fig.classList.remove('pos-top', 'pos-top-right', 'pos-right', 'pos-bottom-right', 'pos-bottom', 'pos-bottom-left', 'pos-left', 'pos-top-left', 'pos-center');
      fig.classList.add(`pos-${value}`);
      fig.dataset.position = value;
    } else if (type === 'filter') {
      fig.classList.forEach((cls) => { if (cls.startsWith('filter-')) fig.classList.remove(cls); });
      if (value && value !== 'none') fig.classList.add(`filter-${value}`);
      fig.dataset.filter = value || 'none';
    }
    this._markDirty();
    this._positionImageToolbar(fig);
  }

  _syncImageToolbar(fig) {
    if (!this.imageToolbar) return;
    const align = fig.dataset.align || 'center';
    const fit = fig.dataset.fit || 'cover';
    const position = fig.dataset.position || 'center';
    const filter = fig.dataset.filter || 'none';
    this.imageToolbar.querySelectorAll('button[data-action^="align-"]').forEach(b => {
      b.style.background = b.dataset.action === `align-${align}` ? 'rgba(13,106,87,0.5)' : '';
    });
    const fitSelect = this.imageToolbar.querySelector('select[data-action="fit"]');
    if (fitSelect) fitSelect.value = fit;
    const posSelect = this.imageToolbar.querySelector('select[data-action="position"]');
    if (posSelect) posSelect.value = position;
    this.imageToolbar.querySelector('select[data-action="filter"]').value = filter;
  }

  _positionImageToolbar(block) {
    if (!this.imageToolbar) return;
    const rect = block.getBoundingClientRect();
    this.imageToolbar.style.top = `${Math.max(8, rect.top - this.imageToolbar.offsetHeight - 10)}px`;
    this.imageToolbar.style.left = `${Math.max(8, Math.min(rect.left + rect.width / 2 - this.imageToolbar.offsetWidth / 2, window.innerWidth - this.imageToolbar.offsetWidth - 8))}px`;
  }

  _hideImageToolbar() { if (this.imageToolbar) this.imageToolbar.classList.remove('open'); }

  _getSelectedBlocks() {
    return Array.from(this.bodyEl.querySelectorAll(':scope > .is-selected'));
  }

  _hideMultiToolbar() { if (this.multiToolbar) this.multiToolbar.classList.remove('open'); }

  _createMultiToolbar() {
    const tb = document.createElement('div');
    tb.className = 'de-multi-tools';
    const makeBtn = (action, label, title) => {
      const b = document.createElement('button');
      b.type = 'button'; b.dataset.action = action; b.title = title; b.textContent = label;
      return b;
    };
    tb.appendChild(makeBtn('move-up', '↑', 'Move up'));
    tb.appendChild(makeBtn('move-down', '↓', 'Move down'));
    tb.appendChild(makeBtn('delete', '×', 'Delete'));
    document.body.appendChild(tb);
    tb.addEventListener('mousedown', (e) => e.preventDefault());
    tb.addEventListener('click', (e) => {
      const btn = e.target.closest('button');
      if (!btn) return;
      const selected = this._getSelectedBlocks();
      if (!selected.length) return;
      const action = btn.dataset.action;
      if (action === 'move-up') {
        const first = selected[0];
        const prev = first.previousElementSibling;
        if (prev) selected.forEach(b => this.bodyEl.insertBefore(b, prev));
        this._markDirty();
      } else if (action === 'move-down') {
        const last = selected[selected.length - 1];
        let ref = last.nextElementSibling;
        if (ref) {
          ref = ref.nextElementSibling;
          selected.forEach(b => {
            if (ref) this.bodyEl.insertBefore(b, ref);
            else this.bodyEl.appendChild(b);
          });
        } else {
          selected.forEach(b => this.bodyEl.appendChild(b));
        }
        this._markDirty();
      } else if (action === 'delete') {
        const adj = selected[selected.length - 1].nextElementSibling || selected[0].previousElementSibling;
        selected.forEach(b => b.remove());
        this._hideMultiToolbar();
        this._markDirty();
        if (adj && this.isBlock(adj)) this.placeCaretAtEnd(adj);
        else this.ensureTrailingParagraph();
      }
      this._updateMultiToolbar();
    });
    return tb;
  }

  _updateMultiToolbar() {
    const selected = this._getSelectedBlocks();
    if (selected.length > 1) {
      this._hideBlockToolbar();
      this._hideImageToolbar();
      if (!this.multiToolbar) this.multiToolbar = this._createMultiToolbar();
      this.multiToolbar.classList.add('open');
      this._positionMultiToolbar(selected[0]);
    } else {
      this._hideMultiToolbar();
    }
  }

  _positionMultiToolbar(block) {
    if (!this.multiToolbar || !block) return;
    const rect = block.getBoundingClientRect();
    this.multiToolbar.style.top = `${Math.max(8, rect.top - this.multiToolbar.offsetHeight - 10)}px`;
    this.multiToolbar.style.left = `${Math.max(8, Math.min(rect.left + rect.width / 2 - this.multiToolbar.offsetWidth / 2, window.innerWidth - this.multiToolbar.offsetWidth - 8))}px`;
  }

  _toggleMultiSelectBlock(block) {
    if (!block || !this.isBlock(block)) return;
    block.classList.toggle('is-selected');
    this._hideBlockToolbar();
    this._hideImageToolbar();
    this._updateMultiToolbar();
  }

  _syncAndShowImageToolbar(fig) {
    if (!this.imageToolbar) this.imageToolbar = this._createImageToolbar();
    this.imageToolbar.classList.add('open');
    this._syncImageToolbar(fig);
    this._positionImageToolbar(fig);
  }

  async _replaceImageUpload(fig) {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = 'image/*';
    input.hidden = true;
    this.bodyEl.parentElement.appendChild(input);
    input.addEventListener('change', async () => {
      const file = input.files && input.files[0];
      if (!file) { input.remove(); return; }
      try {
        const url = await this.uploadImage(file);
        const img = fig.querySelector('img');
        if (img) {
          img.setAttribute('src', url);
          img.setAttribute('alt', file.name || 'image');
          this._markDirty();
        }
      } catch (_) {}
      input.value = '';
      input.remove();
    });
    input.click();
  }

  async promptImageUrl() {
    const url = await this.askInput({ title: 'Insert Image', label: 'Image URL', placeholder: 'https://.../image.png', okText: 'Insert' });
    if (!url) return;
    const caption = (await this.askInput({ title: 'Image Caption', label: 'Caption (optional)', placeholder: 'Caption', okText: 'Apply' })) || '';
    this.insertImageFromUrl(url, caption);
  }

  async uploadImage(file) {
    const reader = new FileReader();
    const dataUrl = await new Promise((resolve, reject) => {
      reader.onload = () => resolve(reader.result);
      reader.onerror = reject;
      reader.readAsDataURL(file);
    });
    const res = await fetch(this.opts.uploadEndpoint, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ filename: file.name, dataUrl }),
    });
    const data = await res.json();
    if (!res.ok || !data.ok) throw new Error(data.error || 'Upload failed');
    return data.url;
  }

  async insertVideo() {
    const raw = await this.askInput({ title: 'Insert Video', label: 'Video URL', placeholder: 'https://youtube.com/... or embed URL', okText: 'Insert' });
    if (!raw) return;
    const embed = DraftEditor.escHtml(DraftEditor.toEmbedUrl(raw));
    const fig = document.createElement('figure');
    fig.className = 'video-embed';
    fig.setAttribute('contenteditable', 'false');
    fig.setAttribute('data-src', embed);
    fig.innerHTML = `<iframe src="${embed}" loading="lazy" allow="autoplay; encrypted-media; picture-in-picture" allowfullscreen></iframe>`;
    const block = this.getCurrentBlock();
    this.addNewBlockAt(block, fig, false);
    const p = this.createParagraph();
    this.addNewBlockAt(fig, p, true);
  }

  async insertLinkCard() {
    const raw = await this.askInput({ title: 'Embed Link', label: 'URL', placeholder: 'https://example.com', okText: 'Embed' });
    if (!raw) return;
    try {
      const res = await fetch(this.opts.fetchMetaEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ url: raw }),
      });
      const data = await res.json();
      if (!res.ok || !data.ok) throw new Error(data.error || 'Unable to fetch URL metadata');
      const url = DraftEditor.escHtml(data.url || raw);
      const title = DraftEditor.escHtml(data.title || raw);
      const desc = DraftEditor.escHtml(data.description || '');
      const site = DraftEditor.escHtml(data.site || '');
      const image = data.image ? `<img src="${DraftEditor.escHtml(data.image)}" alt="${title}">` : '';
      const fig = document.createElement('figure');
      fig.className = 'embed-card';
      fig.setAttribute('contenteditable', 'false');
      fig.setAttribute('data-url', url);
      fig.setAttribute('data-title', title);
      fig.setAttribute('data-description', desc);
      fig.setAttribute('data-site', site);
      fig.setAttribute('data-image', DraftEditor.escHtml(data.image || ''));
      fig.innerHTML = `
        <a class="de-embed-card-link" href="${url}" target="_blank" rel="noopener">
          <div class="de-embed-card-copy"><h4>${title}</h4><p>${desc}</p><small>${site}</small></div>
          ${image}
        </a>`;
      const block = this.getCurrentBlock();
      this.addNewBlockAt(block, fig, false);
      const p = this.createParagraph();
      this.addNewBlockAt(fig, p, true);
    } catch (err) {
      this.setStatus(String(err.message || err), true);
    }
  }

  // ---------- Auto-transform ----------
  _checkAutoTransform(block) {
    if (this.slashState.open) return;
    const text = (block.textContent || '').trim();
    const type = this.getBlockType(block);
    if (!this.isTextBlock(type)) return;

    const transforms = [
      { pattern: /^#\s$/, type: 'h1' },
      { pattern: /^##\s$/, type: 'h2' },
      { pattern: /^###\s$/, type: 'h3' },
      { pattern: /^>\s$/, type: 'blockquote' },
      { pattern: /^---$/, type: 'hr' },
    ];
    for (const t of transforms) {
      if (t.pattern.test(text)) {
        block.innerHTML = '<br>';
        if (t.type === 'hr') {
          const hr = document.createElement('hr');
          this.bodyEl.replaceChild(hr, block);
          const p = this.createParagraph();
          this.addNewBlockAt(hr, p, true);
        } else {
          this.transformCurrentBlock(t.type);
        }
        return;
      }
    }
    const codeMatch = text.match(/^```([a-zA-Z0-9_+-]*)$/);
    if (codeMatch) {
      const lang = codeMatch[1].trim() || 'text';
      block.innerHTML = '<br>';
      this.insertCodeBlockAfter(block, lang, lang === 'text' ? '# write code here' : `# write ${lang} code here`, 'light', 'default');
    }
  }

  // ---------- Keyboard handling ----------
  _isAtStartOfBlock(block, range) {
    const type = this.getBlockType(block);
    if (this.isVoidBlock(type)) return true;
    let node = range.startContainer;
    if (node.nodeType === Node.TEXT_NODE) {
      return range.startOffset === 0 && (node === block.firstChild || node.parentElement === block);
    }
    if (node === block) return range.startOffset === 0;
    return false;
  }

  _isAtEndOfBlock(block, range) {
    const type = this.getBlockType(block);
    if (this.isVoidBlock(type)) return true;
    let node = range.endContainer;
    if (node.nodeType === Node.TEXT_NODE) {
      const text = node.textContent || '';
      return range.endOffset === text.length && (node === block.lastChild || node.parentElement === block);
    }
    if (node === block) return range.endOffset >= (block.childNodes.length || 0);
    return false;
  }

  _isAtStartOfNode(root, range) {
    const r = document.createRange();
    r.selectNodeContents(root);
    r.setEnd(range.startContainer, range.startOffset);
    return r.toString().length === 0;
  }

  _isAtEndOfNode(root, range) {
    const r = document.createRange();
    r.selectNodeContents(root);
    r.setStart(range.endContainer, range.endOffset);
    return r.toString().length === 0;
  }

  _handleReturnKey(e) {
    if (this.slashState.open) {
      e.preventDefault();
      const items = this.SLASH_ITEMS.filter(it =>
        it.label.toLowerCase().includes(this.slashState.query) ||
        it.desc.toLowerCase().includes(this.slashState.query)
      );
      if (items[this.slashState.index]) {
        this._closeSlashMenu();
        items[this.slashState.index].action();
      }
      return;
    }

    const block = this.getCurrentBlock();
    if (!block) return;
    const type = this.getBlockType(block);

    if (type === 'pre') {
      setTimeout(() => { this.ensureTrailingParagraph(); this.refreshBlockStates(); this._markDirty(); }, 0);
      return;
    }

    if (type === 'ul' || type === 'ol') {
      const sel = window.getSelection();
      const li = sel.anchorNode && sel.anchorNode.nodeType === Node.TEXT_NODE ? sel.anchorNode.parentElement : sel.anchorNode;
      if (li && li.tagName.toLowerCase() === 'li' && (li.textContent || '').trim() === '') {
        e.preventDefault();
        const list = block;
        if (list.children.length === 1) {
          const p = this.createParagraph();
          this.bodyEl.replaceChild(p, list);
          this.placeCaretAtEnd(p);
        } else {
          const p = this.createParagraph();
          this.addNewBlockAt(list, p, true);
          li.remove();
        }
        this.refreshBlockStates();
        this._markDirty();
      }
      return;
    }

    if (this.isTextBlock(type)) {
      e.preventDefault();
      this.splitTextBlock(block);
      return;
    }

    e.preventDefault();
    this.addNewBlockAt(block, this.createParagraph(), true);
  }

  _handleBackspaceKey(e) {
    if (this.slashState.open) return;
    const block = this.getCurrentBlock();
    if (!block) return;
    const sel = window.getSelection();
    if (!sel.rangeCount) return;
    const range = sel.getRangeAt(0);
    if (range.startOffset === 0 && this._isAtStartOfBlock(block, range)) {
      const text = (block.textContent || '').trim();
      if (text === '' && this.getBlocks().length > 1) {
        e.preventDefault();
        this.removeBlock(block);
      } else if (text !== '' && block.previousElementSibling) {
        e.preventDefault();
        this.mergeWithPrevious(block);
      }
    }
  }

  // Medium-draft style arrow key handling: create new blocks at boundaries
  _handleArrowKeys(e) {
    const block = this.getCurrentBlock();
    if (!block) return;
    const sel = window.getSelection();
    if (!sel.rangeCount) return;
    const range = sel.getRangeAt(0);
    const type = this.getBlockType(block);

    if (e.key === 'ArrowDown') {
      const next = block.nextElementSibling;
      if (type === 'pre') {
        const code = block.querySelector('code');
        if (code && this._isAtEndOfNode(code, range)) {
          e.preventDefault();
          if (next && this.isBlock(next)) {
            this.placeCaretAtStart(next);
            this._positionBlockPlus(next);
          } else {
            const p = this.createParagraph();
            this.bodyEl.appendChild(p);
            this.placeCaretAtEnd(p);
            this._positionBlockPlus(p);
          }
        }
        return;
      }
      if (this.isVoidBlock(type)) {
        e.preventDefault();
        if (next && this.isBlock(next)) {
          this.placeCaretAtStart(next);
          this._positionBlockPlus(next);
        } else {
          const p = this.createParagraph();
          this.bodyEl.appendChild(p);
          this.placeCaretAtEnd(p);
          this._positionBlockPlus(p);
        }
        return;
      }
      if (this._isAtEndOfBlock(block, range)) {
        e.preventDefault();
        if (next && this.isBlock(next)) {
          this.placeCaretAtStart(next);
          this._positionBlockPlus(next);
        } else {
          const p = this.createParagraph();
          this.bodyEl.appendChild(p);
          this.placeCaretAtEnd(p);
          this._positionBlockPlus(p);
        }
      }
    } else if (e.key === 'ArrowUp') {
      const prev = block.previousElementSibling;
      if (type === 'pre') {
        const code = block.querySelector('code');
        if (code && this._isAtStartOfNode(code, range)) {
          e.preventDefault();
          if (prev && this.isBlock(prev)) {
            this.placeCaretAtEnd(prev);
            this._positionBlockPlus(prev);
          } else {
            const p = this.createParagraph();
            this.bodyEl.insertBefore(p, block);
            this.placeCaretAtEnd(p);
            this._positionBlockPlus(p);
          }
        }
        return;
      }
      if (this.isVoidBlock(type)) {
        e.preventDefault();
        if (prev && this.isBlock(prev)) {
          this.placeCaretAtEnd(prev);
          this._positionBlockPlus(prev);
        } else {
          const p = this.createParagraph();
          this.bodyEl.insertBefore(p, block);
          this.placeCaretAtEnd(p);
          this._positionBlockPlus(p);
        }
        return;
      }
      if (this._isAtStartOfBlock(block, range)) {
        e.preventDefault();
        if (prev && this.isBlock(prev)) {
          this.placeCaretAtEnd(prev);
          this._positionBlockPlus(prev);
        } else {
          const p = this.createParagraph();
          this.bodyEl.insertBefore(p, block);
          this.placeCaretAtEnd(p);
          this._positionBlockPlus(p);
        }
      }
    }
  }

  // ---------- Event wiring ----------
  _bindEvents() {
    if (this.titleEl) this.titleEl.addEventListener('input', () => this._markDirty());

    this.bodyEl.addEventListener('focusin', () => {
      this.getBlocks().forEach(b => b.classList.remove('is-focused'));
      const block = this.getCurrentBlock();
      if (block) {
        block.classList.add('is-focused');
        this._positionBlockPlus(block);
      }
    });

    this.bodyEl.addEventListener('focusout', () => {
      setTimeout(() => {
        this.getBlocks().forEach(b => b.classList.remove('is-focused'));
        if (!this.bodyEl.contains(document.activeElement)) this._hideBlockPlus();
      }, 200);
    });

    this.bodyEl.addEventListener('mouseover', (e) => {
      const block = e.target.closest ? e.target.closest('.de-body > *') : null;
      if (block && this.isBlock(block)) this._positionBlockPlus(block);
    });

    this.bodyEl.addEventListener('mouseout', (e) => {
      const related = e.relatedTarget;
      if (related && (related === this.blockPlus || this.insertMenu.contains(related))) return;
      setTimeout(() => {
        if (!this.bodyEl.matches(':hover') && !this.blockPlus.matches(':hover')) this._hideBlockPlus();
      }, 50);
    });

    this.bodyEl.addEventListener('input', (e) => {
      const block = this.getCurrentBlock();
      if (block) {
        this.setEmptyState(block);
        const text = (block.textContent || '').trim();
        if (text.startsWith('/')) {
          if (!this.slashState.open) this._openSlashMenu(block);
          this._handleSlashInput(block, text);
        } else if (this.slashState.open) {
          this._closeSlashMenu();
        }
        this._checkAutoTransform(block);
      }
      this._markDirty();
    });

    this.bodyEl.addEventListener('keydown', (e) => {
      const td = e.target.closest && e.target.closest('figure.table-block td, figure.table-block th');
      if (td && (e.key === 'Tab' || e.key === 'Enter')) {
        e.preventDefault();
        const row = td.parentElement;
        const table = row.closest('table');
        const cells = Array.from(row.children);
        const rowIdx = Array.from(table.querySelectorAll('tr')).indexOf(row);
        const cellIdx = cells.indexOf(td);
        if (e.key === 'Tab') {
          const nextCell = cells[cellIdx + 1];
          if (nextCell) { nextCell.focus(); this._placeCaretInCell(nextCell); }
          else {
            const rows = Array.from(table.querySelectorAll('tr'));
            const nextRow = rows[rowIdx + 1];
            if (nextRow && nextRow.firstElementChild) { nextRow.firstElementChild.focus(); this._placeCaretInCell(nextRow.firstElementChild); }
            else {
              const newRow = document.createElement('tr');
              const cols = cells.length || 3;
              for (let i = 0; i < cols; i++) { const ntd = document.createElement('td'); ntd.innerHTML = '<br>'; ntd.contentEditable = 'true'; newRow.appendChild(ntd); }
              table.querySelector('tbody') ? table.querySelector('tbody').appendChild(newRow) : table.appendChild(newRow);
              newRow.firstElementChild.focus(); this._placeCaretInCell(newRow.firstElementChild);
            }
          }
        } else {
          const newRow = document.createElement('tr');
          const cols = cells.length || 3;
          for (let i = 0; i < cols; i++) { const ntd = document.createElement('td'); ntd.innerHTML = '<br>'; ntd.contentEditable = 'true'; newRow.appendChild(ntd); }
          table.querySelector('tbody') ? table.querySelector('tbody').insertBefore(newRow, row.nextSibling) : table.insertBefore(newRow, row.nextSibling);
          newRow.children[Math.min(cellIdx, cols - 1)].focus(); this._placeCaretInCell(newRow.children[Math.min(cellIdx, cols - 1)]);
        }
        this._markDirty();
        return;
      }
      const mod = e.metaKey || e.ctrlKey;
      if (mod && (e.key === 'z' || e.key === 'Z')) {
        e.preventDefault();
        if (e.shiftKey) { if (this.history && this.history.redo()) this.setStatus('Redo'); }
        else { if (this.history && this.history.undo()) this.setStatus('Undo'); }
        this.refreshBlockStates();
        return;
      }
      if (mod && (e.key === 'y' || e.key === 'Y')) {
        e.preventDefault();
        if (this.history && this.history.redo()) this.setStatus('Redo');
        this.refreshBlockStates();
        return;
      }
      if (e.key === 'Enter') { this._handleReturnKey(e); return; }
      if (e.key === 'Backspace') { this._handleBackspaceKey(e); return; }
      if (['ArrowUp', 'ArrowDown'].includes(e.key)) {
        this._handleArrowKeys(e);
        return;
      }
      if (e.key === 'Delete' || e.key === 'Backspace') {
        const selected = this.bodyEl.querySelector(':scope > .is-selected');
        if (selected) {
          e.preventDefault();
          const adj = selected.nextElementSibling || selected.previousElementSibling;
          selected.remove(); this._hideBlockToolbar(); this._markDirty();
          if (adj) this.placeCaretAtEnd(adj);
          else this.ensureTrailingParagraph();
          return;
        }
      }
      if (this.slashState.open) {
        if (e.key === 'Escape') { e.preventDefault(); this._closeSlashMenu(); return; }
        if (e.key === 'ArrowDown') { e.preventDefault(); this.slashState.index++; this._renderSlashMenu(); return; }
        if (e.key === 'ArrowUp') { e.preventDefault(); this.slashState.index = Math.max(0, this.slashState.index - 1); this._renderSlashMenu(); return; }
      }
    });

    this.bodyEl.addEventListener('click', (evt) => {
      const fig = evt.target.closest && evt.target.closest('figure.image-block');
      if (fig && !(evt.target.closest && evt.target.closest('figcaption'))) {
        if (evt.shiftKey) {
          evt.preventDefault();
          this._toggleMultiSelectBlock(fig);
          return;
        }
        evt.preventDefault();
        this._selectBlock(fig);
        this._addImageResizeHandle(fig);
        this._observeImageSize(fig);
        return;
      }
      const block = evt.target && evt.target.closest && evt.target.closest('.de-body > *');
      if (block && block.parentElement === this.bodyEl && !block.classList.contains('image-block')) {
        const t = block.tagName.toLowerCase();
        if (['p','h1','h2','h3','h4','h5','h6','blockquote','pre','ul','ol','hr','figure','div'].includes(t)) {
          if (evt.target === block || block.classList.contains('table-block') || block.classList.contains('math')) {
            if (evt.shiftKey) {
              evt.preventDefault();
              this._toggleMultiSelectBlock(block);
              return;
            }
            evt.preventDefault();
            this._selectBlock(block);
            return;
          }
        }
      }
      if (evt.target === this.bodyEl) {
        this._deselectBlocks();
        return;
      }
      if (this.blockToolbar && !this.blockToolbar.contains(evt.target) && !(this.imageToolbar && this.imageToolbar.contains(evt.target)) && !(this.multiToolbar && this.multiToolbar.contains(evt.target))) this._deselectBlocks();
    });

    this.bodyEl.addEventListener('mouseup', () => this._showBubble());
    this.bodyEl.addEventListener('keyup', () => this._showBubble());
    document.addEventListener('selectionchange', () => {
      this._showBubble();
      this.rememberSelection();
    });
    document.addEventListener('click', (evt) => {
      if (!this.bubble.contains(evt.target)) this._showBubble();
      if (!this.slashMenu.contains(evt.target) && !this.bodyEl.contains(evt.target)) this._closeSlashMenu();
      if (!this.insertMenu.contains(evt.target)) this._closeInsertMenu();
      const inEditor = this.bodyEl.contains(evt.target);
      const inToolbar = (this.bubble && this.bubble.contains(evt.target)) ||
                        (this.blockToolbar && this.blockToolbar.contains(evt.target)) ||
                        (this.imageToolbar && this.imageToolbar.contains(evt.target)) ||
                        (this.multiToolbar && this.multiToolbar.contains(evt.target)) ||
                        (this.tableToolbar && this.tableToolbar.contains(evt.target));
      if (!inEditor && !inToolbar) {
        this._deselectBlocks();
        this.bubble.style.display = 'none';
      } else if (inEditor && !inToolbar) {
        if (this.imageToolbar && !this.imageToolbar.contains(evt.target)) this._hideImageToolbar();
        if (this.tableToolbar && !this.tableToolbar.contains(evt.target)) this.tableToolbar.classList.remove('open');
        const selectedObject = this.bodyEl.querySelector(':scope > .is-selected.image-block, :scope > .is-selected.table-block');
        if (selectedObject && !selectedObject.contains(evt.target)) selectedObject.classList.remove('is-selected');
      }
    });

    this.bodyEl.addEventListener('paste', () => {
      setTimeout(() => { this.ensureTrailingParagraph(); this.refreshBlockStates(); this._refreshImageBlocks(); this._markDirty(); }, 0);
    });

    this.blockPlus.addEventListener('click', (e) => {
      e.preventDefault(); e.stopPropagation();
      if (this.insertState.open && this.insertState.referenceBlock === this.activeBlockForPlus) {
        this._closeInsertMenu();
      } else if (this.activeBlockForPlus) {
        this._openInsertMenu(this.activeBlockForPlus, this.blockPlus);
      }
    });

    this.blockPlus.addEventListener('mouseenter', () => {
      if (this.activeBlockForPlus) this._positionBlockPlus(this.activeBlockForPlus);
    });

    this.uploadInput.addEventListener('change', async () => {
      const file = this.uploadInput.files && this.uploadInput.files[0];
      this.uploadInput.value = '';
      if (!file) return;
      try {
        const url = await this.uploadImage(file);
        this.insertImageFromUrl(url, file.name);
        this.setStatus(`Uploaded: ${url}`);
      } catch (err) {
        this.setStatus(String(err.message || err), true);
      }
    });

    this.bubble.addEventListener('click', (evt) => {
      const btn = evt.target.closest('button');
      if (!btn) return;
      const cmd = btn.getAttribute('data-cmd');
      const action = btn.getAttribute('data-action');
      if (cmd) { this.exec(cmd); return; }
      if (action === 'link') {
        this.askInput({ title: 'Add Link', label: 'URL', placeholder: 'https://example.com', okText: 'Insert' })
          .then((url) => { if (url) this.exec('createLink', url); });
      }
      if (action === 'h2') this.transformCurrentBlock('h2');
      if (action === 'quote') this.transformCurrentBlock('blockquote');
      if (action === 'code') this.insertCodeBlockPrompt();
      if (action === 'highlight') this.exec('hiliteColor', this.inlineColor.value);
      if (action === 'clear-hl') this.clearInlineHighlight();
      if (action === 'clear-color') this.clearTextColor();
      if (action === 'font-size-up') this._changeFontSize(2);
      if (action === 'font-size-down') this._changeFontSize(-2);
    });

    this.inlineColor.addEventListener('input', () => this.exec('hiliteColor', this.inlineColor.value));
    this.textColor.addEventListener('input', () => this.exec('foreColor', this.textColor.value));
  }

  _markDirty() {
    this.dirty = true;
    if (this.history) this.history.debouncedPush('edit', 350);
    if (this.opts.onChange) this.opts.onChange(this.getOrgBody());
  }

  // ---------- Org export/import ----------
  nodeToOrg(node) {
    if (!node) return '';
    if (node.nodeType === Node.TEXT_NODE) return node.nodeValue || '';
    if (node.nodeType !== Node.ELEMENT_NODE) return '';
    if (node.classList && node.classList.contains('de-code-block-actions')) return '';

    const tag = node.tagName.toLowerCase();
    const inner = () => Array.from(node.childNodes).map(n => this.nodeToOrg(n)).join('');

    if (tag === 'br') return '\\\\\n';
    if (tag === 'strong' || tag === 'b') return `*${inner().trim()}*`;
    if (tag === 'em' || tag === 'i') return `/${inner().trim()}/`;
    if (tag === 'u') return `_${inner().trim()}_`;
    if (tag === 'del') return `+${inner().trim()}+`;
    if (tag === 'code' && (!node.parentElement || node.parentElement.tagName.toLowerCase() !== 'pre')) {
      return `~${DraftEditor.pickNodeText(node).trim()}~`;
    }
    if (tag === 'a' && node.classList.contains('de-embed-card-link')) return '';
    if (tag === 'a') {
      const href = node.getAttribute('href') || '';
      const text = inner().trim() || href;
      return href ? `[[${href}][${text}]]` : text;
    }

    if (tag === 'pre') {
      const lang = (node.getAttribute('data-lang') || 'text').trim();
      const bg = (node.getAttribute('data-bg') || 'light').trim();
      const syntaxTheme = (node.getAttribute('data-syntax-theme') || 'default').trim();
      const collect = (n) => {
        let out = '';
        Array.from(n.childNodes).forEach(c => {
          if (c.nodeType === Node.TEXT_NODE) out += c.nodeValue || '';
          else if (c.nodeType === Node.ELEMENT_NODE && c.classList && c.classList.contains('de-code-block-actions')) return;
          else if (c.tagName && c.tagName.toLowerCase() === 'br') out += '\n';
          else out += collect(c);
        });
        return out;
      };
      const raw = (node.dataset.raw || collect(node)).replace(/\u200b/g, '').trimEnd();
      let out = '';
      if (bg && bg !== 'light') out += '#+ATTR_HTML: :data-bg ' + bg + '\n';
      if (syntaxTheme && syntaxTheme !== 'default') out += '#+ATTR_HTML: :data-syntax-theme ' + syntaxTheme + '\n';
      out += '#+BEGIN_SRC ' + lang + '\n' + raw + '\n#+END_SRC\n\n';
      return out;
    }

    if (tag === 'figure' && node.classList.contains('table-block')) {
      const table = node.querySelector('table');
      if (!table) return inner().trimEnd() + '\n\n';
      const rows = Array.from(table.querySelectorAll('tr'));
      const lines = rows.map(tr => {
        const cells = Array.from(tr.children).map(td => DraftEditor.escOrg(td.innerText || '')).join(' | ');
        return '| ' + cells + ' |';
      });
      return '\n' + lines.join('\n') + '\n\n';
    }
    if (tag === 'table') {
      const rows = Array.from(node.querySelectorAll('tr'));
      const lines = rows.map(tr => {
        const cells = Array.from(tr.children).map(td => DraftEditor.escOrg(td.innerText || '')).join(' | ');
        return '| ' + cells + ' |';
      });
      return '\n' + lines.join('\n') + '\n\n';
    }

    if (tag === 'span' && node.classList.contains('math')) {
      const latex = (node.getAttribute('data-latex') || node.textContent || '').trim();
      return latex ? `\\(${DraftEditor.escOrg(latex)}\\)` : '';
    }
    if (tag === 'div' && node.classList.contains('math')) {
      const latex = (node.getAttribute('data-latex') || node.textContent || '').trim();
      if (!latex) return '';
      return `#+BEGIN_EXPORT html\n<div class="math" data-latex="${DraftEditor.escHtml(latex)}">${DraftEditor.escHtml(latex)}</div>\n#+END_EXPORT\n\n`;
    }

    if (tag === 'figure' && node.classList.contains('image-block')) {
      const img = node.querySelector('img');
      const cap = node.querySelector('figcaption');
      const src = img ? img.getAttribute('src') || '' : '';
      if (!src) return '';
      const capText = cap ? DraftEditor.escOrg(cap.textContent || '') : '';
      const caption = DraftEditor.isPlaceholderCaption(capText, src) ? '' : capText;
      const width = (node.getAttribute('data-width') || '').trim();
      const height = (node.getAttribute('data-height') || '').trim();
      const fit = (node.getAttribute('data-fit') || '').trim();
      const position = (node.getAttribute('data-position') || '').trim();
      const filter = (node.getAttribute('data-filter') || '').trim();
      const align = (node.getAttribute('data-align') || '').trim();
      let attrs = '';
      if (width) attrs += ` :width ${width}px`;
      if (height) attrs += ` :height ${height}px`;
      if (fit) attrs += ` :data-fit ${fit}`;
      if (position) attrs += ` :data-position ${position}`;
      if (filter && filter !== 'none') attrs += ` :data-filter ${filter}`;
      if (align) attrs += ` :data-align ${align}`;
      const attrLine = attrs ? `#+ATTR_HTML:${attrs}\n` : '';
      const captionLine = caption ? `#+CAPTION: ${caption}\n` : '';
      return `${attrLine}${captionLine}[[${src}]]\n\n`;
    }
    if (tag === 'figure' && node.classList.contains('video-embed')) {
      const src = node.getAttribute('data-src') || (node.querySelector('iframe')?.getAttribute('src') || '');
      if (!src) return '';
      return `#+BEGIN_EXPORT html\n<div style="position:relative;padding-bottom:56.25%;height:0;overflow:hidden;border-radius:8px;"><iframe src="${DraftEditor.escHtml(src)}" style="position:absolute;top:0;left:0;width:100%;height:100%;border:0;" loading="lazy" allow="autoplay; encrypted-media; picture-in-picture" allowfullscreen></iframe></div>\n#+END_EXPORT\n\n`;
    }
    if (tag === 'figure' && node.classList.contains('embed-card')) {
      const url = node.getAttribute('data-url') || '';
      const title = node.getAttribute('data-title') || url;
      const desc = node.getAttribute('data-description') || '';
      const site = node.getAttribute('data-site') || '';
      const image = node.getAttribute('data-image') || '';
      return `#+BEGIN_EXPORT html\n<div style="border:1px solid #ddd;border-radius:8px;overflow:hidden;display:grid;grid-template-columns:1fr 220px;margin:16px 0;"><a href="${DraftEditor.escHtml(url)}" style="padding:14px;text-decoration:none;color:inherit;"><div style="font-size:24px;font-weight:600;line-height:1.3;">${DraftEditor.escHtml(title)}</div><div style="margin-top:8px;color:#555;">${DraftEditor.escHtml(desc)}</div><div style="margin-top:12px;color:#777;">${DraftEditor.escHtml(site)}</div></a>${image ? `<img src="${DraftEditor.escHtml(image)}" alt="${DraftEditor.escHtml(title)}" style="width:100%;height:100%;object-fit:cover;">` : ''}</div>\n#+END_EXPORT\n\n`;
    }
    if (tag === 'img') {
      const src = node.getAttribute('src') || '';
      return src ? `[[${src}]]` : '';
    }
    if (tag === 'mark') {
      const color = node.style.backgroundColor || '#ffea55';
      const text = DraftEditor.pickNodeText(node).trim();
      return text ? `@@html:<mark style="background-color:${DraftEditor.escHtml(color)}">${DraftEditor.escHtml(text)}</mark>@@` : '';
    }
    if (tag === 'span') {
      const color = node.style.color || '';
      const bg = node.style.backgroundColor || '';
      const fontSize = node.style.fontSize || '';
      const text = DraftEditor.pickNodeText(node).trim();
      if (!text) return '';
      if (fontSize) return `@@html:<span style="font-size:${DraftEditor.escHtml(fontSize)}">${DraftEditor.escHtml(text)}</span>@@`;
      if (bg && bg !== 'transparent') return `@@html:<mark style="background-color:${DraftEditor.escHtml(bg)}">${DraftEditor.escHtml(text)}</mark>@@`;
      if (color) return `@@html:<span style="color:${DraftEditor.escHtml(color)}">${DraftEditor.escHtml(text)}</span>@@`;
      return inner();
    }
    if (tag === 'h1') {
      return `* ${inner().trim()}\n\n`;
    }
    if (tag === 'h2') {
      return `** ${inner().trim()}\n\n`;
    }
    if (tag === 'h3') {
      return `*** ${inner().trim()}\n\n`;
    }
    if (tag === 'h4') {
      return `**** ${inner().trim()}\n\n`;
    }
    if (tag === 'h5') {
      return `***** ${inner().trim()}\n\n`;
    }
    if (tag === 'h6') {
      return `****** ${inner().trim()}\n\n`;
    }
    if (tag === 'blockquote') {
      return `#+BEGIN_QUOTE\n${inner().trim()}\n#+END_QUOTE\n\n`;
    }
    if (tag === 'hr') return '\n-----\n\n';
    if (tag === 'li') return `${inner().trim()}\n`;
    if (tag === 'ul') {
      const items = Array.from(node.children).map(li => `- ${this.nodeToOrg(li).trim()}`).join('\n');
      return `${items}\n\n`;
    }
    if (tag === 'ol') {
      const items = Array.from(node.children).map((li, i) => `${i + 1}. ${this.nodeToOrg(li).trim()}`).join('\n');
      return `${items}\n\n`;
    }
    if (tag === 'div' || tag === 'p') {
      return `${inner().trimEnd()}\n\n`;
    }
    return inner();
  }

  getOrgBody() {
    return Array.from(this.bodyEl.childNodes)
      .map(n => this.nodeToOrg(n))
      .join('')
      .replace(/\n{3,}/g, '\n\n')
      .trimEnd();
  }

  toOrgDocument(title, body) {
    const date = new Date().toISOString().slice(0, 10);
    return `#+TITLE: ${title}\n#+DATE: ${date}\n#+OPTIONS: toc:nil\n#+DESCRIPTION: Draft post\n\n${body}\n`;
  }

  static parseInlineOrg(text) {
    if (!text) return '';

    const rawHtmls = [];
    const links = [];
    const codes = [];
    const maths = [];

    // Protect raw HTML fragments first so their contents are not escaped.
    text = text.replace(/@@html:([\s\S]+?)@@/g, (m, html) => {
      const id = rawHtmls.length;
      rawHtmls.push(html);
      return `__ORG_RAW_${id}__`;
    });

    // Protect LaTeX fragments.
    text = text.replace(/\\\(([\s\S]+?)\\\)/g, (m, latex) => {
      const id = maths.length;
      maths.push(latex);
      return `__ORG_MATH_${id}__`;
    });
    text = text.replace(/@@latex:([\s\S]+?)@@/g, (m, latex) => {
      const id = maths.length;
      maths.push(latex);
      return `__ORG_MATH_${id}__`;
    });

    // Protect links so bracket syntax is preserved.
    text = text.replace(/\[\[([^\]]+)\](?:\[([^\]]+)\])?\]/g, (m, url, desc) => {
      const id = links.length;
      links.push({ url: url || '', desc: desc || url || '' });
      return `__ORG_LINK_${id}__`;
    });

    // Protect code / verbatim so inline markup inside them is untouched.
    const protectCode = (m, code) => {
      const id = codes.length;
      codes.push(code);
      return `__ORG_CODE_${id}__`;
    };
    text = text.replace(/~([^~\n]+?)~/g, protectCode);
    text = text.replace(/=([^=\n]+?)=/g, protectCode);

    // Escape the remaining plain text.
    text = DraftEditor.escHtml(text);

    // Apply emphasis markup. Word-boundary rules mirror Org's defaults:
    // opening may follow whitespace/start/(/'/"/{ and closing may precede
    // whitespace/end/.,;:!?"')}\[]\ or backslash.
    const emphasisPre = '(?<=^|[\\s(\'"{])';
    const emphasisPost = '(?=[\\s.,;:!?"\')\\}\\]\\[\\\\]|$)';
    text = text.replace(new RegExp(`${emphasisPre}\\*([^*\\n]+?)\\*${emphasisPost}`, 'g'), '<strong>$1</strong>');
    text = text.replace(new RegExp(`${emphasisPre}/([^/\\n]+?)/${emphasisPost}`, 'g'), '<em>$1</em>');
    text = text.replace(new RegExp(`${emphasisPre}_([^_\\n]+?)_${emphasisPost}`, 'g'), '<u>$1</u>');
    text = text.replace(new RegExp(`${emphasisPre}\\+([^+\\n]+?)\\+${emphasisPost}`, 'g'), '<del>$1</del>');

    // Restore code.
    text = text.replace(/__ORG_CODE_(\d+)__/g, (m, id) => `<code>${DraftEditor.escHtml(codes[id])}</code>`);

    // Restore math.
    text = text.replace(/__ORG_MATH_(\d+)__/g, (m, id) => {
      const latex = maths[id] || '';
      return `<span class="math" data-latex="${DraftEditor.escHtml(latex.trim())}">${DraftEditor.escHtml(latex.trim())}</span>`;
    });

    // Restore links.
    text = text.replace(/__ORG_LINK_(\d+)__/g, (m, id) => {
      const { url, desc } = links[id];
      return `<a href="${DraftEditor.escHtml(url)}" target="_blank" rel="noopener">${DraftEditor.escHtml(desc)}</a>`;
    });

    // Restore raw HTML fragments last so they are emitted verbatim.
    text = text.replace(/__ORG_RAW_(\d+)__/g, (m, id) => rawHtmls[id]);

    return text;
  }

  renderOrgToHtml(orgText) {
    const lines = String(orgText || '').replace(/\r\n?/g, '\n').split('\n');
    const out = [];
    let i = 0;
    let pendingCaption = '';
    let pendingAttrHtml = null;

    const isImageUrl = (url) => /\.(?:png|jpe?g|gif|webp|svg|bmp)(?:\?.*)?$/i.test(url);

    const blockStyleFromAttr = (attr) => {
      return { style: '', dataAttrs: '' };
    };

    while (i < lines.length) {
      let line = lines[i];
      let trimmed = line.trim();
      const attrHtml = pendingAttrHtml;
      pendingAttrHtml = null;

      if (!trimmed) { i++; continue; }

      // Skip file-level header keywords that might leak into the body.
      if (/^#\+(TITLE|DATE|OPTIONS|DESCRIPTION|AUTHOR|EMAIL|FILETAGS|KEYWORDS|LANGUAGE|HTML_HEAD|HTML_HEAD_EXTRA|LATEX_HEADER|LATEX_HEADER_EXTRA|SETUPFILE|SELECT_TAGS|EXCLUDE_TAGS|EXPORT_SELECT_TAGS|EXPORT_EXCLUDE_TAGS|INFOJS_OPT|BIND):\s*/i.test(trimmed)) {
        i++; continue;
      }

      // Block attributes.
      if (/^#\+CAPTION:\s*/i.test(trimmed)) {
        pendingCaption = trimmed.replace(/^#\+CAPTION:\s*/i, '').trim();
        i++; continue;
      }
      if (/^#\+ATTR_HTML:\s*/i.test(trimmed)) {
        pendingAttrHtml = pendingAttrHtml || {};
        const rest = trimmed.replace(/^#\+ATTR_HTML:\s*/i, '').trim();
        const pairs = rest.matchAll(/:([a-zA-Z0-9_-]+)\s+([^:\s][^:]*?)(?=\s+:|\s*$)/g);
        for (const m of pairs) pendingAttrHtml[m[1]] = m[2].trim();
        i++; continue;
      }
      if (/^#\+(ATTR_[A-Z]+|NAME):\s*/i.test(trimmed)) {
        i++; continue;
      }

      // Source blocks.
      if (/^#\+BEGIN_SRC\b/i.test(trimmed)) {
        const match = trimmed.match(/^#\+BEGIN_SRC\s+([a-zA-Z0-9_+-]+)/i);
        const lang = match ? match[1] : 'text';
        const srcLines = [];
        i++;
        while (i < lines.length && !/^#\+END_SRC\b/i.test(lines[i].trim())) {
          srcLines.push(lines[i]);
          i++;
        }
        if (i < lines.length) i++;
        const code = srcLines.join('\n').replace(/\u200b/g, '').trimEnd();
        const bg = (attrHtml && attrHtml['data-bg']) || 'light';
        const syntaxTheme = (attrHtml && attrHtml['data-syntax-theme']) || 'default';
        out.push(`<pre class="code-block syntax-theme-${DraftEditor.escHtml(syntaxTheme)} language-${DraftEditor.escHtml(lang)}" data-lang="${DraftEditor.escHtml(lang)}" data-bg="${DraftEditor.escHtml(bg)}" data-syntax-theme="${DraftEditor.escHtml(syntaxTheme)}"><code>${DraftEditor.escHtml(code)}</code></pre>`);
        continue;
      }

      // Tables.
      if (/^\|/.test(trimmed)) {
        const tableLines = [];
        while (i < lines.length && /^\|/.test(lines[i].trim())) {
          tableLines.push(lines[i]);
          i++;
        }
        const rows = tableLines.filter(l => !/^\|[-:|\s]+\|\s*$/.test(l.trim())).map(line => {
          const cells = line.trim().split('|').slice(1, -1).map(s => DraftEditor.parseInlineOrg(s.trim()));
          return `<tr>${cells.map(c => `<td contenteditable="true">${c || '<br>'}</td>`).join('')}</tr>`;
        });
        if (rows.length) {
          out.push(`<figure class="table-block"><table><tbody>${rows.join('')}</tbody></table></figure>`);
        }
        continue;
      }

      // Block math.
      if (/^\\\[/.test(trimmed)) {
        const mathLines = [];
        if (/\\\]$/.test(trimmed)) {
          mathLines.push(trimmed.replace(/^\\\[\s*/, '').replace(/\s*\\\]$/, ''));
          i++;
        } else {
          i++;
          while (i < lines.length && !/^\\\]/.test(lines[i].trim())) {
            mathLines.push(lines[i]);
            i++;
          }
          if (i < lines.length) i++;
        }
        const latex = mathLines.join('\n').trim();
        if (latex) {
          out.push(`<div class="math" data-latex="${DraftEditor.escHtml(latex)}">${DraftEditor.escHtml(latex)}</div>`);
        }
        continue;
      }

      // Property drawers (from normalized headings) are ignored.
      if (/^:PROPERTIES:\s*$/i.test(trimmed)) {
        i++;
        while (i < lines.length && !/^:END:\s*$/i.test(lines[i].trim())) i++;
        if (i < lines.length) i++;
        continue;
      }

      // Quote blocks.
      if (/^#\+BEGIN_QUOTE\b/i.test(trimmed)) {
        const quoteLines = [];
        i++;
        while (i < lines.length && !/^#\+END_QUOTE\b/i.test(lines[i].trim())) {
          quoteLines.push(lines[i]);
          i++;
        }
        if (i < lines.length) i++;
        const body = quoteLines.map(l => `<p>${DraftEditor.parseInlineOrg(l)}</p>`).join('');
        const { style, dataAttrs } = blockStyleFromAttr(attrHtml);
        out.push(`<blockquote${style}${dataAttrs}>${body}</blockquote>`);
        continue;
      }

      // Raw HTML export blocks.
      if (/^#\+BEGIN_EXPORT\s+html\b/i.test(trimmed)) {
        const exportLines = [];
        i++;
        while (i < lines.length && !/^#\+END_EXPORT\b/i.test(lines[i].trim())) {
          exportLines.push(lines[i]);
          i++;
        }
        if (i < lines.length) i++;
        out.push(exportLines.join('\n'));
        continue;
      }

      // Example blocks (render like plain code blocks).
      if (/^#\+BEGIN_EXAMPLE\b/i.test(trimmed)) {
        const exLines = [];
        i++;
        while (i < lines.length && !/^#\+END_EXAMPLE\b/i.test(lines[i].trim())) {
          exLines.push(lines[i]);
          i++;
        }
        if (i < lines.length) i++;
        const code = exLines.join('\n').trimEnd();
        out.push(`<pre class="code-block syntax-theme-default language-text" data-lang="text" data-bg="light" data-syntax-theme="default"><code>${DraftEditor.escHtml(code)}</code></pre>`);
        continue;
      }

      // Horizontal rule.
      if (/^-----+\s*$/.test(trimmed)) {
        out.push('<hr>');
        i++; continue;
      }

      // Headings.
      const heading = trimmed.match(/^(\*{1,6})\s+(.+)$/);
      if (heading) {
        const level = heading[1].length;
        const { style, dataAttrs } = blockStyleFromAttr(attrHtml);
        out.push(`<h${level}${style}${dataAttrs}>${DraftEditor.parseInlineOrg(heading[2])}</h${level}>`);
        i++; continue;
      }

      // Unordered lists.
      const ulMatch = trimmed.match(/^[-*]\s+(.+)$/);
      if (ulMatch) {
        const items = [];
        let current = [ulMatch[1]];
        i++;
        while (i < lines.length) {
          const nextLine = lines[i];
          const nextTrimmed = nextLine.trim();
          if (!nextTrimmed) break;
          // Hard-break continuation: a line following a `\\` line belongs to the same item.
          if (current.length > 0 && /\\\\$/.test(current[current.length - 1])) {
            current.push(nextTrimmed);
            i++;
            continue;
          }
          const nextItem = nextLine.match(/^[-*]\s+(.+)$/);
          if (nextItem) {
            items.push(current);
            current = [nextItem[1]];
            i++;
            continue;
          }
          if (/^\s+/.test(nextLine)) {
            current.push(nextTrimmed);
            i++;
            continue;
          }
          break;
        }
        items.push(current);
        const lis = items.map(item => {
          const joined = item.join(' ').replace(/\\\\ /g, '__ORG_BR__').replace(/\\\\$/g, '__ORG_BR__');
          return `<li>${DraftEditor.parseInlineOrg(joined).replace(/__ORG_BR__/g, '<br>')}</li>`;
        }).join('');
        out.push(`<ul>${lis}</ul>`);
        continue;
      }

      // Ordered lists.
      const olMatch = trimmed.match(/^(\d+)[.)]\s+(.+)$/);
      if (olMatch) {
        const items = [];
        let current = [olMatch[2]];
        i++;
        while (i < lines.length) {
          const nextLine = lines[i];
          const nextTrimmed = nextLine.trim();
          if (!nextTrimmed) break;
          if (current.length > 0 && /\\\\$/.test(current[current.length - 1])) {
            current.push(nextTrimmed);
            i++;
            continue;
          }
          const nextItem = nextLine.match(/^\d+[.)]\s+(.+)$/);
          if (nextItem) {
            items.push(current);
            current = [nextItem[1]];
            i++;
            continue;
          }
          if (/^\s+/.test(nextLine)) {
            current.push(nextTrimmed);
            i++;
            continue;
          }
          break;
        }
        items.push(current);
        const lis = items.map(item => {
          const joined = item.join(' ').replace(/\\\\ /g, '__ORG_BR__').replace(/\\\\$/g, '__ORG_BR__');
          return `<li>${DraftEditor.parseInlineOrg(joined).replace(/__ORG_BR__/g, '<br>')}</li>`;
        }).join('');
        out.push(`<ol>${lis}</ol>`);
        continue;
      }

      // Standalone image/link.
      const simpleLink = trimmed.match(/^\[\[([^\]]+)\]\]$/);
      if (simpleLink) {
        const url = simpleLink[1];
        if (isImageUrl(url)) {
          const cap = DraftEditor.parseInlineOrg(pendingCaption);
          pendingCaption = '';
          const width = attrHtml && attrHtml['width'] ? attrHtml['width'] : '760px';
          const height = attrHtml && attrHtml['height'] ? attrHtml['height'] : '420px';
          const fit = attrHtml && attrHtml['data-fit'] ? attrHtml['data-fit'] : 'cover';
          const position = attrHtml && attrHtml['data-position'] ? attrHtml['data-position'] : 'center';
          const filter = attrHtml && attrHtml['data-filter'] ? attrHtml['data-filter'] : 'none';
          const align = attrHtml && attrHtml['data-align'] ? attrHtml['data-align'] : 'center';
          const fitClass = ` fit-${DraftEditor.escHtml(fit)}`;
          const posClass = ` pos-${DraftEditor.escHtml(position)}`;
          const widthStyle = ` style="width:${DraftEditor.escHtml(width)};max-width:100%;height:${DraftEditor.escHtml(height)};" data-width="${parseInt(width, 10)}" data-height="${parseInt(height, 10)}"`;
          const alt = cap || url.split('/').pop().split('?')[0] || 'image';
          const filterClass = filter && filter !== 'none' ? ` filter-${DraftEditor.escHtml(filter)}` : '';
          out.push(`<figure class="image-block${fitClass}${posClass} align-${DraftEditor.escHtml(align)}${filterClass}"${widthStyle} data-align="${DraftEditor.escHtml(align)}" data-filter="${DraftEditor.escHtml(filter)}" data-fit="${DraftEditor.escHtml(fit)}" data-position="${DraftEditor.escHtml(position)}"><img src="${DraftEditor.escHtml(url)}" alt="${alt}">${cap ? `<figcaption contenteditable="true">${cap}</figcaption>` : ''}</figure>`);
        } else {
          out.push(`<p><a href="${DraftEditor.escHtml(url)}" target="_blank" rel="noopener">${DraftEditor.escHtml(url)}</a></p>`);
        }
        i++; continue;
      }

      const descLink = trimmed.match(/^\[\[([^\]]+)\]\[([^\]]+)\]\]$/);
      if (descLink) {
        const url = descLink[1];
        const desc = descLink[2];
        if (isImageUrl(url)) {
          const cap = DraftEditor.parseInlineOrg(pendingCaption || desc);
          pendingCaption = '';
          const width = attrHtml && attrHtml['width'] ? attrHtml['width'] : '760px';
          const height = attrHtml && attrHtml['height'] ? attrHtml['height'] : '420px';
          const fit = attrHtml && attrHtml['data-fit'] ? attrHtml['data-fit'] : 'cover';
          const position = attrHtml && attrHtml['data-position'] ? attrHtml['data-position'] : 'center';
          const filter = attrHtml && attrHtml['data-filter'] ? attrHtml['data-filter'] : 'none';
          const align = attrHtml && attrHtml['data-align'] ? attrHtml['data-align'] : 'center';
          const fitClass = ` fit-${DraftEditor.escHtml(fit)}`;
          const posClass = ` pos-${DraftEditor.escHtml(position)}`;
          const widthStyle = ` style="width:${DraftEditor.escHtml(width)};max-width:100%;height:${DraftEditor.escHtml(height)};" data-width="${parseInt(width, 10)}" data-height="${parseInt(height, 10)}"`;
          const alt = cap || url.split('/').pop().split('?')[0] || 'image';
          const filterClass = filter && filter !== 'none' ? ` filter-${DraftEditor.escHtml(filter)}` : '';
          out.push(`<figure class="image-block${fitClass}${posClass} align-${DraftEditor.escHtml(align)}${filterClass}"${widthStyle} data-align="${DraftEditor.escHtml(align)}" data-filter="${DraftEditor.escHtml(filter)}" data-fit="${DraftEditor.escHtml(fit)}" data-position="${DraftEditor.escHtml(position)}"><img src="${DraftEditor.escHtml(url)}" alt="${alt}">${cap ? `<figcaption contenteditable="true">${cap}</figcaption>` : ''}</figure>`);
        } else {
          out.push(`<p><a href="${DraftEditor.escHtml(url)}" target="_blank" rel="noopener">${DraftEditor.parseInlineOrg(desc)}</a></p>`);
        }
        i++; continue;
      }

      // Paragraph: collect lines until blank line or block marker.
      const paraLines = [trimmed];
      i++;
      while (i < lines.length) {
        const nextLine = lines[i];
        const nextTrimmed = nextLine.trim();
        if (!nextTrimmed) break;
        if (/^#\+(BEGIN_|END_|CAPTION:|ATTR_|NAME:|[A-Z_]+:\s)/i.test(nextTrimmed)) break;
        paraLines.push(nextTrimmed);
        i++;
      }
      const paraText = paraLines.join(' ').replace(/\\\\ /g, '__ORG_BR__').replace(/\\\\$/g, '__ORG_BR__');
      const { style, dataAttrs } = blockStyleFromAttr(attrHtml);
      out.push(`<p${style}${dataAttrs}>${DraftEditor.parseInlineOrg(paraText).replace(/__ORG_BR__/g, '<br>')}</p>`);
    }

    return out.join('\n');
  }

  // ---------- Save / load ----------
  async loadDraftFromQuery(slug, kind = 'draft') {
    if (!slug) return false;
    try {
      const res = await fetch(`${this.opts.loadDraftEndpoint}?slug=${encodeURIComponent(slug)}&kind=${encodeURIComponent(kind)}`);
      const data = await res.json();
      if (!res.ok || !data.ok) throw new Error(data.error || 'Unable to load draft');
      if (this.titleEl) this.titleEl.innerText = data.title || '';
      this.bodyEl.innerHTML = this.renderOrgToHtml(data.body || '');
      this.bodyEl.querySelectorAll('pre.code-block').forEach(pre => this._ensureCodeBlockActions(pre));
      this.bodyEl.querySelectorAll('pre.code-block').forEach(pre => { if (window.Prism) this._highlightCodeBlock(pre); });
      this._refreshImageBlocks();
      this.activeDraftTargetPath = data.targetPath || '';
      this.activeSourceMode = data.mode || (kind === 'post' ? 'publish' : 'draft');
      this.setTags(data.tags || []);
      this.setStatus(`Editing ${this.activeSourceMode === 'publish' ? 'published post' : 'draft'}: ${this.activeDraftTargetPath || slug}`);
      this.ensureTrailingParagraph();
      this.refreshBlockStates();
      this.dirty = false;
      if (this.history) this.history.push('load');
      return true;
    } catch (err) {
      this.setStatus(String(err.message || err), true);
      return false;
    }
  }

  setTags(tags) {
    this.tags = (tags || []).map(t => String(t).trim().toLowerCase()).filter(Boolean);
  }

  getTags() {
    return this.tags.slice();
  }

  async saveDoc(mode) {
    const title = this.titleEl ? this.titleEl.innerText.trim() : 'Untitled';
    const body = this.getOrgBody();
    if (!title) {
      this.setStatus('Title is required before saving.', true);
      if (this.titleEl) this.titleEl.focus();
      return;
    }
    try {
      const payload = { title, body, mode, tags: this.tags };
      if (mode === 'draft' && this.activeDraftTargetPath && this.activeDraftTargetPath.startsWith('drafts/')) {
        payload.targetPath = this.activeDraftTargetPath;
      } else if (mode === 'publish' && this.activeDraftTargetPath) {
        payload.targetPath = this.activeDraftTargetPath;
      }
      const res = await fetch(this.opts.saveEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });
      const data = await res.json();
      if (!res.ok || !data.ok) throw new Error(data.error || 'Failed to save');
      if (data.path) this.activeDraftTargetPath = data.path;
      if (data.mode) this.activeSourceMode = data.mode;
      this.dirty = false;
      this.setStatus(`${data.mode === 'publish' ? 'Published' : 'Draft saved'}: ${data.path}`);
      if (this.opts.onSave) this.opts.onSave(data);
    } catch (err) {
      this.setStatus(String(err.message || err), true);
    }
  }

  async copyOrg() {
    const title = this.titleEl ? this.titleEl.innerText.trim() : 'Draft';
    const org = this.toOrgDocument(title, this.getOrgBody());
    try {
      await navigator.clipboard.writeText(org);
      this.setStatus('Org text copied to clipboard.');
    } catch (_) {
      this.setStatus('Clipboard copy failed.', true);
    }
  }

  setHtml(html) {
    this.bodyEl.innerHTML = html || '<p><br></p>';
    this.ensureTrailingParagraph();
    this.refreshBlockStates();
    this._refreshImageBlocks();
  }

  focus() {
    this.bodyEl.focus();
  }
}

if (typeof module !== 'undefined' && module.exports) module.exports = DraftEditor;
