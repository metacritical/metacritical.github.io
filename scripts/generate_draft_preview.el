;;; generate_draft_preview.el --- Build draft preview pages -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)
(require 'ox-html)

(defun sds/slugify (s)
  (let* ((lower (downcase (string-trim s)))
         (step1 (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (step2 (replace-regexp-in-string "-+" "-" step1))
         (trimmed (replace-regexp-in-string "\\`-\\|-\\'" "" step2)))
    (if (string-empty-p trimmed) "draft" trimmed)))

(defun sds/html-escape (s)
  (let ((v (or s "")))
    (setq v (string-replace "&" "&amp;" v))
    (setq v (string-replace "<" "&lt;" v))
    (setq v (string-replace ">" "&gt;" v))
    (setq v (string-replace "\"" "&quot;" v))
    v))

(defun sds/parse-tag-list (raw)
  (let* ((trimmed (string-trim (or raw "")))
         (colon-style (and (string-prefix-p ":" trimmed)
                           (string-suffix-p ":" trimmed)))
         (pieces (if colon-style
                     (split-string trimmed ":" t "[[:space:]\n\r\t]+")
                   (split-string trimmed "[, ]+" t "[[:space:]\n\r\t]+"))))
    (delete-dups
     (seq-filter (lambda (it) (not (string-empty-p it)))
                 (mapcar (lambda (it) (string-trim (downcase it))) pieces)))))

(defun sds/extract-first-image (body)
  (let ((case-fold-search t))
    (when (and body
               (string-match "\\[\\[\\([^]\n]+\\(?:\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\|\\.webp\\|\\.svg\\)\\)\\(?:\\]\\[[^]]*\\)?\\]\\]" body))
      (match-string 1 body))))

(defun sds/estimate-read-mins (text)
  (let* ((clean (replace-regexp-in-string "\\[\\[[^]]+\\]\\[[^]]+\\]\\]" " " (or text "")))
         (words (split-string clean "[^[:alnum:]]+" t))
         (count (length words)))
    (max 1 (/ (+ count 199) 200))))

(defun sds/excerpt (desc body)
  (let* ((source (if (string-empty-p (string-trim (or desc "")))
                     (or body "")
                   desc))
         (flat (replace-regexp-in-string "[\n\r\t]+" " " source))
         (trimmed (string-trim flat)))
    (if (> (length trimmed) 180)
        (concat (substring trimmed 0 177) "...")
      trimmed)))

(defun sds/sort-items-by-date-desc (items)
  (sort (copy-sequence items)
        (lambda (a b)
          (let ((da (or (plist-get a :date) "0000-00-00"))
                (db (or (plist-get b :date) "0000-00-00")))
            (string> da db)))))

(defun sds/group-by-tags (items)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((tags (or (plist-get item :tags) '("untagged"))))
        (dolist (tag tags)
          (puthash tag (cons item (gethash tag table)) table))))
    (let (groups)
      (maphash
       (lambda (k v)
         (push (cons k (sds/sort-items-by-date-desc v)) groups))
       table)
      (sort groups
            (lambda (a b)
              (cond
               ((string= (car a) "untagged") nil)
               ((string= (car b) "untagged") t)
               (t (string< (car a) (car b)))))))))

(defun sds/org-file-to-editable-html (path)
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (let ((org-export-with-toc nil)
              (org-export-with-section-numbers nil)
              (org-export-with-author nil)
              (org-export-with-date nil)
              (org-export-with-title nil)
              (org-export-with-tags nil)
              (org-html-htmlize-output-type 'css))
          (org-export-string-as (buffer-string) 'html t
                                '(:with-toc nil
                                  :section-numbers nil
                                  :with-author nil
                                  :with-date nil
                                  :with-title nil
                                  :with-tags nil))))
    (error "")))

(defun sds/parse-org (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let ((title (file-name-base path))
          (date "")
          (description "")
          (tags '())
          (body-lines '()))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match-p "\\`#\\+TITLE:" line)
            (setq title (string-trim (substring line (length "#+TITLE:")))))
           ((string-match-p "\\`#\\+DATE:" line)
            (setq date (string-trim (substring line (length "#+DATE:")))))
           ((string-match-p "\\`#\\+DESCRIPTION:" line)
            (setq description (string-trim (substring line (length "#+DESCRIPTION:")))))
           ((string-match-p "\\`#\\+FILETAGS:" line)
            (setq tags (append tags (sds/parse-tag-list (substring line (length "#+FILETAGS:"))))))
           ((string-match-p "\\`#\\+TAGS:" line)
            (setq tags (append tags (sds/parse-tag-list (substring line (length "#+TAGS:"))))))
           ((string-match-p "\\`#\\+KEYWORDS:" line)
            (setq tags (append tags (sds/parse-tag-list (substring line (length "#+KEYWORDS:"))))))
           ((string-match-p "\\`#\\+" line)
            nil)
           (t
            (push line body-lines))))
        (forward-line 1))
      (let* ((body (string-trim (string-join (nreverse body-lines) "\n")))
             (clean-tags (delete-dups (seq-filter (lambda (it) (not (string-empty-p it))) tags)))
             (image (sds/extract-first-image body))
             (html-body (sds/org-file-to-editable-html path)))
        (list :title title
              :date date
              :description description
              :tags clean-tags
              :body body
              :html-body html-body
              :image image)))))

(defun sds/detail-page (item)
  (let* ((title (sds/html-escape (plist-get item :title)))
         (body-html (or (plist-get item :html-body) ""))
         (draft-path (sds/html-escape (plist-get item :draft-path)))
         (date (sds/html-escape (or (plist-get item :date) "")))
         (tags (or (plist-get item :tags) '("untagged")))
         (tag-html (mapconcat (lambda (tag) (format "<span class=\"tag\">%s</span>" (sds/html-escape tag))) tags ""))
         (mins (number-to-string (or (plist-get item :read-mins) 1))))
    (format "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>%s</title>
  <link rel=\"icon\" href=\"/media/images/logo.png\" type=\"image/png\">
  <link rel=\"stylesheet\" href=\"/media/css/style.css\" type=\"text/css\">
  <link rel=\"stylesheet\" href=\"/media/css/theme-medium.css\" type=\"text/css\">
  <style>
    body { background:#faf9f5; }
    #wrapper.post-page { max-width: 900px; }
    .draft-badge { font-size:.5em; vertical-align:super; margin-left:6px; color:#6f6a5e; font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; font-weight:600; letter-spacing:0; }
    .draft-nav-link { color:#2d2b27; font-size:14px; font-weight:500; font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; padding:8px 10px; text-decoration:none; }
    .draft-editable { min-height: 40vh; }
    .draft-editable[contenteditable=\"true\"]:focus { outline: none; }
    .draft-editable p { margin-bottom: 20px; }
    .tag { background:#ece7dc; color:#4f4a3f; border-radius:999px; padding:4px 10px; font:600 12px/1.2 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; margin-right:8px; }
    .draft-tools { position: fixed; z-index: 9999; display:none; gap:6px; align-items:center; background:#1f1f1f; border-radius:12px; padding:8px; box-shadow: 0 12px 22px rgba(0,0,0,.2); }
    .draft-tools button { border:none; background:transparent; color:#fff; border-radius:8px; padding:8px 10px; font-weight:700; font-size:13px; min-width:34px; }
    .draft-tools button:hover { background: rgba(255,255,255,.14); }
    .draft-plus { position: fixed; z-index: 9998; display:none; align-items:center; gap:8px; }
    .draft-plus-toggle { width:44px; height:44px; border-radius:50%%; border:1px solid #2f2f2f; background:#1f1f1f; color:#fff; display:inline-flex; align-items:center; justify-content:center; padding:0; }
    .draft-plus-toggle svg { width:25px; height:25px; fill: currentColor; }
    .draft-plus-menu { display:none; align-items:center; gap:8px; }
    .draft-plus.open .draft-plus-menu { display:inline-flex; }
    .draft-plus-menu button { width:36px; height:36px; border-radius:50%%; border:1px solid #0f5f4f; background:#fff; color:#0f5f4f; font:700 12px/1 -apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; }
  </style>
</head>
<body class=\"container\">
<nav class=\"main-nav medium-nav\">
  <div class=\"nav-left site-brand\">
    <a class=\"brand-logo-link\" href=\"/\"><img class=\"brand-logo\" src=\"/media/images/logo.png\" alt=\"Self dot send\"></a>
    <div class=\"brand-copy\"><a class=\"brand-title\" href=\"/\">Selfd<span class=\"brand-o\">o</span>tsend<sup class=\"draft-badge\">Draft</sup></a><div class=\"brand-tagline\">Message passing is just a procedure call.</div></div>
  </div>
  <div class=\"nav-right\">
    <a class=\"draft-nav-link\" href=\"/drafts/\">Drafts</a><a class=\"draft-nav-link\" href=\"/editor\">Write</a><a class=\"draft-nav-link\" href=\"/\">Published</a>
  </div>
</nav>
<section id=\"wrapper\" class=\"post-page\">
  <div class=\"post\">
    <aside class=\"story-rail\" aria-label=\"Story actions\">
      <button class=\"story-clap-btn\" type=\"button\" aria-label=\"Clap for this story\"><img src=\"/media/images/clap.png\" alt=\"\" aria-hidden=\"true\"></button>
      <span class=\"story-clap-count\">0</span><div class=\"story-share-label\">Share</div>
      <a class=\"story-share-link\" href=\"#\" aria-label=\"Share on X\">𝕏</a><a class=\"story-share-link\" href=\"#\" aria-label=\"Share on Facebook\">f</a><a class=\"story-share-link\" href=\"#\" aria-label=\"Share on LinkedIn\">in</a>
    </aside>
    <h1 class=\"title\" id=\"draft-title\" contenteditable=\"true\">%s</h1>
    <div class=\"post-author-row\"><img class=\"post-author-avatar\" src=\"/media/images/avatar.jpg\" alt=\"Pankaj Doharey\"><div class=\"post-author-meta\"><span class=\"post-author-name\">Pankaj Doharey</span><span class=\"post-author-date\">%s · %s min read</span></div></div>
    <div class=\"tags\">%s</div>
    <div id=\"draft-body\" class=\"draft-editable\" contenteditable=\"true\" data-target-path=\"%s\">%s</div>
  </div>
</section>

<div class=\"draft-tools\" id=\"draft-tools\">
  <button type=\"button\" data-cmd=\"bold\"><b>B</b></button>
  <button type=\"button\" data-cmd=\"italic\"><i>I</i></button>
  <button type=\"button\" data-action=\"link\">Link</button>
  <button type=\"button\" data-action=\"highlight\">HL</button>
  <button type=\"button\" data-action=\"clear\">Clr</button>
</div>
<div class=\"draft-plus\" id=\"draft-plus\">
  <button class=\"draft-plus-toggle\" id=\"draft-plus-toggle\" type=\"button\" title=\"Add an image, video, embed, or new part\" aria-label=\"Add an image, video, embed, or new part\"><svg viewBox=\"0 0 25 25\" aria-hidden=\"true\"><path d=\"M20 12h-7V5h-1v7H5v1h7v7h1v-7h7\" fill-rule=\"evenodd\"></path></svg></button>
  <div class=\"draft-plus-menu\" id=\"draft-plus-menu\">
    <button type=\"button\" data-action=\"image\">Img</button>
    <button type=\"button\" data-action=\"upload\">Up</button>
    <button type=\"button\" data-action=\"video\">Vid</button>
    <button type=\"button\" data-action=\"embed\">&lt;&gt;</button>
    <button type=\"button\" data-action=\"code\">{}</button>
  </div>
  <input id=\"draft-upload\" type=\"file\" accept=\"image/*\" hidden>
</div>

<script>
(function(){
  const body = document.getElementById('draft-body');
  const title = document.getElementById('draft-title');
  const tools = document.getElementById('draft-tools');
  const plus = document.getElementById('draft-plus');
  const plusToggle = document.getElementById('draft-plus-toggle');
  const plusMenu = document.getElementById('draft-plus-menu');
  const upload = document.getElementById('draft-upload');
  const targetPath = body.dataset.targetPath;
  let saveTimer = null;
  let plusTimer = null;

  function setSelPos(){
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0 || sel.isCollapsed) { tools.style.display='none'; return; }
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    if (!rect || !rect.width) { tools.style.display='none'; return; }
    tools.style.display='inline-flex';
    tools.style.left = Math.max(8, rect.left + rect.width/2 - tools.offsetWidth/2) + 'px';
    tools.style.top = Math.max(8, rect.top - tools.offsetHeight - 10) + 'px';
  }

  function showPlusNearCaret(){
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) return;
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    plus.style.display='inline-flex';
    plus.classList.remove('open');
    plus.style.left = Math.max(8, rect.left - 8) + 'px';
    plus.style.top = (rect.bottom + 8) + 'px';
    clearTimeout(plusTimer);
    plusTimer = setTimeout(()=>{ plus.style.display='none'; }, 5000);
  }

  function htmlToOrgNode(node){
    if (!node) return '';
    if (node.nodeType === Node.TEXT_NODE) return node.textContent;
    if (node.nodeType !== Node.ELEMENT_NODE) return '';
    const tag = node.tagName.toLowerCase();
    if (tag === 'h2') return '\\n** ' + node.textContent.trim() + '\\n';
    if (tag === 'h3') return '\\n*** ' + node.textContent.trim() + '\\n';
    if (tag === 'blockquote') return '\\n#+BEGIN_QUOTE\\n' + node.textContent.trim() + '\\n#+END_QUOTE\\n';
    if (tag === 'pre') {
      const lang = (node.dataset.lang || '').trim() || 'text';
      return '\\n#+BEGIN_SRC ' + lang + '\\n' + node.innerText.replace(/\\n+$/,'') + '\\n#+END_SRC\\n';
    }
    if (tag === 'figure' && node.classList.contains('image-block')) {
      const img = node.querySelector('img');
      const cap = node.querySelector('figcaption');
      if (!img) return '';
      const alt = (cap ? cap.textContent.trim() : (img.getAttribute('alt') || '')).trim();
      return '\\n[[' + (img.getAttribute('src') || '') + '][' + alt + ']]\\n';
    }
    if (tag === 'figure' && (node.classList.contains('video-embed') || node.classList.contains('embed-card'))) {
      return '\\n#+BEGIN_EXPORT html\\n' + node.outerHTML + '\\n#+END_EXPORT\\n';
    }
    if (tag === 'div' || tag === 'p') return '\\n' + node.textContent.trim() + '\\n';
    let out = '';
    node.childNodes.forEach(ch => { out += htmlToOrgNode(ch); });
    return out;
  }

  function htmlToOrg(){
    let bodyOrg = '';
    body.childNodes.forEach(n => { bodyOrg += htmlToOrgNode(n); });
    bodyOrg = bodyOrg.replace(/\\n{3,}/g, '\\n\\n').trim();
    const t = (title.innerText || '').trim() || 'Untitled Draft';
    return { title: t, body: bodyOrg };
  }

  function saveDraft(){
    const payload = htmlToOrg();
    fetch('/editor/api/save', {
      method: 'POST',
      headers: {'Content-Type':'application/json'},
      body: JSON.stringify({ title: payload.title, body: payload.body, mode: 'draft', targetPath })
    }).catch(()=>{});
  }

  function scheduleSave(){
    clearTimeout(saveTimer);
    saveTimer = setTimeout(saveDraft, 1200);
  }

  function insertHtmlAtCursor(html){
    const sel = window.getSelection();
    if (!sel || sel.rangeCount === 0) return;
    const range = sel.getRangeAt(0);
    range.deleteContents();
    const wrap = document.createElement('div');
    wrap.innerHTML = html;
    const frag = document.createDocumentFragment();
    let node; let last = null;
    while ((node = wrap.firstChild)) { last = frag.appendChild(node); }
    range.insertNode(frag);
    if (last) {
      range.setStartAfter(last); range.collapse(true);
      sel.removeAllRanges(); sel.addRange(range);
    }
    scheduleSave();
  }

  tools.addEventListener('click', (e)=>{
    const btn = e.target.closest('button');
    if (!btn) return;
    const cmd = btn.dataset.cmd;
    const action = btn.dataset.action;
    if (cmd) { document.execCommand(cmd, false, null); scheduleSave(); setSelPos(); return; }
    if (action === 'link') {
      const url = prompt('URL');
      if (url) { document.execCommand('createLink', false, url); scheduleSave(); }
    }
    if (action === 'highlight') { document.execCommand('hiliteColor', false, '#ffea55'); scheduleSave(); }
    if (action === 'clear') { document.execCommand('removeFormat', false, null); scheduleSave(); }
  });

  plusToggle.addEventListener('click', ()=>{ plus.classList.toggle('open'); });

  plusMenu.addEventListener('click', async (e)=>{
    const btn = e.target.closest('button');
    if (!btn) return;
    const action = btn.dataset.action;
    if (action === 'image') {
      const url = prompt('Image URL');
      if (url) insertHtmlAtCursor('<figure class=\"image-block\"><img src=\"' + url + '\" alt=\"\"><figcaption></figcaption></figure><p><br></p>');
      return;
    }
    if (action === 'upload') { upload.click(); return; }
    if (action === 'video') {
      const url = prompt('Video embed URL');
      if (url) insertHtmlAtCursor('<figure class=\"video-embed\"><iframe src=\"' + url + '\" loading=\"lazy\" allowfullscreen></iframe></figure><p><br></p>');
      return;
    }
    if (action === 'embed') {
      const url = prompt('Link URL');
      if (!url) return;
      try {
        const r = await fetch('/editor/api/fetch-meta', { method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify({url}) });
        const j = await r.json();
        const t = j.title || url, d = j.description || '', s = j.site || '', img = j.image || '';
        insertHtmlAtCursor('<figure class=\"embed-card\"><a class=\"embed-card-link\" href=\"'+url+'\"><div class=\"embed-card-copy\"><h4>'+t+'</h4><p>'+d+'</p><small>'+s+'</small></div>' + (img ? '<img src=\"'+img+'\" alt=\"'+t+'\">' : '') + '</a></figure><p><br></p>');
      } catch (_) {}
      return;
    }
    if (action === 'code') {
      const lang = (prompt('Language (e.g. python, sh, js)') || 'text').trim();
      insertHtmlAtCursor('<pre class=\"code-block\" data-lang=\"'+lang+'\">\\n</pre><p><br></p>');
      return;
    }
  });

  upload.addEventListener('change', async ()=>{
    const file = upload.files && upload.files[0];
    if (!file) return;
    try {
      const reader = new FileReader();
      const dataUrl = await new Promise((resolve, reject)=>{ reader.onload = ()=>resolve(reader.result); reader.onerror = reject; reader.readAsDataURL(file); });
      const r = await fetch('/editor/api/upload', { method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify({ filename: file.name, dataUrl }) });
      const j = await r.json();
      if (j && j.ok && j.url) insertHtmlAtCursor('<figure class=\"image-block\"><img src=\"'+j.url+'\" alt=\"'+file.name+'\"><figcaption>'+file.name+'</figcaption></figure><p><br></p>');
    } catch (_) {}
    upload.value = '';
  });

  document.addEventListener('selectionchange', ()=>{ setTimeout(setSelPos, 0); });
  body.addEventListener('keyup', (e)=>{ if (e.key === 'Enter') showPlusNearCaret(); scheduleSave(); });
  body.addEventListener('input', scheduleSave);
  title.addEventListener('input', scheduleSave);

  setInterval(saveDraft, 5000);
})();
</script>
</body>
</html>\n"
            title title date mins tag-html draft-path body-html)))

(defun sds/card-html (item)
  (let* ((slug (sds/html-escape (plist-get item :slug)))
         (title (sds/html-escape (plist-get item :title)))
         (desc (sds/html-escape (or (plist-get item :excerpt) "")))
         (date (sds/html-escape (or (plist-get item :date) "")))
         (mins (number-to-string (or (plist-get item :read-mins) 1)))
         (image (plist-get item :image))
         (image-html
          (if (and image (not (string-empty-p image)))
              (format "<img src=\"%s\" alt=\"%s\">" (sds/html-escape image) title)
            "<div class=\"ph\"></div>")))
    (format "<article class=\"draft-card\">
  <a class=\"draft-link\" href=\"/drafts/%s/\">
    <div class=\"draft-thumb\">%s</div>
    <div class=\"draft-copy\">
      <h3>%s</h3>
      <p>%s</p>
      <div class=\"draft-meta\">%s · %s min read</div>
    </div>
  </a>
</article>"
            slug image-html title desc date mins)))

(defun sds/index-page (items)
  (let* ((groups (sds/group-by-tags items))
         (body
          (mapconcat
           (lambda (group)
             (let ((tag (car group))
                   (tag-items (cdr group)))
               (format "<section class=\"draft-group\">
  <div class=\"group-head\"><h2>%s</h2></div>
  <div class=\"draft-grid\">%s</div>
</section>"
                       (capitalize (sds/html-escape tag))
                       (mapconcat #'sds/card-html tag-items "\n"))))
           groups
           "\n")))
    (format "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
  <title>Draft Preview</title>
  <link rel=\"icon\" href=\"/media/images/logo.png\" type=\"image/png\">
  <style>
    body { margin: 0; background: #f7f6f2; color: #1d1b18; font-family: ui-sans-serif,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; }
    nav.main-nav { padding:12px 22px; background:#faf9f5; margin:0 auto; display:flex; justify-content:space-between; align-items:center; gap:10px; border-bottom:1px solid #e6e1d5; }
    .nav-left.site-brand { display:flex; align-items:center; gap:12px; min-width:0; }
    .brand-logo { width:50px; height:50px; border-radius:50%%; flex:0 0 50px; }
    .brand-copy { display:flex; flex-direction:column; justify-content:center; min-width:0; }
    .brand-title { color:#1f1f1b; font-family:\"Charter\",\"Iowan Old Style\",\"Palatino Linotype\",Palatino,\"Times New Roman\",serif; font-size:3.1em; font-weight:500; line-height:1; letter-spacing:-0.02em; white-space:nowrap; text-decoration:none; }
    .brand-o { color:#36c9c7; }
    .draft-badge { font-size:.5em; vertical-align:super; margin-left:6px; color:#6f6a5e; font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif; font-weight:600; }
    .brand-tagline { font-size:12px; line-height:1.2; color:#7b7569; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
    .nav-right { display:flex; align-items:center; gap:8px; }
    .draft-nav-link { color:#2d2b27; font-size:14px; font-weight:500; padding:8px 10px; text-decoration:none; }
    main { max-width: 1320px; margin: 0 auto; padding: 22px 28px 64px; }
    h1 { margin: 0 0 8px; font-size: clamp(30px, 4.2vw, 56px); line-height: 1.04; font-family: ui-serif, Georgia, Cambria, serif; }
    .lead { margin: 0 0 24px; color:#6a655a; font-size: 15px; }
    .group-head { display:flex; align-items:center; justify-content:space-between; border-bottom:1px solid #dfd8cc; margin-bottom:12px; }
    .group-head h2 { margin: 0 0 -1px; display:inline-block; font-size: 24px; line-height: 1.1; border-bottom:2px solid #b6b0a4; padding-bottom: 6px; }
    .draft-group { margin: 0 0 28px; }
    .draft-grid { display:grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; }
    .draft-card { border:1px solid #ddd7cb; border-radius: 6px; background:#fff; overflow:hidden; }
    .draft-link { color:inherit; text-decoration:none; display:grid; grid-template-columns: 38%% 62%%; min-height: 230px; }
    .draft-thumb { min-height: 100%%; background: linear-gradient(135deg,#ece8df,#ded7ca); }
    .draft-thumb img { width:100%%; height:100%%; object-fit: cover; display:block; }
    .draft-thumb .ph { width:100%%; height:100%%; min-height: 230px; background: radial-gradient(circle at 30%% 20%%,#ece6da,#d9d2c4 58%%); }
    .draft-copy { padding: 18px 18px 16px; display:flex; flex-direction:column; }
    .draft-copy h3 { margin: 0 0 8px; font-size: clamp(30px, 2vw, 46px); line-height: 1.06; letter-spacing: -0.02em; }
    .draft-copy p { margin: 0; font-size: 14px; line-height: 1.45; color:#5f5b53; }
    .draft-meta { margin-top:auto; padding-top: 14px; font-size:13px; color:#716c61; font-weight:600; }
    @media (max-width: 960px) {
      .draft-grid { grid-template-columns: 1fr; }
      .draft-link { grid-template-columns: 1fr; }
      .draft-thumb .ph { min-height: 180px; }
    }
  </style>
</head>
<body>
<nav class=\"main-nav medium-nav\">
  <div class=\"nav-left site-brand\">
    <a class=\"brand-logo-link\" href=\"/\"><img class=\"brand-logo\" src=\"/media/images/logo.png\" alt=\"Self dot send\"></a>
    <div class=\"brand-copy\"><a class=\"brand-title\" href=\"/\">Selfd<span class=\"brand-o\">o</span>tsend<sup class=\"draft-badge\">Draft</sup></a><div class=\"brand-tagline\">Message passing is just a procedure call.</div></div>
  </div>
  <div class=\"nav-right\">
    <a class=\"draft-nav-link\" href=\"/drafts/\">Drafts</a><a class=\"draft-nav-link\" href=\"/editor\">Write</a><a class=\"draft-nav-link\" href=\"/\">Published</a>
  </div>
</nav>
<main>
  <h1>Draft Preview</h1>
  <p class=\"lead\">Browse and edit drafts before publishing.</p>
  %s
</main>
</body>
</html>\n" body)))

(defun sds/main ()
  (let* ((blog-dir (file-name-as-directory (expand-file-name (car command-line-args-left))))
         (drafts-dir (expand-file-name "drafts" blog-dir))
         (public-drafts-dir (expand-file-name "public/drafts" blog-dir))
         (items '()))
    (unless blog-dir
      (error "Usage: emacs --batch -l scripts/generate_draft_preview.el <BLOG_DIR>"))
    (when (file-directory-p public-drafts-dir)
      (delete-directory public-drafts-dir t))
    (make-directory public-drafts-dir t)
    (when (file-directory-p drafts-dir)
      (dolist (draft (sort (directory-files drafts-dir t "\\.org\\'") #'string<))
        (let* ((parsed (sds/parse-org draft))
               (title (plist-get parsed :title))
               (slug (sds/slugify (file-name-base draft)))
               (date (plist-get parsed :date))
               (tags (or (plist-get parsed :tags) '("untagged")))
               (body (plist-get parsed :body))
               (description (plist-get parsed :description))
               (excerpt (sds/excerpt description body))
               (image (plist-get parsed :image))
               (html-body (plist-get parsed :html-body))
               (read-mins (sds/estimate-read-mins body))
               (item (list :slug slug
                           :title title
                           :date date
                           :tags tags
                           :body body
                           :html-body html-body
                           :draft-path (concat "drafts/" (file-name-nondirectory draft))
                           :excerpt excerpt
                           :read-mins read-mins
                           :image image))
               (out-dir (expand-file-name slug public-drafts-dir))
               (out-file (expand-file-name "index.html" out-dir)))
          (make-directory out-dir t)
          (with-temp-file out-file
            (insert (sds/detail-page item)))
          (push item items))))
    (with-temp-file (expand-file-name "index.html" public-drafts-dir)
      (insert (sds/index-page (sds/sort-items-by-date-desc items))))
    (princ (format "Generated draft preview pages: %d\n" (length items)))))

(sds/main)
