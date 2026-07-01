/**
 * code-block-controls.js
 *
 * Enhances static <pre> code blocks on published articles and draft previews
 * with a language badge, copy button, and light/dark theme toggle.
 *
 * Self-contained: injects its own CSS. No dependencies (no Prism needed).
 * Works alongside existing htmlize/doom-monokai-pro highlighting.
 *
 * Dark mode: doom-monokai-pro colors (loaded via code-theme-monokai-pro.css)
 * Light mode: proper color overrides — NO CSS filter hacks.
 */
(function () {
  'use strict';

  var CSS = [
    '.org-src-container{position:relative;overflow:hidden}',
    '.org-src-container>pre.src{font-size:14px;line-height:1.6}',
    '.cb-bar{position:absolute;top:0;right:0;display:flex;align-items:center;gap:2px;z-index:5;',
    'opacity:0;transition:opacity .15s;padding:5px 8px}',
    '.org-src-container:hover .cb-bar{opacity:1}',
    '.cb-lang{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:10px;',
    'font-weight:600;text-transform:uppercase;letter-spacing:.05em;color:#9c968a;',
    'background:rgba(255,255,255,.07);padding:2px 8px;border-radius:3px}',
    '.cb-btn{background:rgba(255,255,255,.07);border:1px solid rgba(255,255,255,.1);',
    'color:#aaa;padding:2px 9px;border-radius:4px;cursor:pointer;',
    'font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:11px;',
    'line-height:1.4;transition:background .12s,color .12s,border-color .12s}',
    '.cb-btn:hover{background:rgba(255,255,255,.16);color:#fff}',
    '.cb-btn.copied{color:#36c9c7;border-color:#36c9c7}',
    '',
    '/* Light background toggle — keeps monokai syntax colors, only swaps bg */',
    'html[data-theme="medium"] .org-src-container.cb-light,',
    'html[data-theme="medium"] .org-src-container.cb-light pre,',
    'html[data-theme="medium"] .org-src-container.cb-light pre.src,',
    'html[data-theme="classic"] .org-src-container.cb-light,',
    'html[data-theme="classic"] .org-src-container.cb-light pre,',
    'html[data-theme="classic"] .org-src-container.cb-light pre.src,',
    '.org-src-container.cb-light,',
    '.org-src-container.cb-light pre,',
    '.org-src-container.cb-light pre.src {',
    '  background:#faf8f4 !important;',
    '  border-color:#e0dccf !important',
    '}',
    '.org-src-container.cb-light .cb-lang{color:#6f6a5e;background:#e8e3d4}',
    '.org-src-container.cb-light .cb-btn{color:#6f6a5e;background:#e8e3d4;border-color:#d4cec0}',
    '.org-src-container.cb-light .cb-btn:hover{background:#ddd6c6;color:#1f1f1b}',
    '.org-src-container.cb-light .cb-btn.copied{color:#0f7b4d;border-color:#0f7b4d}'
  ].join('\n');

  function injectCSS() {
    var tag = document.createElement('style');
    tag.textContent = CSS;
    document.head.appendChild(tag);
  }

  function getLang(pre) {
    var classes = pre.className.split(/\s+/);
    for (var i = 0; i < classes.length; i++) {
      var c = classes[i];
      if (c.indexOf('src-') === 0 && c.length > 4) {
        return c.substring(4);
      }
    }
    var m = pre.className.match(/language-([\w-]+)/);
    return m ? m[1] : '';
  }

  function enhance(container) {
    if (container.querySelector('.cb-bar')) return;

    var pre = container.querySelector('pre.src') || container.querySelector('pre');
    if (!pre) return;

    var lang = getLang(pre);

    var bar = document.createElement('div');
    bar.className = 'cb-bar';
    bar.setAttribute('contenteditable', 'false');

    if (lang) {
      var badge = document.createElement('span');
      badge.className = 'cb-lang';
      badge.textContent = lang;
      bar.appendChild(badge);
    }

    var copyBtn = document.createElement('button');
    copyBtn.className = 'cb-btn';
    copyBtn.type = 'button';
    copyBtn.textContent = 'Copy';
    copyBtn.setAttribute('aria-label', 'Copy code');
    copyBtn.addEventListener('click', function () {
      var text = pre.textContent;
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(function () {
          showCopied(copyBtn);
        });
      } else {
        var ta = document.createElement('textarea');
        ta.value = text;
        ta.style.position = 'fixed';
        ta.style.opacity = '0';
        document.body.appendChild(ta);
        ta.select();
        try { document.execCommand('copy'); showCopied(copyBtn); } catch (_) {}
        document.body.removeChild(ta);
      }
    });
    bar.appendChild(copyBtn);

    var themeBtn = document.createElement('button');
    themeBtn.className = 'cb-btn';
    themeBtn.type = 'button';
    themeBtn.textContent = '\u25D0';
    themeBtn.title = 'Toggle light/dark';
    themeBtn.setAttribute('aria-label', 'Toggle code theme');
    themeBtn.addEventListener('click', function () {
      container.classList.toggle('cb-light');
      themeBtn.textContent = container.classList.contains('cb-light') ? '\u25D1' : '\u25D0';
    });
    bar.appendChild(themeBtn);

    container.appendChild(bar);
  }

  function showCopied(btn) {
    var prev = btn.textContent;
    btn.textContent = '\u2713';
    btn.classList.add('copied');
    setTimeout(function () {
      btn.textContent = prev;
      btn.classList.remove('copied');
    }, 1400);
  }

  function wrapBarePre(pre) {
    if (pre.closest('.org-src-container')) return null;
    var wrapper = document.createElement('div');
    wrapper.className = 'org-src-container';
    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(pre);
    return wrapper;
  }

  function enhanceAll() {
    document.querySelectorAll('.org-src-container').forEach(enhance);
    document.querySelectorAll('pre.src').forEach(function (pre) {
      var w = wrapBarePre(pre);
      if (w) enhance(w);
    });
  }

  injectCSS();

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', enhanceAll);
  } else {
    enhanceAll();
  }
})();
