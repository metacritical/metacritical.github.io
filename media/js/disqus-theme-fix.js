// Disqus comment box visibility fix.
//
// The Disqus composer renders inside a cross-origin iframe with dark-theme
// white text, invisible on our light page. We fix this with:
//
// 1. CSS filter: invert(1) hue-rotate(180deg) on the iframe element
//    (also set in style.css / injected by publish.sh into generated CSS).
// 2. JS: attempt to inject a counter-filter <style> into the iframe's
//    document so images get double-inverted (net zero = original appearance).
//    This will succeed for same-origin iframes and throw a SecurityError
//    for cross-origin ones, which we catch and ignore.
//
// The MutationObserver re-applies on dynamic iframe insertion by Disqus.
(function () {
  var FILTER = 'invert(1) hue-rotate(180deg)';

  function injectCounterFilter(iframe) {
    try {
      var doc = iframe.contentDocument || iframe.contentWindow.document;
      if (!doc) return false;
      if (doc.getElementById('disqus-img-unfilter')) return true;
      var style = doc.createElement('style');
      style.id = 'disqus-img-unfilter';
      style.textContent =
        'img, video, canvas, iframe, svg, .avatar, .user-avatar {' +
        '  filter: ' + FILTER + ' !important;' +
        '}';
      (doc.head || doc.documentElement || doc.body).appendChild(style);
      return true;
    } catch (e) {
      return false;
    }
  }

  function applyFix() {
    document.querySelectorAll('#disqus_thread iframe').forEach(function (iframe) {
      if (!/disqus/i.test(iframe.src || '')) return;
      iframe.style.filter = FILTER;
      if (injectCounterFilter(iframe)) return;
      iframe.addEventListener('load', function () {
        injectCounterFilter(iframe);
      });
    });
  }

  applyFix();

  var observer = new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      var added = mutations[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        if (added[j].nodeName === 'IFRAME' ||
            (added[j].querySelector && added[j].querySelector('iframe'))) {
          applyFix();
          return;
        }
      }
    }
  });
  observer.observe(document.getElementById('disqus_thread') || document.body, {
    childList: true,
    subtree: true,
  });
  window.addEventListener('load', applyFix);
})();
