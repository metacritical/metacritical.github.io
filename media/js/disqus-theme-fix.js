// Disqus comment box visibility fix.
//
// The Disqus composer renders inside a cross-origin iframe. When Disqus
// picks its dark theme, the text is white on a transparent body —
// invisible on a light page. Parent CSS/JS cannot reach inside the iframe.
//
// Fix: CSS filter: invert(1) hue-rotate(180deg) on the iframe element
// inverts the rendered bitmap — white text becomes black, transparent
// areas stay transparent so our page background shows through.
//
// This JS ensures the filter is applied even if Disqus injects the
// iframe dynamically (MutationObserver re-applies on iframe insertion).
(function () {
  function applyFilter() {
    document.querySelectorAll('#disqus_thread iframe').forEach(function (iframe) {
      if (/disqus/i.test(iframe.src)) {
        iframe.style.filter = 'invert(1) hue-rotate(180deg)';
      }
    });
  }
  applyFilter();
  var observer = new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      var added = mutations[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        if (added[j].nodeName === 'IFRAME' || (added[j].querySelector && added[j].querySelector('iframe'))) {
          applyFilter();
          return;
        }
      }
    }
  });
  observer.observe(document.getElementById('disqus_thread') || document.body, {
    childList: true,
    subtree: true,
  });
  window.addEventListener('load', applyFilter);
})();
