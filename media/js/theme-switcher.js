(function () {
  var THEME_KEY = 'sds-theme-v2';
  var root = document.documentElement;

  function apply(theme) {
    root.setAttribute('data-theme', theme);
    var btn = document.getElementById('theme-switch');
    if (btn) btn.textContent = theme === 'medium' ? 'Theme: Medium' : 'Theme: Classic';
  }

  var stored = localStorage.getItem(THEME_KEY);
  apply(stored === 'classic' ? 'classic' : 'medium');

  document.addEventListener('DOMContentLoaded', function () {
    var btn = document.getElementById('theme-switch');
    if (!btn) return;
    btn.addEventListener('click', function () {
      var next = root.getAttribute('data-theme') === 'medium' ? 'classic' : 'medium';
      localStorage.setItem(THEME_KEY, next);
      apply(next);
    });
    apply(root.getAttribute('data-theme') || 'medium');
  });
})();
