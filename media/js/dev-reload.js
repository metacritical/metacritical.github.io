(function() {
  var lastId = null;
  var stopped = false;
  function check() {
    if (stopped) return;
    fetch('/dev-build-id.txt', { cache: 'no-store' })
      .then(function(r) {
        if (!r.ok) { stopped = true; return ''; }
        return r.text();
      })
      .then(function(id) {
        id = id.trim();
        if (!id) return;
        if (lastId !== null && id !== lastId) {
          location.reload();
        }
        lastId = id;
      })
      .catch(function() { stopped = true; });
  }
  setInterval(check, 2000);
  check();
})();
