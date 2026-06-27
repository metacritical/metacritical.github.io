(function () {
  function clapApiUrl() {
    return (window.SELF_DOTSEND_CLAP_API || "/api/clap").replace(/\/$/, "");
  }

  function getPageSlug() {
    var path = window.location.pathname.replace(/\/$/, "");
    var parts = path.split("/").filter(Boolean);
    if (!parts.length) return "home";
    return parts.join("-");
  }

  function formatCount(n) {
    if (n >= 1e6) return (n / 1e6).toFixed(1).replace(/\.0$/, "") + "M";
    if (n >= 1e3) return (n / 1e3).toFixed(1).replace(/\.0$/, "") + "k";
    return String(n);
  }

  function ensureClapStyles() {
    if (document.getElementById("selfdotsend-clap-styles")) return;
    var style = document.createElement("style");
    style.id = "selfdotsend-clap-styles";
    style.textContent =
      ".story-clap-btn.is-clapped { filter: drop-shadow(0 0 6px rgba(54,201,199,.65)); transform: scale(1.08); transition: transform .15s ease; }";
    document.head.appendChild(style);
  }

  function initClaps() {
    var rails = document.querySelectorAll(".story-rail");
    if (!rails.length) return;
    var slug = getPageSlug();
    var url = clapApiUrl();
    ensureClapStyles();

    rails.forEach(function (rail) {
      var btn = rail.querySelector(".story-clap-btn");
      var countEl = rail.querySelector(".story-clap-count");
      if (!btn || !countEl) return;

      function setCount(n) {
        countEl.textContent = formatCount(n);
      }

      fetch(url + "?slug=" + encodeURIComponent(slug), { method: "GET" })
        .then(function (r) { return r.json(); })
        .then(function (data) { setCount(data.count || 0); })
        .catch(function () { setCount(0); });

      btn.addEventListener("click", function () {
        btn.disabled = true;
        fetch(url, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ slug: slug })
        })
          .then(function (r) { return r.json(); })
          .then(function (data) {
            setCount(data.count || 0);
            btn.classList.add("is-clapped");
          })
          .catch(function () {})
          .finally(function () { btn.disabled = false; });
      });
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initClaps);
  } else {
    initClaps();
  }
})();
