(function () {
  function clapApiUrl() {
    return (window.SELF_DOTSEND_CLAP_API || "https://selfdotsend-reactions.pankajdoharey.workers.dev/api/clap").replace(/\/$/, "");
  }

  function getPageSlug() {
    var path = window.location.pathname.replace(/\/$/, "");
    var parts = path.split("/").filter(Boolean);
    if (!parts.length) return "home";
    return parts.join("-");
  }

  function getVisitorId() {
    var key = "sds_visitor_id";
    var id = localStorage.getItem(key);
    if (id) return id;
    if (window.crypto && window.crypto.randomUUID) {
      id = window.crypto.randomUUID();
    } else {
      id = "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, function (c) {
        var r = (Math.random() * 16) | 0;
        var v = c === "x" ? r : (r & 0x3) | 0x8;
        return v.toString(16);
      });
    }
    localStorage.setItem(key, id);
    return id;
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
      ".story-clap-btn.is-clapped { filter: drop-shadow(0 0 6px rgba(54,201,199,.65)); transform: scale(1.08); transition: transform .15s ease; }" +
      ".story-clap-btn.is-clap-error { animation: clap-shake .4s ease; }" +
      "@keyframes clap-shake { 0%,100%{transform:translateX(0)} 25%{transform:translateX(-3px)} 75%{transform:translateX(3px)} }";
    document.head.appendChild(style);
  }

  function initClaps() {
    var rails = document.querySelectorAll(".story-rail");
    if (!rails.length) return;
    var slug = getPageSlug();
    var url = clapApiUrl();
    var visitorId = getVisitorId();
    ensureClapStyles();

    rails.forEach(function (rail) {
      var btn = rail.querySelector(".story-clap-btn");
      var countEl = rail.querySelector(".story-clap-count");
      if (!btn || !countEl) return;

      function setCount(n) {
        countEl.textContent = formatCount(n);
      }

      function setClapped(clapped) {
        if (clapped) {
          btn.classList.add("is-clapped");
        } else {
          btn.classList.remove("is-clapped");
        }
      }

      fetch(url + "?slug=" + encodeURIComponent(slug) + "&visitor=" + encodeURIComponent(visitorId), { method: "GET" })
        .then(function (r) { return r.json(); })
        .then(function (data) {
          setCount(data.count || 0);
          setClapped(!!data.clapped);
        })
        .catch(function () { setCount(0); });

      btn.addEventListener("click", function () {
        if (btn.classList.contains("is-clapped")) return;
        btn.disabled = true;
        fetch(url, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ slug: slug, visitor_id: visitorId })
        })
          .then(function (r) { return r.json(); })
          .then(function (data) {
            setCount(data.count || 0);
            setClapped(!!data.clapped);
          })
          .catch(function () {
            btn.classList.add("is-clap-error");
            setTimeout(function () { btn.classList.remove("is-clap-error"); }, 1500);
          })
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
