(function () {
  var overlay;
  var openBtn;
  var closeBtn;
  var input;
  var results;
  var indexData = [];
  var loadPromise;
  var visibleMatches = [];
  var selectedIndex = -1;

  function asText(value) {
    return (value || "").toString();
  }

  function escapeHtml(value) {
    return asText(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function normalize(value) {
    return asText(value).toLowerCase().trim();
  }

  function tokenize(value) {
    return normalize(value)
      .split(/[^a-z0-9]+/g)
      .filter(Boolean);
  }

  function ensureLeadingSlash(url) {
    if (!url) return "/";
    return url.charAt(0) === "/" ? url : "/" + url;
  }

  function renderMessage(message) {
    visibleMatches = [];
    selectedIndex = -1;
    results.innerHTML = '<li class="site-search-empty">' + escapeHtml(message) + "</li>";
  }

  function setActive(index) {
    var items = results.querySelectorAll(".site-search-item");
    if (!items.length) return;

    if (index < 0) index = items.length - 1;
    if (index >= items.length) index = 0;
    selectedIndex = index;

    items.forEach(function (item, idx) {
      var active = idx === selectedIndex;
      item.classList.toggle("is-active", active);
      item.setAttribute("aria-selected", active ? "true" : "false");
      if (active) item.scrollIntoView({ block: "nearest" });
    });
  }

  function goToSelection() {
    if (selectedIndex < 0) return;
    var items = results.querySelectorAll(".site-search-item");
    var item = items[selectedIndex];
    if (!item) return;
    var link = item.querySelector("a.site-search-link");
    if (link && link.href) window.location.assign(link.href);
  }

  function renderResults(matches, query) {
    visibleMatches = matches.slice(0, 12);
    if (!visibleMatches.length) {
      renderMessage("No matching posts.");
      return;
    }

    var html = visibleMatches.map(function (item, idx) {
      var date = item.date ? '<span class="site-search-date">' + escapeHtml(item.date) + "</span>" : "";
      var excerpt = item.excerpt ? '<p class="site-search-excerpt">' + escapeHtml(item.excerpt) + "</p>" : "";
      return (
        '<li class="site-search-item' + (idx === 0 ? " is-active" : "") + '" aria-selected="' + (idx === 0 ? "true" : "false") + '">' +
        '<a href="' + escapeHtml(ensureLeadingSlash(item.url)) + '" class="site-search-link">' +
        '<div class="site-search-title">' + escapeHtml(item.title) + "</div>" +
        date +
        excerpt +
        "</a>" +
        "</li>"
      );
    }).join("");

    results.innerHTML = html;
    selectedIndex = 0;

    var items = results.querySelectorAll(".site-search-item");
    items.forEach(function (item, idx) {
      item.addEventListener("mouseenter", function () {
        setActive(idx);
      });
    });

    if (!query) setActive(0);
  }

  function scoreItem(item, terms) {
    var title = normalize(item.title);
    var excerpt = normalize(item.excerpt);
    var slug = normalize(item.url);
    var date = normalize(item.date);
    var score = 0;

    for (var i = 0; i < terms.length; i += 1) {
      var t = terms[i];
      if (title.indexOf(t) !== -1) score += 12;
      if (excerpt.indexOf(t) !== -1) score += 5;
      if (slug.indexOf(t) !== -1) score += 3;
      if (date.indexOf(t) !== -1) score += 2;
    }
    return score;
  }

  function runSearch(query) {
    var terms = tokenize(query);
    if (!terms.length) {
      renderResults(indexData.slice(0, 12), "");
      return;
    }

    var ranked = indexData
      .map(function (item) {
        return { item: item, score: scoreItem(item, terms) };
      })
      .filter(function (entry) {
        return entry.score > 0;
      })
      .sort(function (a, b) {
        return b.score - a.score;
      })
      .map(function (entry) {
        return entry.item;
      });

    renderResults(ranked, query);
  }

  function loadIndex() {
    if (loadPromise) return loadPromise;
    loadPromise = fetch("/search-index.json", { cache: "no-store" })
      .then(function (response) {
        if (!response.ok) throw new Error("search index unavailable");
        return response.json();
      })
      .then(function (data) {
        indexData = Array.isArray(data) ? data : [];
        return indexData;
      })
      .catch(function () {
        indexData = [];
        return indexData;
      });
    return loadPromise;
  }

  function openSearch() {
    overlay.hidden = false;
    document.body.classList.add("search-open");
    renderMessage("Loading posts...");
    loadIndex().then(function () {
      runSearch(input.value);
      input.focus();
      input.select();
    });
  }

  function closeSearch() {
    overlay.hidden = true;
    document.body.classList.remove("search-open");
  }

  function isTypingTarget(el) {
    if (!el) return false;
    var tag = (el.tagName || "").toLowerCase();
    return tag === "input" || tag === "textarea" || el.isContentEditable;
  }

  document.addEventListener("DOMContentLoaded", function () {
    overlay = document.getElementById("site-search-overlay");
    openBtn = document.getElementById("site-search-open");
    closeBtn = document.getElementById("site-search-close");
    input = document.getElementById("site-search-input");
    results = document.getElementById("site-search-results");

    if (!overlay || !openBtn || !closeBtn || !input || !results) return;

    openBtn.addEventListener("click", openSearch);
    closeBtn.addEventListener("click", closeSearch);
    overlay.addEventListener("click", function (event) {
      if (event.target === overlay) closeSearch();
    });
    input.addEventListener("input", function () {
      runSearch(input.value);
    });
    input.addEventListener("keydown", function (event) {
      if (event.key === "ArrowDown") {
        event.preventDefault();
        setActive(selectedIndex + 1);
        return;
      }
      if (event.key === "ArrowUp") {
        event.preventDefault();
        setActive(selectedIndex - 1);
        return;
      }
      if (event.key === "Enter") {
        event.preventDefault();
        goToSelection();
      }
    });
    results.addEventListener("click", function (event) {
      var link = event.target.closest("a.site-search-link");
      if (!link) return;
      event.preventDefault();
      window.location.assign(link.href);
    });

    document.addEventListener("keydown", function (event) {
      if (event.key === "Escape" && !overlay.hidden) {
        closeSearch();
        return;
      }
      if ((event.key === "/" && !isTypingTarget(event.target)) ||
          ((event.metaKey || event.ctrlKey) && (event.key === "k" || event.key === "K"))) {
        event.preventDefault();
        if (overlay.hidden) openSearch();
      }
    });
  });
})();
