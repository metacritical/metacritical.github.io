(function () {
  function estimateReadMinutes(text) {
    var words = (text || "")
      .replace(/\s+/g, " ")
      .trim()
      .split(" ")
      .filter(Boolean).length;
    return Math.max(1, Math.ceil(words / 220));
  }

  function appendReadTime(base, minutes) {
    if (!base || !minutes) return;
    var next = base + " \u00b7 " + minutes + " min read";
    return next;
  }

  function getPostTextForReadTime() {
    var post = document.querySelector(".post");
    if (!post) return "";
    var clone = post.cloneNode(true);
    Array.prototype.forEach.call(clone.querySelectorAll("pre, code, .org-src-container, script, style"), function (el) {
      el.remove();
    });
    return (clone.textContent || "").replace(/\s+/g, " ").trim();
  }

  function applyStoryReadTime() {
    var authorDateEl = document.querySelector(".post-author-date");
    if (!authorDateEl) return;
    var text = getPostTextForReadTime();
    if (!text) return;
    var mins = estimateReadMinutes(text);
    var baseDate = (authorDateEl.textContent || "").trim();
    authorDateEl.textContent = appendReadTime(baseDate || "", mins);
  }

  function enhanceHomeReadTimeCards() {
    var cards = document.querySelectorAll(".medium-post-card");
    if (!cards.length) return;

    cards.forEach(function (card) {
      var link = card.querySelector(".medium-post-main");
      var meta = card.querySelector(".medium-post-meta");
      if (!link || !meta) return;
      var href = link.getAttribute("href");
      if (!href) return;
      var key = "readtime:" + href;

      var cached = sessionStorage.getItem(key);
      if (cached) {
        meta.textContent = appendReadTime((meta.textContent || "").trim(), parseInt(cached, 10));
        return;
      }

      fetch(href, { credentials: "same-origin" })
        .then(function (res) {
          if (!res.ok) throw new Error("bad response");
          return res.text();
        })
        .then(function (html) {
          var doc = new DOMParser().parseFromString(html, "text/html");
          var post = doc.querySelector(".post");
          if (!post) return;
          Array.prototype.forEach.call(post.querySelectorAll("pre, code, .org-src-container, script, style"), function (el) {
            el.remove();
          });
          var mins = estimateReadMinutes((post.textContent || "").replace(/\s+/g, " ").trim());
          sessionStorage.setItem(key, String(mins));
          meta.textContent = appendReadTime((meta.textContent || "").trim(), mins);
        })
        .catch(function () {});
    });
  }

  function setupStoryActions() {
    var rail = document.querySelector(".story-rail");
    if (!rail) return;

    var clapBtn = rail.querySelector(".story-clap-btn");
    var clapCount = rail.querySelector(".story-clap-count");
    var xLink = rail.querySelector(".story-share-x");
    var fbLink = rail.querySelector(".story-share-fb");
    var lnLink = rail.querySelector(".story-share-ln");
    var url = window.location.href;
    var title = (document.querySelector(".post .title") || {}).textContent || document.title;
    var key = "story_claps:" + window.location.pathname;

    var count = parseInt(localStorage.getItem(key) || "0", 10);
    if (!Number.isFinite(count)) count = 0;
    clapCount.textContent = String(count);

    clapBtn.addEventListener("click", function () {
      count += 1;
      localStorage.setItem(key, String(count));
      clapCount.textContent = String(count);
    });

    var eUrl = encodeURIComponent(url);
    var eTitle = encodeURIComponent(title);
    xLink.href = "https://twitter.com/intent/tweet?url=" + eUrl + "&text=" + eTitle;
    fbLink.href = "https://www.facebook.com/sharer/sharer.php?u=" + eUrl;
    lnLink.href = "https://www.linkedin.com/sharing/share-offsite/?url=" + eUrl;
  }

  function isRetina() {
    var mq = "(-webkit-min-device-pixel-ratio: 1.5), (min-resolution: 1.5dppx)";
    return (window.devicePixelRatio && window.devicePixelRatio > 1) ||
      (window.matchMedia && window.matchMedia(mq).matches);
  }

  function applyRetinaImages() {
    if (!isRetina()) return;
    // Class names starting with digits must be escaped in selectors.
    // Using attribute selector avoids selector syntax errors in browsers.
    var imgs = document.querySelectorAll("img[class~='2x']");
    imgs.forEach(function (img) {
      if (img.dataset.retinaApplied === "1") return;
      var src = img.getAttribute("src") || "";
      if (!src || src.indexOf("@2x.") !== -1) {
        img.dataset.retinaApplied = "1";
        return;
      }
      var next = src
        .replace(/\.png(\?.*)?$/i, "@2x.png$1")
        .replace(/\.jpg(\?.*)?$/i, "@2x.jpg$1")
        .replace(/\.jpeg(\?.*)?$/i, "@2x.jpeg$1");
      if (next !== src) {
        img.addEventListener("error", function onErr() {
          img.removeEventListener("error", onErr);
          img.setAttribute("src", src);
        });
        img.setAttribute("src", next);
      }
      img.dataset.retinaApplied = "1";
    });
  }

  function normalizeLang(lang) {
    var map = {
      elisp: "lisp",
      "emacs-lisp": "lisp",
      sh: "bash",
      shell: "bash",
      zsh: "bash",
      js: "javascript",
      ts: "typescript"
    };
    return map[lang] || lang;
  }

  function applyPrettifyFallback() {
    if (!(window.PR && typeof window.PR.prettyPrint === "function")) return;

    var blocks = document.querySelectorAll("pre.src");
    blocks.forEach(function (block) {
      // If Org htmlize already injected semantic spans, keep that output.
      if (block.querySelector("span[class^='org-']")) return;

      block.classList.add("prettyprint");

      var lang = "";
      Array.prototype.forEach.call(block.classList, function (cls) {
        if (cls.indexOf("src-") === 0) {
          lang = cls.slice(4);
        }
      });
      lang = normalizeLang((lang || "").replace(/^:+/, ""));
      if (lang) block.classList.add("lang-" + lang);
    });

    window.PR.prettyPrint();
  }

  document.addEventListener("DOMContentLoaded", function () {
    applyRetinaImages();
    applyPrettifyFallback();

    // The homepage body is authored in Org export HTML; drop the synthetic
    // "Home" title injected by the generic post template.
    var path = window.location.pathname || "/";
    if (path === "/" || path === "/index.html") {
      var topTitle = document.querySelector(".post > h1.title");
      if (topTitle && (topTitle.textContent || "").trim().toLowerCase() === "home") {
        topTitle.remove();
      }
    }

    // Clean post metadata presentation.
    var meta = document.querySelector(".post-meta");
    if (meta) {
      var dateEl = meta.querySelector(".post-info--date");
      var modEl = meta.querySelector(".post-info--mod-date");
      var tagsEl = meta.querySelector(".post-info--tags");
      var authorDateEl = document.querySelector(".post-author-date");

      if (dateEl && authorDateEl) {
        var authoredDate = (dateEl.textContent || "").trim();
        if (authoredDate) {
          authorDateEl.textContent = authoredDate;
        } else {
          authorDateEl.remove();
        }
      }

      if (dateEl && modEl) {
        var d1 = (dateEl.textContent || "").trim();
        var d2 = (modEl.textContent || "").trim();
        if (d1 && d2 && d1 === d2) {
          modEl.remove();
        }
      }

      if (tagsEl) {
        var t = (tagsEl.textContent || "").replace(/\s+/g, " ").trim();
        if (!t || t.toLowerCase() === "n/a") {
          tagsEl.remove();
        }
      }
    }

    applyStoryReadTime();
    enhanceHomeReadTimeCards();
    setupStoryActions();
  });
})();
