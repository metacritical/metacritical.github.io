(function () {
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
  });
})();
