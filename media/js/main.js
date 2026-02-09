(function () {
  function isRetina() {
    var mq = "(-webkit-min-device-pixel-ratio: 1.5), (min-resolution: 1.5dppx)";
    return (window.devicePixelRatio && window.devicePixelRatio > 1) ||
      (window.matchMedia && window.matchMedia(mq).matches);
  }

  function applyRetinaImages() {
    if (!isRetina()) return;
    var imgs = document.querySelectorAll("img.2x");
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

  document.addEventListener("DOMContentLoaded", function () {
    applyRetinaImages();
  });
})();
