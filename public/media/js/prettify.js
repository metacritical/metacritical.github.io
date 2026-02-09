(function () {
  var GO_KEYWORDS = /\b(?:break|case|chan|const|continue|default|defer|else|fallthrough|for|func|go|goto|if|import|interface|map|package|range|return|select|struct|switch|type|var)\b/g;
  var SH_KEYWORDS = /\b(?:if|then|else|elif|fi|for|in|do|done|case|esac|while|until|function|select|time)\b/g;

  function escapeHtml(text) {
    return text
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  }

  function stash(text, pattern, className, bank) {
    return text.replace(pattern, function (m) {
      var token = "@@HL" + bank.length + "@@";
      bank.push('<span class="' + className + '">' + m + "</span>");
      return token;
    });
  }

  function restore(text, bank) {
    return text.replace(/@@HL(\d+)@@/g, function (_, idx) {
      return bank[Number(idx)] || "";
    });
  }

  function langFromBlock(block) {
    var lang = "";
    Array.prototype.forEach.call(block.classList, function (cls) {
      if (cls.indexOf("src-") === 0) lang = cls.slice(4).toLowerCase();
    });
    return lang;
  }

  function highlightGo(raw) {
    var bank = [];
    var text = escapeHtml(raw);
    text = stash(text, /\/\/[^\n]*/g, "org-comment", bank);
    text = stash(text, /`[^`]*`/g, "org-string", bank);
    text = stash(text, /"(?:\\.|[^"\\])*"/g, "org-string", bank);
    text = text.replace(GO_KEYWORDS, '<span class="org-keyword">$&</span>');
    text = text.replace(/\b\d+(?:\.\d+)?\b/g, '<span class="org-constant">$&</span>');
    return restore(text, bank);
  }

  function highlightShell(raw) {
    var bank = [];
    var text = escapeHtml(raw);
    text = stash(text, /(^|\s)#.*$/gm, "org-comment", bank);
    text = stash(text, /"(?:\\.|[^"\\])*"/g, "org-string", bank);
    text = stash(text, /'(?:\\.|[^'\\])*'/g, "org-string", bank);
    text = text.replace(SH_KEYWORDS, '<span class="org-keyword">$&</span>');
    text = text.replace(/\$[A-Za-z_][A-Za-z0-9_]*/g, '<span class="org-variable-name">$&</span>');
    return restore(text, bank);
  }

  function highlightBlock(block) {
    if (block.querySelector("span[class^='org-']")) return;
    var lang = langFromBlock(block);
    var raw = block.textContent || "";
    if (!raw.trim()) return;

    var html = "";
    if (lang === "go") {
      html = highlightGo(raw);
    } else if (lang === "bash" || lang === "sh" || lang === "shell" || lang === "zsh") {
      html = highlightShell(raw);
    } else {
      return;
    }
    block.innerHTML = html;
  }

  document.addEventListener("DOMContentLoaded", function () {
    var blocks = document.querySelectorAll("pre.src");
    blocks.forEach(highlightBlock);
  });
})();
