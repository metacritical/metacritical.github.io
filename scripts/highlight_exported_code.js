#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const GO_KEYWORDS =
  /\b(?:break|case|chan|const|continue|default|defer|else|fallthrough|for|func|go|goto|if|import|interface|map|package|range|return|select|struct|switch|type|var)\b/g;
const SH_KEYWORDS =
  /\b(?:if|then|else|elif|fi|for|in|do|done|case|esac|while|until|function|select|time)\b/g;
const SCHEME_KEYWORDS =
  /\b(?:define|lambda|let|let\*|letrec|if|cond|case|when|unless|begin|do|and|or|not|set!|quote|quasiquote|unquote|define-record-type|define-syntax|syntax-rules|let-syntax|letrec-syntax|module|import|export|include|library|cond-expand|else|arrow|=>|delay|force|parameterize|guard|raise|with-exception-handler|dynamic-wind|call\/cc|call-with-current-continuation)\b/g;

function stash(text, pattern, className, bank) {
  return text.replace(pattern, (m) => {
    const token = `@@HL${bank.length}@@`;
    bank.push(`<span class="${className}">${m}</span>`);
    return token;
  });
}

function restore(text, bank) {
  return text.replace(/@@HL(\d+)@@/g, (_, idx) => bank[Number(idx)] || "");
}

function highlightGo(raw) {
  const bank = [];
  let text = raw;
  text = stash(text, /\/\/[^\n]*/g, "org-comment", bank);
  text = stash(text, /`[^`]*`/g, "org-string", bank);
  text = stash(text, /"(?:\\.|[^"\\])*"/g, "org-string", bank);
  text = text.replace(GO_KEYWORDS, '<span class="org-keyword">$&</span>');
  text = text.replace(/\b\d+(?:\.\d+)?\b/g, '<span class="org-constant">$&</span>');
  return restore(text, bank);
}

function highlightShell(raw) {
  const bank = [];
  let text = raw;
  text = stash(text, /(^|\s)#.*$/gm, "org-comment", bank);
  text = stash(text, /"(?:\\.|[^"\\])*"/g, "org-string", bank);
  text = stash(text, /'(?:\\.|[^'\\])*'/g, "org-string", bank);
  text = text.replace(SH_KEYWORDS, '<span class="org-keyword">$&</span>');
  text = text.replace(/\$[A-Za-z_][A-Za-z0-9_]*/g, '<span class="org-variable-name">$&</span>');
  return restore(text, bank);
}

function highlightScheme(raw) {
  const bank = [];
  let text = raw;
  // Comments: ; to end of line
  text = stash(text, /;[^\n]*/g, "org-comment", bank);
  // Block comments: #| ... |#
  text = stash(text, /#\|[\s\S]*?\|#/g, "org-comment", bank);
  // Datum comments: #;
  text = stash(text, /#;(?!["\s])/g, "org-comment", bank);
  // Strings: "..."
  text = stash(text, /"(?:\\.|[^"\\])*"/g, "org-string", bank);
  // Characters: #\name (e.g. #\a, #\newline, #\space)
  text = stash(text, /#\\[A-Za-z0-9]+/g, "org-constant", bank);
  // Booleans and special: #t #f #\nul
  text = stash(text, /#(?:t|f|true|false)\b/g, "org-constant", bank);
  // Keywords
  text = text.replace(SCHEME_KEYWORDS, '<span class="org-keyword">$&</span>');
  // Numbers
  text = text.replace(/\b\d+(?:\.\d+)?\b/g, '<span class="org-constant">$&</span>');
  // Definitions: function names after define
  text = stash(text, /\((?:define|defun)\s+(?:\([^)]*\)|[^\s())]+)/g, "org-function-name", bank);
  return restore(text, bank);
}

function highlightByLang(lang, code) {
  if (code.includes('<span class="org-')) return code;
  if (lang === "go") return highlightGo(code);
  if (lang === "bash" || lang === "sh" || lang === "shell" || lang === "zsh") {
    return highlightShell(code);
  }
  if (lang === "scheme" || lang === "scm" || lang === "racket" || lang === "lisp" || lang === "elisp" || lang === "emacs-lisp" || lang === "cl") {
    return highlightScheme(code);
  }
  return code;
}

function processHtml(filePath) {
  let html = fs.readFileSync(filePath, "utf8");
  let changed = false;

  html = html.replace(
    /<pre class="src src-([a-z0-9-]+)">([\s\S]*?)<\/pre>/gi,
    (full, lang, code) => {
      const next = highlightByLang(String(lang).toLowerCase(), code);
      if (next !== code) {
        changed = true;
        return `<pre class="src src-${lang}">${next}</pre>`;
      }
      return full;
    }
  );

  if (changed) fs.writeFileSync(filePath, html, "utf8");
}

function walk(dir) {
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) walk(full);
    else if (entry.isFile() && entry.name.endsWith(".html")) processHtml(full);
  }
}

const root = process.argv[2];
if (!root) {
  console.error("Usage: highlight_exported_code.js <public-dir>");
  process.exit(1);
}

walk(root);
