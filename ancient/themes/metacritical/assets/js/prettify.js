/**
 * @license
 * Copyright (C) 2006 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @fileoverview
 * Registers a language handler for various flavors of basic.
 *
 * To use, include prettify.js and this file in your HTML page.
 * Then put your code in an HTML tag like
 *      <pre class="prettyprint lang-basic"></pre>
 *
 * @author peter.henderson@gmail.com
 */

PR['registerLangHandler'](
    PR['createSimpleLexer'](
        [
         // Whitespace
         [PR['PR_PLAIN'],       /^[\t\n\r \xA0]+/, null, '\t\n\r \xA0'],
         // A line comment that starts with REM
         [PR['PR_COMMENT'],     /^(?:REM[^\n\r]*)/i, null],
        ],
        [
         [PR['PR_KEYWORD'], /^(?:DATA|LET|GO(?:TO|SUB)|RETURN|(?:END)?(?:FOR|IF|NEXT|SELECT|WHILE|FUNCTION|SUB))\b/i, null],
         [PR['PR_LITERAL'], /^(?:TRUE|FALSE|NULL)\b/i, null],
         // A number is a decimal real literal or in scientific notation.
         [PR['PR_LITERAL'], /^[+-]?(?:(?:(?:\.\d+|\d+\.\d+)(?:E[+-]?\d+)?)|(?:\d+E[+-]?\d+))/i, null],
         // An identifier
         [PR['PR_PLAIN'], /^[A-Z_$][A-Z0-9_$]*/i, null],
         // A run of punctuation
         [PR['PR_PUNCTUATION'], /^[^\w\t\n\r \xA0\"\\\'\`\-\+\*]+/, null],
         // String, character, or attribute value
         [PR['PR_STRING'], /^[\"\\\'\`][^\"\\\'\`]*(?:[\"\\\'\`]|$)/, null]
        ]),
    ['basic', 'cbm']);

// Code to initialize prettify syntax highlighting
function prettyPrint() {
  // Convert source blocks to prettify format
  var blocks = document.querySelectorAll('pre.src');
  blocks.forEach(function(block) {
    block.classList.remove('src');
    block.classList.add('prettyprint');
    // Convert language class
    Array.from(block.classList).forEach(function(cls) {
      if (cls.startsWith('src-')) {
        var lang = cls.substring(4);
        block.classList.remove(cls);
        block.classList.add('lang-' + lang);
      }
    });
  });

  // Initialize syntax highlighting
  var elements = document.getElementsByTagName('pre');
  for (var i = 0; i < elements.length; i++) {
    var element = elements[i];
    if (element.className.indexOf('prettyprint') >= 0) {
      // Apply syntax highlighting
      if (element.innerHTML) {
        element.innerHTML = PR.prettyPrintOne(element.innerHTML, null, false);
      }
    }
  }
}

// Initialize on page load
document.addEventListener('DOMContentLoaded', function() {
  prettyPrint();
});
