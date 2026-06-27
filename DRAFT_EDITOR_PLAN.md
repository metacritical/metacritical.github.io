# Draft Editor Refinement Plan

## 1. Toolbar architecture
- [x] 1.1 Define exclusive toolbar model (text selection, block selection, object selection, multi-select)
- [x] 1.2 Ensure only one toolbar is visible at a time in both editors
- [x] 1.3 Hide object/block toolbars when text is selected
- [x] 1.4 Deselect objects when clicking empty space outside object/toolbar

## 2. Block toolbar cleanup
- [x] 2.1 Remove alignment buttons from block toolbar in inline editor
- [x] 2.2 Remove font-size select from block toolbar in inline editor
- [x] 2.3 Remove alignment buttons from block toolbar in standalone editor
- [x] 2.4 Remove font-size select from block toolbar in standalone editor
- [x] 2.5 Keep only move-up, move-down, delete in block toolbar

## 3. Inline font-size feature
- [x] 3.1 Add A+ / A- buttons to inline text toolbar in inline editor
- [x] 3.2 Add A+ / A- buttons to inline text toolbar in standalone editor
- [x] 3.3 Implement font-size increase/decrease as inline style on selection
- [x] 3.4 Ensure new paragraphs/headings keep default sizes
- [x] 3.5 Export/import inline font-size to Org (`@@html:<span style="font-size:...">...@@`)

## 4. Object toolbar improvements
- [x] 4.1 Widen image toolbar to keep all buttons on one line
- [x] 4.2 Verify Object fit dropdown values apply real CSS classes
- [x] 4.3 Verify Object position dropdown values apply real CSS classes
- [x] 4.4 Verify Filter dropdown values apply real CSS filter classes
- [x] 4.5 Apply same fixes to standalone editor image toolbar

## 5. Object deselection
- [x] 5.1 Add document-level click handler to deselect images/tables/equations when clicking outside
- [x] 5.2 Ensure object toolbars hide on outside click in inline editor
- [x] 5.3 Ensure object toolbars hide on outside click in standalone editor

## 6. Verification
- [x] 6.1 `node --check media/js/draft-editor.js`
- [x] 6.2 `emacs --batch -f batch-byte-compile scripts/generate_draft_preview.el`
- [x] 6.3 `LOCAL_DEV=1 ./publish.sh`
- [x] 6.4 Extract inline script and `node --check`
- [x] 6.5 Commit changes

## 7. Code block syntax highlighting and themes
- [x] 7.1 Add Prism.js highlighter to standalone editor page
- [x] 7.2 Add Prism.js highlighter to inline editor detail page
- [x] 7.3 Replace clear half-moon button with theme/language menu in standalone editor
- [x] 7.4 Replace clear half-moon button with theme/language menu in inline editor
- [x] 7.5 Re-highlight code blocks on insert, language change, theme change, and after typing
- [x] 7.6 Provide light, dark, and Monokai themes
- [x] 7.7 Ensure Org export/import preserves language and theme
- [x] 7.8 Verify all build checks and commit
