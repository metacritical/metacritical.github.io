# AGENTS.md

## Anti-Regression Rules

**Before modifying any file, read the FULL file** — not just the section you
plan to change. Partial reads cause regressions because you miss existing
functionality that your edit will overwrite or disconnect.

**Never recreate functionality that already exists.** Before writing new code
for a feature (tag management, toolbar, save mechanism, image handling, etc.),
grep the codebase for existing implementations. If it exists, wire it up — do
not rewrite it.

**Before saving changes, verify the save path is clean.** When auto-save or
manual save writes org files from the editor's HTML→Org conversion, ensure
non-content elements (series nav CSS, toolbars, metadata rows, scripts) are
excluded from the editable body. If they leak into the body, they get written
to the org file as garbage text.

**After each change, verify these still work:**
1. Code blocks: syntax highlighting, copy button, dark/light toggle
2. Auto-save: content AND tags persist to the org file
3. Editor toolbars: bubble, slash menu, plus button, image toolbar
4. Series nav: renders correctly (not dumped as raw text)
5. No garbage CSS or HTML in org source files after save

**When something breaks, git checkout the corrupted file from HEAD before
rebuilding.** Auto-save can corrupt org files. Always restore from git first.

---

## Dev Server Rules

**NEVER kill or restart the dev server.** Doorman is running and watches for
file changes automatically. It rebuilds and reloads on its own. If you need to
verify changes, just wait for Doorman to rebuild — do not run `kill`, `lsof`,
or `python -m http.server` to restart anything. If the server is already
running on port 8080, leave it alone.

---

## Build & Deploy

**Always use the scripts. Never run manual git/rsync commands to deploy.**

### publish.sh
Builds the site from source (org files, themes, media) into `public/`.
- Renders ditaa diagrams
- Runs AOG (org-mode static site generator)
- Copies media files to `public/media/`
- Injects script tags (claps.js, disqus-theme-fix.js) and normalizes URLs
- Use standalone for local preview: `./publish.sh` then `./scripts/dev-web.sh`

### deploy.sh
Full deploy pipeline. Calls `publish.sh` internally, then deploys to master.
- Must be run from the `source` branch (it will error otherwise)
- Commits `public/` on source, then switches to master
- Uses only git commands (no rsync): `git rm`, `git checkout source -- public/`, flatten to root
- Commits and pushes to `master`, then switches back to `source`
- Run: `./deploy.sh`

### Workflow
1. Make changes on `source` branch
2. `git add -A && git commit -m "..." && git push origin source`
3. `./deploy.sh` (builds + deploys in one step)

### Local preview (no deploy)
- `./publish.sh` — rebuilds `public/`
- `./scripts/dev-web.sh` — serves `public/` on port 8080
- `./scripts/dev-watch.sh` — auto-rebuilds on file changes (Doorman)

### Key gotchas
- `public/` is tracked on `source` but absent on `master` — deploy.sh uses `git checkout source -- public/` to transfer files between branches
- deploy.sh runs `git clean -fd` before checkout to remove stale untracked files from prior failed deploys
- AOG overwrites `public/media/css/` with its own CSS — site-specific CSS overrides are handled in JS, not CSS injection
- Disqus filter (`invert(1) hue-rotate(180deg)`) is applied entirely from `media/js/disqus-theme-fix.js`, which also attempts to inject a counter-filter for images inside the cross-origin iframe

---

## Core Rules

The agent must follow these rules for every commit:

1. Write commit titles in title case.
2. Capitalize the first letter of every major word in the title.
3. Do not use lowercase-only commit prefixes such as `feat:`, `fix:`, `chore:`, or `wip:`.
4. Prefer full category words such as `Feature`, `Fix`, `Refactor`, `Cleanup`, and `Docs`.
5. Add an emoji prefix that matches the nature of the change.
6. Keep the title concise, but not cryptic.
7. Use a structured body for any non-trivial commit.
8. Describe what changed, where it changed, why it changed, and what impact it has.
9. Mention ticket IDs when a task is tied to a ticket, issue, bug report, feature request, or milestone.
10. Do not group unrelated work into a single commit.
11. Avoid filler words and generic wording such as "stuff," "things," "misc," "minor," or "updates."
12. Avoid weak phrases like "fix issue," "update files," "change some code," or "small cleanup."
13. If the change is visual, mention the affected asset, diagram, component, or screen.
14. If the change restores prior behavior, say `Restore` or `Revert`, not just `Fix`.
15. If assets are regenerated, state what triggered the regeneration.

## Icon And Emoji Rules

**For UI elements** (toolbars, buttons, menus, editor controls):
- Always prefer inline SVG icons over emojis or Unicode symbols.
- Use Feather-style SVGs (`viewBox="0 0 24 24"`, `stroke="currentColor"`,
  `stroke-width="2"`) to match the existing icon set in the slash menu.
- Simple text labels (e.g., `B`, `I`, `L`, `C`, `R`) are acceptable and do
  not need SVG replacement.
- Never use emoji characters (e.g., 💾, 🖼) for toolbar or button icons.

**For commit messages**:
- Always prefer emoji prefixes (see the Approved Categories table above).
- Commit message emojis are the only place emojis should be used.

---

## Commit Title Format

Use this exact format:

`<emoji> <Category>: <Action-Oriented Title> (<optional-scope>)`

### Valid examples

- `✨ Feature: Introduce Prism Syntax Highlighting (Editor)`
- `🐛 Fix: Restore Original Gap Buffer Diagram (Assets)`
- `♻️ Refactor: Simplify Toolbar State Management (Draft Editor)`
- `🧹 Cleanup: Remove Emacs Lockfile From Tracking (Repo Hygiene)`
- `🔁 Revert: Restore cGRE Gap Rendering In Ditaa Diagrams (Docs)`
- `🔄 Sync: Regenerate Public Assets After SVG Size Update (Build Output)`

### Invalid examples

- `feat: add syntax highlighting`
- `fix: stuff`
- `chore: updates`
- `wip: editor work`
- `fixup: restore image`
- `small cleanup`

---

## Approved Categories

Use one primary category per commit title.

| Emoji | Category | When To Use |
|---|---|---|
| ✨ | Feature | A new user-facing or developer-facing capability |
| 🚀 | Enhancement | A meaningful improvement to an existing capability |
| 🐛 | Fix | A bug fix, regression fix, or correctness repair |
| ♻️ | Refactor | Internal structural improvement without intended behavior change |
| 🧹 | Cleanup | Removal of clutter, dead files, unused code, or repo hygiene work |
| ⚡ | Optimization | Performance, efficiency, or size improvements |
| 📝 | Docs | Documentation-only changes |
| 🎨 | UI | Visual polish, styling, layout, icons, or presentation changes |
| 🧱 | Build | Tooling, dependency, CI, generation pipeline, or build configuration changes |
| 🔁 | Revert | Explicit rollback of a previous change |
| 🔄 | Sync | Regenerated outputs or aligned generated artifacts |
| 🔒 | Security | Hardening, patching, or exposure reduction |
| 🧪 | Test | Tests added, improved, or corrected |
| 📦 | Assets | Static assets, images, icons, diagrams, or packaged resources |

### Category selection rules

- Use `Feature` instead of `Feat`.
- Use `Enhancement` when existing behavior becomes better, richer, or clearer.
- Use `Fix` when correcting wrong behavior, broken output, regressions, or invalid state.
- Use `Cleanup` only when there is no meaningful behavior change.
- Use `Refactor` only when the primary result is code structure improvement.
- Use `Revert` when intentionally undoing an earlier commit or behavior.
- Use `Sync` when generated files are updated to match a source change.
- Use `Assets` when static resources are the primary focus, especially diagrams and images.

---

## Title Writing Rules

The title must be short, informative, and action-led.

### Preferred verb patterns

Use stronger verbs such as:

- Introduce
- Implement
- Restore
- Correct
- Replace
- Eliminate
- Simplify
- Consolidate
- Refine
- Enhance
- Align
- Normalize
- Harden
- Streamline
- Expand
- Reduce
- Repair
- Regenerate
- Rebuild
- Remove
- Recover
- Reconcile

### Scope examples

Scopes should be short and concrete:

- `Editor`
- `Draft Editor`
- `Toolbar`
- `Assets`
- `Diagrams`
- `Docs`
- `Syntax Highlighting`
- `Persistence`
- `Repo Hygiene`
- `Build Output`
- `SVG Icons`
- `Image Toolbar`

### Preferred title patterns

- `✨ Feature: Introduce Inline Font Size Controls (Toolbar)`
- `🎨 UI: Enlarge Plus Menu SVG Icons For Better Visibility (Assets)`
- `🐛 Fix: Correct Published Edit Persistence (Draft Editor)`
- `📦 Assets: Restore Gapbuffer1 Diagram From Git History (Docs)`
- `🧹 Cleanup: Remove .DS_Store And Tighten Gitignore Rules (Repo Hygiene)`

---

## Commit Body Format

For every non-trivial commit, include a body using the exact sections below.

```text
<blank line>
Changes:
- ...
- ...

Impact:
- ...
- ...

Context:
- ...
- ...

Refs:
- ...
```

### Section rules

#### `Changes:`
State exactly what was modified.

Include:
- affected files, folders, modules, components, assets, or scripts
- the specific implementation action
- any restored, regenerated, removed, or replaced artifacts

#### `Impact:`
State why the change matters.

Include:
- user-facing effect
- developer-facing effect
- visual, behavioral, or maintenance effect
- reproducibility or consistency improvements

#### `Context:`
State the motivation, cause, or background.

Include:
- bug trigger
- regression source
- design rationale
- dependency between files or systems
- restoration reason if taken from git history

#### `Refs:`
Use only when relevant.

Include:
- ticket IDs
- issue IDs
- milestone names
- feature request tags
- bug report references

Examples:
- `Refs: ISSUE-142`
- `Refs: BUG-31, DOC-ASSET-07`
- `Refs: Editor Toolbar Polish`

---

## File-Level Reporting Expectations

The agent should mention affected files or asset groups in the body whenever practical.

### Good examples

- `Updated ditaa generation commands in docs/scripts/render-diagrams.sh to restore the original gap color output.`
- `Replaced public/images/gapbuffer.png with the historical version recovered from Git history.`
- `Removed tracked lockfile artifacts from the repository and reinforced ignore rules in .gitignore.`
- `Regenerated published assets in public/ after increasing SVG icon dimensions in the editor menu set.`

### Bad examples

- `Changed some files.`
- `Updated assets.`
- `Fixed editor stuff.`
- `Cleanup.`

The body should help a reviewer understand:
- which file or subsystem was touched
- how the change was performed
- whether generated outputs were refreshed
- whether any visual or behavioral result changed

---

## Ticket And Classification Rules

When work is tied to a ticket, issue, bug, or feature request, say so clearly.

### Use these labels inside the body when relevant

- `Ticket:` for project tracking items
- `Bug:` for bug reports or regressions
- `Feature:` for requested capability work
- `Regression:` for previously working behavior that broke
- `Follow-up:` for incremental polish after a prior merge

### Examples

```text
Context:
- Bug: Diagram rendering drifted after ditaa color changes.
- Regression: The original gap buffer image was replaced by a visually incorrect variant.

Refs:
- Ticket: DOC-219
- Bug: RENDER-41
```

If no formal ticket exists, the agent should still classify the purpose in plain language.

Example:

```text
Context:
- Feature: Adds code block language and theme controls to the draft editor.
```

---

## Preferred Vocabulary

The agent must avoid repetitive wording and should vary nouns, verbs, and descriptors naturally while staying precise.

### Better nouns

Use these instead of generic words like "thing" or "stuff":

- artifact
- asset
- diagram
- toolbar
- control
- workflow
- rendering
- output
- behavior
- state
- pipeline
- interaction
- layout
- visibility
- persistence
- surface
- variant
- regression
- alignment
- integration
- command
- rule
- history
- presentation
- consistency
- fidelity
- configuration

### Better verbs

Use these instead of repetitive verbs like "fix," "change," or "update" everywhere:

- restore
- recover
- rebuild
- refine
- reconcile
- align
- correct
- replace
- remove
- regenerate
- tighten
- standardize
- streamline
- polish
- repair
- recover
- reinforce
- introduce
- integrate
- expand
- normalize

### Better descriptors and adverbs

Use these when they genuinely add signal:

- original
- historical
- visual
- consistent
- reproducible
- explicit
- stable
- correct
- cleaner
- safer
- clearer
- deliberate
- targeted
- scoped
- precise
- maintainable
- reversible
- compatible
- improved
- polished

### Avoid overused low-signal wording

Avoid:
- this
- these
- thing
- stuff
- some
- various
- minor
- misc
- tweak
- tweakup
- fixup
- wip
- temp
- quick fix
- cleanup changes

If a pronoun would reduce clarity, replace it with the actual subject.

Bad:
- `This updates these files to fix it.`

Good:
- `Restores the original diagram asset and regenerates the published output to recover visual fidelity.`

---

## Style Rules For Agent-Written Commits

The tone should be concise, technical, and deliberate.

The agent must:
- write in active voice
- prefer concrete subjects over pronouns
- state intent before implementation details when possible
- avoid slang
- avoid placeholder terms
- avoid speculation
- avoid exaggerated language unless clearly warranted
- avoid stuffing multiple unrelated nouns into one title

The agent must not:
- use `WIP` in commit titles
- use `fixup` unless explicitly instructed during an interactive rebase workflow
- use `chore` as a default bucket for unclear work
- use lowercase category prefixes
- omit impact for meaningful changes
- hide important scope in the body while leaving the title vague

---

## Decision Rules For Choosing The Right Commit Type

### Use `Feature` when
- a new editor control is added
- a new menu is introduced
- a new integration is added
- a new capability becomes available to users or developers

### Use `Enhancement` when
- an existing interface becomes easier to use
- a control becomes clearer, larger, or more discoverable
- an existing workflow is improved without being entirely new

### Use `Fix` when
- behavior is broken
- output is wrong
- persistence fails
- rendering is incorrect
- a regression is being corrected

### Use `Refactor` when
- code structure changes but intended behavior stays the same
- state management is reorganized
- repeated logic is consolidated
- internals become easier to maintain

### Use `Cleanup` when
- lockfiles are removed
- ignored junk files are cleaned up
- dead code is deleted
- comments or stale artifacts are removed without behavior change

### Use `Sync` when
- generated output is refreshed after source changes
- public assets are rebuilt from updated source files
- generated docs or bundles are aligned with the latest code

### Use `Revert` when
- a prior change is intentionally undone
- a previous color scheme, asset, or behavior is restored by design

---

## Commit Examples Based On Common Cases

### 1. Asset restoration

```text
📦 Assets: Restore Original Gap Buffer Diagram (Docs)

Changes:
- Replaced `public/images/gapbuffer.png` with the historical asset recovered from Git history.
- Removed the altered variant that diverged from the intended documentation output.
- Standardized the related ditaa rendering command flags for consistent regeneration.

Impact:
- Restores the expected visual explanation of the gap buffer concept.
- Prevents diagram drift between source generation and published assets.

Context:
- Regression: Earlier rendering changes introduced an unintended visual mismatch.
- The restored asset better reflects the original documentation baseline.

Refs:
- Bug: DOC-ASSET-12
```

### 2. Repo hygiene

```text
🧹 Cleanup: Remove Emacs Lockfile From Tracking (Repo Hygiene)

Changes:
- Deleted the tracked Emacs lockfile artifact from the repository.
- Reinforced ignore rules to prevent editor-generated lockfiles from reappearing.

Impact:
- Keeps the repository free of machine-local noise.
- Reduces accidental diffs and review clutter.

Context:
- The tracked lockfile was not part of the source of truth and created avoidable churn.
```

### 3. UI improvement

```text
🎨 UI: Enlarge Plus Menu SVG Icons For Better Visibility (Editor)

Changes:
- Increased plus-menu SVG dimensions to 24px.
- Raised stroke width to improve readability on high-density displays.
- Regenerated dependent public assets after the icon source update.

Impact:
- Improves icon clarity and discoverability inside the editor surface.
- Produces more legible controls across desktop displays.

Context:
- The previous icon size appeared too small during real editor use.

Refs:
- Feature: Toolbar Visibility Polish
```

### 4. New capability

```text
✨ Feature: Introduce Prism Syntax Highlighting Menus (Draft Editor)

Changes:
- Integrated Prism.js highlighting for code blocks.
- Added theme and language selection menus to the editor toolbar.
- Connected code block rendering to the selected presentation options.

Impact:
- Gives authors direct control over code presentation.
- Improves readability and publishing quality for technical posts.

Context:
- Feature: Richer code block authoring was required for the visual draft editor.
```

### 5. Visual rollback

```text
🔁 Revert: Restore cGRE Gap Color In Ditaa Diagrams (Docs)

Changes:
- Replaced the modified gap color with the previous cGRE-based rendering value.
- Updated affected diagram generation inputs to match the restored palette.

Impact:
- Returns documentation visuals to the established look and feel.
- Reduces inconsistency across related rendered diagrams.

Context:
- The replacement color introduced an undesirable visual departure from the original style.
```

---

## Rules For Multi-Part Changes

If a commit touches multiple files, the body must explain the relationship between them.

Example:
- source file changed
- generated output refreshed
- ignore rules tightened
- restored asset copied from history

Do not merely list files. Explain the chain of effect.

Good:
- `Adjusted the source SVG set in the editor icon directory and regenerated the public asset bundle so published controls reflect the larger icon geometry.`

Bad:
- `Updated src/icons, public, and config files.`

---

## Atomicity Rules

The agent must keep commits atomic.

A single commit may include:
- one feature and its directly required assets
- one bug fix and the regenerated output it necessitates
- one cleanup pass in one clear area
- one revert covering one intentional rollback theme

A single commit must not include:
- unrelated editor polish plus repo cleanup plus docs rewrite
- a new feature plus a separate bug fix in another subsystem
- generated assets from unrelated sources mixed together

If the work spans unrelated concerns, split it into separate commits.

---

## Title Length And Body Density

Guidelines:
- Keep the title ideally between 45 and 80 characters.
- Keep body bullets specific and compact.
- Use 2 to 4 bullets in `Changes:` for most commits.
- Use 1 to 3 bullets in `Impact:`.
- Use 1 to 2 bullets in `Context:` unless more detail is truly needed.

Do not write essay-length commit bodies. Do write enough detail for a future maintainer to understand the reason behind the change.

---

## Fallback Behavior When Context Is Weak

If the agent has limited context, it must still produce a useful commit message.

Fallback rules:
1. Infer the most likely primary category from the diff.
2. Mention the concrete files or subsystems affected.
3. State observable impact only; do not invent motivations.
4. Use `Refactor`, `Cleanup`, or `Sync` when the diff supports those classifications more clearly than `Feature` or `Fix`.
5. If a root cause is uncertain, say `Context: Aligns generated output with source changes.` rather than guessing.

---

## Pre-Commit Self-Check

Before finalizing any commit message, the agent must verify:

- Is the title in title case?
- Is the first word capitalized?
- Is the category a full word, not shorthand?
- Is there an emoji prefix?
- Does the title describe the actual change?
- Does the body explain what changed?
- Does the body explain impact?
- Does the body explain context?
- Are file names or subsystems mentioned where useful?
- Is any ticket, bug, or feature reference included when relevant?
- Are vague words removed?
- Is the commit atomic?

If any answer is "no," revise the commit message before committing.

---

## Hard Prohibitions

The agent must never generate commit messages like:

- `feat: editor`
- `fix: some issue`
- `chore: updates`
- `wip: working on toolbar`
- `fixup: image`
- `cleanup stuff`
- `updated files`
- `misc changes`
- `small tweaks`
- `more changes`

The agent must never use `feat`, `fix`, `chore`, `fixup`, or `wip` as the visible public title style unless explicitly instructed by a human for a special workflow.

---

## Final Instruction

When the agent writes a commit message, the result should read like a concise engineering note, not like an autogenerated label. Every commit should help a reviewer answer four questions immediately:

1. What changed?
2. Where did it change?
3. Why was it necessary?
4. What effect does it have?

If the commit message cannot answer those four questions clearly, rewrite it before committing.
