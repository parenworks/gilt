# TODO — Features to Make Gilt the Ultimate Git CLI

## Priority Implementation Order

### Tier 1: High-Impact Missing Features

- [x] **1. Reflog Viewer**
  - What: A dedicated panel/view for browsing reflog entries, with the ability to checkout, diff, or reset to any reflog entry.
  - Why it matters: Every other major TUI has this (tig, lazygit, git-fuzzy, forgit). Gilt has undo/redo via reflog (z/Z) but no way to browse the reflog. This is critical for recovering from mistakes.
  - Effort: Medium — `git reflog` parsing + a new view mode.

- [x] **2. Grep View (`git grep`)**
  - What: Search file contents across the repo using `git grep`, with results shown in a navigable panel.
  - Why it matters: Tig has a dedicated grep view. Currently gilt's `/` only filters panel lists by name — it can't search file *contents*. This is a major workflow gap.
  - Effort: Medium — `git grep --line-number` parsing + results panel + jump-to-file.

- [x] **3. Tree/Blob View (Browse Files at Any Commit)**
  - What: Browse the directory tree of any commit/branch/tag, and view file contents (blobs) at that revision.
  - Why it matters: Tig has tree and blob views. Git-gui has a tree browser. Gilt can show commit *files* (changed files in a commit) but can't browse the full tree at an arbitrary revision.
  - Effort: Medium — `git ls-tree -r` parsing + tree navigation + `git show <ref>:<path>` for blob viewing.

- [x] **4. Multi-Commit Cherry-Pick (Copy/Paste)**
  - What: Select multiple commits with a range, "copy" them, then "paste" (cherry-pick) onto another branch.
  - Why it matters: Lazygit supports this with shift+C / shift+V. Gilt's cherry-pick is single-commit only (`C` on one commit).
  - Effort: Small — track a list of copied commits, then cherry-pick them in order.

- [x] **5. File Content View (Show File at Revision)**
  - What: View the full content of any file at any commit, not just its diff.
  - Why it matters: Essential for understanding what a file looked like at a point in time. Tig, gitui, and git-gui all have this.
  - Effort: Small — `git show <ref>:<path>` + pager.

### Tier 2: Medium-Impact Missing Features

- [x] **6. Line Range Tracing (`git log -L`)**
  - What: Trace the evolution of specific line ranges or functions across commits — "who changed this function and when?"
  - Why it matters: Gitk has this, and it's one of the most powerful history-analysis features in git. No TUI I found has it.
  - Effort: Medium — `git log -L<start>,<end>:<file>` parsing + specialized view.

- [x] **7. Blame Enhancements**
  - What gilt has: Basic blame with hash/author/date/line, and `Enter` to see commit details.
  - What's missing:
    - Navigate to parent commit — jump to the previous revision that touched the blamed line (tig, git-gui)
    - Copy detection (`-C -C -C`) — detect lines moved from other files (tig, git-gui)
    - Blame from specific commit — blame at a revision, not just HEAD (git-gui)
    - Jump to line number — start blame at a specific line (gitui, git-gui)
  - Effort: Small-Medium — add flags to `git blame` call + navigation.

- [x] **8. Diff Search / In-Diff Search**
  - What: Search within diff output with highlighting of matches.
  - Why it matters: Git-fuzzy's killer feature. Find specific changes across many diffs.
  - Effort: Medium — search through diff text + highlight matches.

- [x] **9. Commit Graph Improvements**
  - What gilt has: Basic graph toggle (`g`).
  - What's missing:
    - Color-coded branch lanes — visual graph with colored lines showing branch relationships (lazygit, gitk, giv)
    - Show all branches — graph across all branches, not just current
    - Topological ordering — `--topo-order` option (tig)
  - Effort: Medium-Hard — parsing `git log --graph` with lane tracking is non-trivial.

- [x] **10. Force Push Options**
  - What gilt has: Basic push with force option.
  - What's missing:
    - `--force-with-lease` — safer force push (lazygit has this as default)
    - Push to specific remote (not just origin)
    - Push with `--set-upstream`
  - Effort: Small — add options to push dialog.

- [x] **11. Pull with Rebase / Pull Fast-Forward Only**
  - What gilt has: Basic `git pull`.
  - What's missing:
    - `git pull --rebase` — pull with rebase instead of merge
    - `git pull --ff-only` — fast-forward only pull
  - Effort: Small — add pull options dialog.

- [x] **12. Clone / Init from UI**
  - What: Clone a repository or init a new one from within the TUI, not just from CLI.
  - Why it matters: Froggit and gittui support this. Gilt can only operate on existing repos.
  - Effort: Small-Medium — dialog for URL + destination, then `git clone`.

### Tier 3: Polish & Power-User Features

- [x] **13. Configurable Keybindings**
  - What gilt has: Custom *commands* via `commands.conf`, but no way to override built-in keybindings.
  - What's missing: Full keybinding configuration (like tig's `.tigrc`, lazygit's config, gitui's `key_config.ron`).
  - Effort: Medium — keybinding map + config parsing + context system.

- [x] **14. Command Palette**
  - What: `:` opens a searchable menu of all available actions.
  - Why it matters: Gittui and giv have this. Great for discoverability — no need to memorize keybindings.
  - Effort: Medium — enumerate all actions + fuzzy search + dispatch.

- [ ] **15. Syntax Highlighting in Diffs**
  - What: Color-code diff output based on file language syntax.
  - Why it matters: Gitui has this via syntect. Makes diffs much more readable.
  - Effort: Hard in Common Lisp — would need a syntax highlighting library or external tool integration (bat, etc.).

- [ ] **16. Split Diff View**
  - What: Show diff in split (side-by-side) mode instead of unified.
  - Why it matters: Gittui and gitkraft support this. Many users prefer side-by-side.
  - Effort: Medium — `git diff --word-diff=porcelain` or custom rendering.

- [ ] **17. Numstat in File List**
  - What: Show lines added/deleted per file in the files panel (e.g. `+12 -3`).
  - Why it matters: Lazygit has this. Quick visual indicator of change size.
  - Effort: Small — `git diff --numstat` parsing.

- [ ] **18. Bulk Branch Operations**
  - What: Select multiple branches and delete/merge them in bulk.
  - Why it matters: Lazygit supports bulk branch deletion.
  - Effort: Small-Medium — multi-select + batch operations.

- [ ] **19. Commit Message Templates / Prefixes**
  - What: Predefined commit message prefixes (e.g., `feat:`, `fix:`) configurable per branch pattern.
  - Why it matters: Lazygit has this. Useful for conventional commits.
  - Effort: Small — config + dialog enhancement.

- [ ] **20. Not-In-Repo Behavior**
  - What: When launched outside a git repo, offer to init, clone, or browse recent repos.
  - Why it matters: Froggit, lazygit, and gittui handle this gracefully. Gilt just errors.
  - Effort: Small-Medium — startup dialog + repo creation.

- [ ] **21. Git Notes Support**
  - What: Add, edit, and view `git notes` (annotations on commits).
  - Why it matters: Magit supports this. Useful for adding context to commits without changing them.
  - Effort: Small — `git notes add/show` wrappers.

- [ ] **22. `git clean` Integration**
  - What: Interactive `git clean` to remove untracked files.
  - Why it matters: Forgit has `gclean`. Useful for removing build artifacts.
  - Effort: Small — `git clean -nd` (dry-run) preview + confirm + `git clean -fd`.

- [ ] **23. Format-Patch / Apply-Patch**
  - What: Export commits as `.patch` files and apply patches from files.
  - Why it matters: Magit supports this. Essential for email-based workflows (kernel, etc.).
  - Effort: Small — `git format-patch` + `git am` wrappers.

- [ ] **24. Submodule Conflict Resolution**
  - What: Dedicated UI for resolving submodule conflicts (view ours/theirs commits, choose).
  - Why it matters: Lazygit has specialized submodule conflict handling.
  - Effort: Medium — submodule conflict detection + resolution UI.

- [ ] **25. Divergence Indicators**
  - What: Show ahead/behind counts next to branch names in the branches panel.
  - Why it matters: Lazygit shows arrows with numbers. Gilt has the data (`git-ahead-behind`) but doesn't display it in the branch list.
  - Effort: Small — format branch list items with ahead/behind.
