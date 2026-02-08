# Gilt Feature Roadmap

## Implemented

### Status Panel

- [x] Repository status display
- [x] Current branch indicator
- [x] Tracking info (ahead/behind)
- [x] Repository state detection (merging, rebasing, cherry-picking, reverting, bisecting)

### Files Panel

- [x] Stage/unstage individual files
- [x] Commit with message
- [x] Discard changes
- [x] Diff view in main panel (staged and unstaged)
- [x] Conflict resolution (ours/theirs)
- [x] Edit file in external editor
- [x] Hunk staging (partial staging via `e` key)
- [x] Blame view
- [x] Worktree management (add, remove, force remove)
- [x] Stash view (apply, pop, drop)

### Branches Panel

- [x] Local branch list
- [x] Remote branch list
- [x] Create new branch
- [x] Checkout branch
- [x] Delete local branch
- [x] Delete remote branch
- [x] Merge branch into current
- [x] Rebase current branch onto selected branch
- [x] Track remote branch
- [x] Tag list with annotated/lightweight display
- [x] Create tag (lightweight and annotated)
- [x] Delete tag
- [x] Submodule list with status indicators
- [x] Update submodule

### Commits Panel

- [x] Commit log with hash, author, and message
- [x] Commit detail view with full message
- [x] Cherry-pick
- [x] Revert commit
- [x] Amend commit (staged changes into HEAD, with or without new message)
- [x] Squash commits (soft reset approach)
- [x] Fixup commit
- [x] Reset to commit (soft, mixed, hard)
- [x] Interactive rebase (pick, reword, squash, fixup, drop, reorder)
- [x] Search commits
- [x] Reflog view
- [x] Blame view
- [x] Tag commit

### Stash Panel

- [x] Stash current changes (with optional message)
- [x] Stash staged only
- [x] Stash including untracked
- [x] Apply stash
- [x] Pop stash
- [x] Drop stash
- [x] Stash diff view

### Global

- [x] Push to remote
- [x] Pull from remote
- [x] Fetch from remote(s)
- [x] Command log panel
- [x] Context-sensitive help bar
- [x] Panel navigation (Tab, h/l, number keys)
- [x] Main panel scrolling
- [x] Help screen (?)
- [x] FFI-based terminal control (no stty dependency)

---

## Recently Completed

- [x] **Rename branch** — rename a local branch
- [x] **Stage all / unstage all toggle** — stage or unstage all files at once
- [x] **Push tag** — push a tag to remote
- [x] **Fast-forward branch** — fast-forward a branch to match its upstream
- [x] **New branch from stash** — create a new branch and apply a stash to it

---

## Planned

### High Priority

- [x] **Commit with editor** — open `$EDITOR` for commit message (supports multiline, verbose)
- [x] **Undo/redo** — use reflog to undo/redo the last git command (`z`/`Z`)
- [x] **Commit without hook** — commit bypassing pre-commit hooks (`--no-verify`)

### Medium Priority

- [ ] **Checkout tag** — checkout a tag as detached HEAD
- [ ] **Rename stash** — rename a stash entry
- [ ] **Set upstream** — set/unset tracking branch for local branch
- [ ] **Diff context size** — increase/decrease lines of context in diff view
- [ ] **Whitespace toggle** — toggle whitespace visibility in diffs
- [ ] **Squash merge** — option for squash merge in merge dialog
- [ ] **Sort branches** — sort branches by name, date, etc.
- [ ] **Bisect** — interactive git bisect from commits panel
- [ ] **Search / filter** — search and filter within all panels

### Lower Priority

- [ ] **File tree view** — toggle between flat and tree file layout
- [ ] **Copy to clipboard** — copy commit hash, branch name, file path
- [ ] **Open in browser** — open commit or PR URL in browser
- [ ] **Shell command execution** — run arbitrary git/shell commands
- [ ] **External diff tool** — launch `git difftool` for selected file
- [ ] **Ignore file** — add file to .gitignore
- [ ] **Range select** — multi-select items with `v` key
- [ ] **Page up/down** — page navigation in long lists
- [ ] **Enter submodule** — navigate into submodule as nested repo
- [ ] **Recent repo switching** — switch between recently opened repos
- [ ] **Screen mode cycling** — normal/half/fullscreen layout modes
- [ ] **Custom command keybindings** — user-defined commands in config file
- [ ] **Git-flow integration** — git-flow init/start/finish workflows
- [ ] **Custom patch building** — select individual lines from commits to build patches
- [ ] **Create pull request** — open PR creation from branches panel
