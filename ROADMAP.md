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
- [x] Line-level staging (select individual lines within hunks)
- [x] Blame view
- [x] Worktree management (add, remove, force remove)
- [x] Stash view (apply, pop, drop)
- [x] Stash individual files (`S` key)

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
- [x] Commit graph view (`g` key — all branches)
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
- [x] Stash diff view (colored diff in main panel)

### Global

- [x] Push to remote
- [x] Pull from remote
- [x] Fetch from remote(s)
- [x] Command log panel
- [x] Context-sensitive help bar
- [x] Panel navigation (Tab, h/l, number keys)
- [x] Main panel scrolling
- [x] Mouse scroll wheel navigation
- [x] Mouse click support (terminal-dependent)
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

- [x] **Checkout tag** — checkout a tag as detached HEAD
- [x] **Rename stash** — rename a stash entry
- [x] **Set upstream** — set/unset tracking branch for local branch
- [x] **Diff context size** — increase/decrease lines of context in diff view
- [x] **Whitespace toggle** — toggle whitespace visibility in diffs
- [x] **Squash merge** — option for squash merge in merge dialog
- [x] **Sort branches** — sort branches by name, date, etc.
- [x] **Bisect** — interactive git bisect from commits panel
- [x] **Search / filter** — search and filter within all panels

### Lower Priority

- [x] **File tree view** — toggle between flat and tree file layout
- [x] **Copy to clipboard** — copy commit hash, branch name, file path
- [x] **Open in browser** — open commit or PR URL in browser
- [x] **Shell command execution** — run arbitrary git/shell commands
- [x] **External diff tool** — launch `git difftool` for selected file
- [x] **Ignore file** — add file to .gitignore
- [x] **Range select** — multi-select items with `v` key
- [x] **Page up/down** — page navigation in long lists
- [x] **Enter submodule** — navigate into submodule as nested repo
- [x] **Recent repo switching** — switch between recently opened repos
- [x] **Screen mode cycling** — normal/half/fullscreen layout modes
- [x] **Custom command keybindings** — user-defined commands in config file
- [x] **Git-flow integration** — git-flow init/start/finish workflows
- [x] **Custom patch building** — select individual lines from commits to build patches
- [x] **Create pull request** — open PR creation from branches panel
