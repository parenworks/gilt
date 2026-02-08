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

## Planned

### Medium Priority

- [x] **Rename branch** — rename a local branch
- [x] **Stage all / unstage all toggle** — stage or unstage all files at once
- [x] **Push tag** — push a tag to remote
- [x] **Fast-forward branch** — fast-forward a branch to match its upstream
- [x] **New branch from stash** — create a new branch and apply a stash to it

### Lower Priority

- [ ] **Search / filter** — search and filter within all panels
- [ ] **Commit without hook** — commit bypassing pre-commit hooks
- [ ] **Commit with editor** — open external editor for commit message
- [ ] **Ignore file** — add file to .gitignore
- [ ] **Set upstream** — set tracking branch for local branch
- [ ] **Add remote** — add a new remote
- [ ] **Remove remote** — remove an existing remote
- [ ] **Rename stash** — rename a stash entry
- [ ] **Checkout tag** — checkout a tag (detached HEAD)
- [ ] **Tree view** — toggle between flat and tree file view
- [ ] **Custom commands** — user-defined git commands
