# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.15.0] - 2026-02-09

### Added

- **Line-level staging** — Stage individual lines within a hunk
  - Press `e` on a modified file to enter hunk mode
  - Press `Enter` on a hunk to see individual diff lines
  - `Space` toggles line selection (● selected, ○ unselected)
  - `a` selects all changed lines, `n` deselects all
  - `Enter` stages only the selected lines
  - `Escape` returns to hunk list
  - Builds valid partial patches with corrected line counts
- **Stash individual files** — Press `S` on files panel to stash selected file(s)
  - Opens dialog for optional stash message
  - Supports untracked files (`--include-untracked`)
  - Works with range selection (`v`) for multiple files
- **Commit graph** — Press `g` on commits panel to toggle graph view
  - Shows `git log --graph --oneline --decorate --all` in main panel
  - Toggle on/off with `g` key
- **Stash diff view** — Main panel shows colored diff when stash panel is focused
- **Mouse scroll wheel** — Scroll wheel navigates items up/down in focused panel
  - SGR mouse tracking enabled (works in terminals that support it)
  - Click support implemented for terminals that pass click events through

### Fixed

- **Blue bar artifacts on backspace in dialogs** — Old characters now cleared when input text shrinks
- **Stash individual files failing for untracked files** — Added `--include-untracked` flag

## [0.14.0] - 2026-02-09

### Added

- **Interactive rebase** — Press `i` on commits panel to enter rebase mode
  - Select a commit to define the range (HEAD to selected)
  - `p` pick, `r` reword, `s` squash, `f` fixup, `d` drop
  - `J`/`K` to reorder commits
  - `Enter` to execute, `q` to cancel
  - Color-coded display for each action type
- **Rebase branch** — Press `R` on branches panel to rebase current branch onto selected branch
- **Rename branch** — Press `N` on branches panel to rename a local branch
- **Fast-forward branch** — Press `F` on branches panel to fast-forward a branch to match upstream
- **Push tag** — Press `T` in tags view to push a tag (or all tags) to remote
- **Stage all / unstage all toggle** — `a` key now toggles between staging and unstaging all files
- **New branch from stash** — Press `B` in stashes view to create a branch from a stash
- **Commit with editor** — Press `C` on files panel to open `$EDITOR` for commit message
- **Commit without hook** — Press `w` on files panel to commit bypassing pre-commit hooks
- **Undo/redo** — Press `z` to undo last git command, `Z` to redo (uses reflog)
- **Checkout tag** — Press `Space` in tags view to checkout tag as detached HEAD
- **Rename stash** — Press `R` in stashes view to rename a stash entry
- **Set upstream** — Press `u` on branches panel to set/unset upstream tracking
- **Diff context size** — Press `{`/`}` to decrease/increase diff context lines
- **Whitespace toggle** — Press `W` to toggle whitespace visibility in diffs
- **Squash merge** — Merge dialog now offers Merge or Squash options
- **Sort branches** — Press `s` on branches panel to cycle sort: name/date/recent
- **Bisect** — Press `b` on commits panel to start bisect, then `b`:bad `g`:good `Q`:reset
- **Search / filter** — Press `/` on files, branches, or stash panels to filter items
- **Copy to clipboard** — Press `y` to copy file path, branch name, or commit hash
- **Open in browser** — Press `o` to open commit or branch URL in browser
- **Shell command** — Press `:` to run arbitrary shell commands (vim-style)
- **External diff tool** — Press `x` on files panel to launch `git difftool`
- **Ignore file** — Press `I` on files panel to add file to `.gitignore`
- **File tree view** — Press `T` on files panel to toggle flat/tree layout
- **Range select** — Press `v` on files panel to start/end range, stage/unstage range
- **Page up/down** — `PgUp`/`PgDn` for page navigation in long lists
- **Enter submodule** — Press `Enter` on submodule to navigate into it as nested repo
- **Recent repo switching** — Press `L` to show recent repos, Enter to switch
- **Screen mode cycling** — Press `+` to cycle normal/half/full layout modes
- **Create pull request** — Press `O` on branches panel to open PR in browser
- **Git-flow integration** — Press `E` for git-flow menu (feature/release/hotfix)
- **Custom command keybindings** — Define in `~/.config/gilt/commands.conf` (key=command)
- **Custom patch building** — `git apply --cached` support for building patches
- **Force push** — Push dialog now includes Force Push (with lease) option
- **Context-sensitive hints bar** — Branches panel hints change for Local/Remotes/Tags/Submodules views
- **ROADMAP.md** — Feature roadmap tracking implemented and planned features

### Fixed

- **ESC key not working** — Fixed escape key handling for closing overlays and dialogs
- **Arrow keys not working** — Fixed arrow key input handling across all panels
- **`w` key cycling wrong panel** — Fixed `w` to cycle Files/Worktrees/Stashes on files panel (was on commits panel)
- **Staged files not visually distinct** — Staged files now display in green
- **Key handlers consuming keys for wrong panels** — Systematic fix of panel guards on all key handlers to prevent keys from being swallowed by the wrong panel's handler
- **`x` (difftool) crash** — Fixed undefined function error for alternate screen functions
- **Browser URL wrong for SSH host aliases** — Resolves SSH host aliases (e.g., `github-parenworks` → `github.com`) via `~/.ssh/config`
- **`X`, `F`, `C`, `R` not working on commits panel** — Earlier handlers for other panels were consuming these keys
- **`g` (bisect good) not working** — Stash pop handler was consuming the key on commits panel
- **`o` (resolve conflict ours) not working** — Merged conflict resolution and open-in-browser into single context-sensitive handler
- **`A` (add remote) not working in remotes view** — Add Worktree handler lacked panel guard
- **`R` (rename remote) not working in remotes view** — Rebase handler consumed key in remotes view
- **Git Flow dialog buttons overflow** — Dialog width now accounts for total button width
- **Screen resize (`+`) crash** — Fixed `UNSIGNED-BYTE -1` error from negative dimensions in draw functions
- **Copy file path only copying filename** — Now copies full path (repo root + relative path)
- **Shell command output staggered** — Multi-line output now split into individual log lines
- **D key not working on branches panel** — Duplicate key handler made branch/tag/remote deletion unreachable; merged into single dispatcher
- **Duplicate function definitions** — Removed old `git-stash-list` and `git-stash-pop` that were superseded by enhanced versions

## [0.13.0] - 2026-02-07

### Changed

- **Terminal control via FFI** — Replaced all `stty` subprocess calls with direct POSIX termios FFI
  - Uses `sb-posix:tcgetattr`/`tcsetattr` for raw mode control
  - Uses `sb-alien` ioctl with `TIOCGWINSZ` for terminal size queries
  - Eliminates `stty` dependency entirely (fixes NixOS, containers, non-standard paths)
  - Faster startup and keypress handling (no subprocess forks)
- `--debug` mode now tests termios FFI directly instead of stty
- `diagnose.lisp` rewritten to use FFI-based diagnostics

### Removed

- `*stty-path*` parameter and `GILT_STTY_PATH` environment variable (no longer needed)
- `find-stty`, `find-tty`, `detect-terminal-type` utility functions (replaced by FFI)
- `NIXOS_SUPPORT.md` — no longer needed since stty dependency is eliminated

## [0.12.0] - 2026-02-04

### Added

- **Cross-Platform Unix Support**
  - NixOS compatibility - Auto-detects stty at `/run/current-system/sw/bin/stty`
  - Dynamic path resolution for stty command across different Unix systems
  - Dynamic TTY device detection (`/dev/tty`, `/dev/pts/0`, etc.)
  - Terminal emulator detection with optimized settings for Alacritty
  - Configurable via environment variables: `GILT_STTY_PATH`, `GILT_TTY_PATH`, `GILT_ESCAPE_TIMEOUT`

### Fixed

- **Keyboard Input Freeze** - Fixed freeze when pressing keys on NixOS/Alacritty
  - Use `O_NONBLOCK` on TTY file descriptor for non-blocking I/O
  - Replaced blocking read with polling loop
  - Adaptive escape sequence timeout based on terminal type

### Documentation

- Added `NIXOS_SUPPORT.md` with cross-platform troubleshooting guide
- Updated `diagnose.lisp` with comprehensive system detection tests

## [0.11.0] - 2026-02-02

### Added

- **Stash Management**
  - Stash list panel - Press `w` in Files panel to cycle to Stashes view
  - Pop stash (`p` or `P`) - Apply and remove stash
  - Apply stash (`Enter`) - Apply without removing
  - Drop stash (`D`) - Delete a stash
  - Stash with message (`s`) - Create named stashes
  - Stash diff preview - View colored diff in main panel
  - Main panel scrolling - Press `0` to focus, `j/k` to scroll

- **Commit Operations**
  - Amend commit (`A`) - Amend HEAD with or without new message
  - Reset to commit (`X`) - Soft/Mixed/Hard reset options
  - Fixup commit (`F`) - Create fixup! commit for autosquash

## [0.10.0] - 2026-02-02

### Added

- Help overlay (`?` key) - View all keybindings
- Async push/pull with in-TUI output and credential prompt support
- Blame view (`b` on files) with commit info on Enter
- Cherry-pick from branches (`C` on branches panel)
- Commit search (`/`) - Filter by message or author
- Status bar with branch tracking info (ahead/behind)
- Tag support - Create (`t`), delete (`D`), view tags
- Remote management - Add (`A`), rename (`R`), delete remote branches
- Submodule support - View and update (`U`) submodules
- Config viewer (`G`) - Browse git config (local/global/system)
- Worktree management (`w` in Files panel) - Add (`A`), remove (`D`)

## [0.1.0] - 2026-02-02

### Added

- Initial release of Gilt - Git Interface for Lisp Terminal
- **Core UI**
  - LazyGit-inspired 5-panel layout with colored output
  - Pure ANSI rendering (no ncurses dependency)
  - 256-color support with syntax highlighting
  - Context-sensitive help bar with version display
  - Help overlay (`?` key)
- **File Operations**
  - Stage/unstage files with `Space`
  - Stage all with `a`
  - Discard changes with `d`
  - Hunk staging mode with `e`
  - Conflict resolution (`o` for ours, `t` for theirs, `X` to abort)
- **Commit Operations**
  - Create commits with multi-line message support
  - Squash commits (`S`)
  - Cherry-pick commits (`C`)
  - Revert commits (`R`)
  - Search commits by message/author (`/`)
- **Branch Operations**
  - Create new branches (`n`)
  - Checkout branches (`Enter`)
  - Merge branches (`M`)
  - Delete branches (`D`)
  - Cherry-pick from other branches (`C` on branches panel)
  - Toggle Local/Remotes/Tags view (`w`)
- **Tag Support**
  - View tags in branches panel (cycle with `w`)
  - Create tags on commits (`t` on commits panel)
  - Create tags on HEAD (`t` in tags view)
  - Delete tags (`D` in tags view)
- **Remote Operations**
  - Push to origin (`P`) with async output display
  - Pull from origin (`p`) with async output display
  - Fetch from remotes (`f`)
  - Track remote branches
- **Stash Operations**
  - Stash changes (`s`)
  - Pop stash (`g`)
  - Apply stash (`Enter`)
- **Blame View**
  - View git blame for files (`b`)
  - Navigate blame lines with selection highlight
  - View commit details for any line (`Enter`)
- **Status Bar**
  - Branch tracking info (ahead/behind upstream)
  - Repository state indicator (MERGING, REBASING, etc.)

[Unreleased]: https://github.com/parenworks/gilt/compare/v0.15.0...HEAD
[0.15.0]: https://github.com/parenworks/gilt/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/parenworks/gilt/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/parenworks/gilt/releases/tag/v0.13.0
[0.12.0]: https://github.com/parenworks/gilt/releases/tag/v0.12.0
[0.11.0]: https://github.com/parenworks/gilt/releases/tag/v0.11.0
[0.10.0]: https://github.com/parenworks/gilt/releases/tag/v0.10.0
[0.1.0]: https://github.com/parenworks/gilt/releases/tag/v0.1.0
