# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.18.0] - 2026-08-16

### Added

- **Reflog viewer** — Browse git reflog entries in a dedicated panel view
  - Press `w` on files panel to cycle Files → Worktrees → Stashes → Reflog → Files
  - Shows reflog selector (HEAD@{N}), short hash, and action message
  - `Enter` shows full commit details (hash, selector, action, full message)
  - `X` resets to any reflog entry (soft/mixed/hard)
  - Main panel shows diff of selected reflog entry vs current HEAD
  - `/` filters reflog entries by message or hash
- **Grep view** — Search file contents across the repo using `git grep`
  - Press `w` on files panel to cycle to Grep view
  - Press `/` to enter search pattern, results show file:line:content
  - `Enter` opens the file in `$EDITOR` at the matching line
  - Main panel shows file content with context around the matching line (highlighted)
  - `git-show-file` data layer function for viewing file content at any ref
- **Tree/Blob view** — Browse the directory tree of any commit and view file contents
  - Press `T` on commits panel to browse the tree at that commit
  - Directories shown with `[D]`, files with `[F]`, sorted dirs-first
  - `Enter` on a directory descends into it; `Enter` on a file shows its content
  - `Esc` goes up one level, or exits tree mode at root
  - `..` entry navigates to parent directory
  - Main panel shows file content with line numbers (blob view)
  - `git-ls-tree` data layer function for listing tree entries
- **Multi-commit cherry-pick** — Copy multiple commits and cherry-pick all at once
  - Press `Space` on commits panel to toggle a commit in the copy list (shown with `+` marker)
  - Press `V` to paste (cherry-pick all copied commits in order, oldest first)
  - Toast shows count of copied commits
- **Line range tracing** — Trace the history of a line using `git log -L`
  - In blame view, press `L` to trace the selected line's history
  - Shows list of commits that touched that line, with author/date/message
  - `Enter` shows the diff for the selected trace entry
  - `Esc` returns to blame view
- **Blame enhancements** — Enhanced blame view with parent navigation and copy detection
  - `p` navigates to the parent commit's blame (go backwards in history)
  - `C` toggles copy detection (`-C` flag) to detect code moved from other files
  - `o` opens the file at the selected line in `$EDITOR`
  - `L` traces the selected line's history (line range tracing)
  - `git-blame-at` and `git-blame-parent` data layer functions
- **Diff search** — Search within diff text in the main panel
  - Focus the main panel (Tab), then press `/` to search within the diff
  - `n` jumps to next match, `N` jumps to previous match
  - Toast shows match count
- **Commit graph improvements** — Better graph layout and all-branches commit list
  - `--topo-order` flag for cleaner graph layout (no intermixed branch lines)
  - `G` (capital) toggles all-branches mode for the commits panel
  - `git-log-all` data layer function for all-branch commits in topo order
- **Force push options** — Safer push options with force-with-lease and set-upstream
  - `P` dialog now offers: Push, Force with Lease, Force Push, Set Upstream
  - Force with Lease only forces if remote ref hasn't changed since last fetch
  - Set Upstream pushes and sets tracking branch
  - `git-push-force-with-lease`, `git-push-force`, `git-push-set-upstream` data layer functions
- **Pull with rebase/FF-only** — Pull options dialog
  - `p` dialog now offers: Pull, Pull --rebase, Pull --ff-only
  - Pull --rebase rebases local commits on top of pulled changes
  - Pull --ff-only only fast-forwards, fails if diverged
- **Clone/Init from UI** — Clone a repository or init a new one from within gilt
  - `Alt+c` opens Clone dialog, enter URL to clone
  - `Alt+i` opens Init dialog, initializes git repo in current directory
  - After cloning, gilt switches to the cloned repository
  - `git-clone` data layer function
- **Configurable keybindings** — Keybinding map with config parsing and context system
  - `~/.config/gilt/keybindings.conf` file with `[context]` sections
  - Contexts: global, files, branches, commits, tags, stashes
  - Format: `key=action` per line, context-specific bindings checked first then global
  - `load-keybindings` and `lookup-keybinding` data layer functions
  - Example config file at `keybindings.example.conf`
- **Command palette** — Searchable menu of all actions
  - Press `:` to open the command palette
  - Type to filter commands by name
  - `Enter` executes the selected command
  - `j`/`k` or arrow keys to navigate, `Esc` to cancel
- **Syntax highlighting in diffs** — Color-code file content by language using bat
  - Blob/tree view uses `bat` (or `batcat`) for syntax highlighting
  - `H` in tree mode toggles syntax highlighting on/off
  - Falls back to plain text if bat is not installed
  - `bat-available-p` and `bat-highlight` data layer functions
- **Split diff view** — Side-by-side diff rendering
  - Press `\` to toggle split diff mode
  - Uses `git diff --side-by-side` with 200-column width
  - `git-diff-split` and `git-diff-staged-split` data layer functions
- **Numstat in file list** — Show +N -M per file in the files panel
  - Press `=` on files panel to toggle numstat display
  - Shows added/removed line counts per file from `git diff --numstat`
  - `git-diff-numstat` data layer function
- **Bulk branch operations** — Multi-select branches for batch delete
  - `Space` on branches panel toggles branch selection (shown with `+` marker)
  - `D` deletes all selected branches at once with confirmation dialog
  - Toast shows count of selected branches
- **Commit message templates** — Configurable prefixes per branch pattern
  - `~/.config/gilt/commit-templates.conf` with `pattern=prefix` format
  - Pattern matched against branch name (substring match)
  - Prefix pre-filled in commit dialog when on matching branch
  - `load-commit-templates` and `get-commit-template` data layer functions
  - Example config at `commit-templates.example.conf`
- **Not-in-repo behavior** — Init/clone/browse recent from startup
  - When started outside a git repo, offers interactive menu:
    - `[1]` Initialize a new repository here
    - `[2]` Clone a repository (prompts for URL)
    - `[3]` Browse recent repositories (numbered list)
    - `[q]` Quit
  - Successfully opened repos are saved to recent repos list
- **Git notes support** — Add/edit/view git notes
  - `N` on commits panel opens dialog to add/edit note for selected commit
  - Notes shown in commit detail view
  - Dialog supports Save, Delete, and Cancel
  - `git-notes-show`, `git-notes-add`, `git-notes-remove`, `git-notes-list` data layer functions
- **git clean integration** — Dry-run preview + confirm + clean -fd
  - `K` on files panel shows preview of what would be removed
  - Dialog offers `Clean -fd` (untracked) and `Clean -fdx` (include ignored)
  - `git-clean-dry-run` and `git-clean` data layer functions
- **Format-patch / apply-patch** — git format-patch + git am wrappers
  - `F` on commits panel creates a patch file for the selected commit
  - `A` on files panel opens dialog to apply a patch file
  - Dialog supports `git apply`, `git am`, and check-only modes
  - `git-format-patch`, `git-format-patch-single`, `git-apply-patch-file`, `git-am-patch` data layer functions
- **Makefile fix** — `make install` no longer rebuilds under sudo
  - `install` target checks for existing binary instead of depending on build target
- **Comprehensive worktree management** — Full lazygit-parity worktree features:
  - **Switch to worktree** (`Enter` in worktrees view) — switches the entire gilt session into the worktree's directory, reloading all panels
  - **Leave worktree** (`Esc`) — returns to the parent repository context
  - **Open worktree in editor** (`o` in worktrees view) — opens the worktree directory in `$EDITOR`
  - **Lock/unlock worktree** (`L` in worktrees view) — lock with optional reason, unlock
  - **Prune worktrees** (`p` in worktrees view) — removes stale worktree entries
  - **Enhanced worktree removal** (`D` in worktrees view) — options to remove only, remove + delete local branch, or remove + delete both local and remote branches
  - **Cross-panel worktree creation** (`W` on branches/commits/tags/stashes panels) — creates a worktree from the selected ref with context-sensitive options (new branch + worktree, checkout in worktree, detached worktree, tracking branch + worktree)
  - **Location picker for new worktrees** — "Pick Location" button in Add Worktree dialog shows candidate parent directories (existing worktree parents, configured default path, repo root parent, home directory)
  - **Auto-switch after creating worktree** — automatically switches into the newly created worktree after creation
  - **Configurable default worktree path** — set `defaultPath=` in `~/.config/gilt/worktree.conf` to add a default parent directory to the location picker
  - **Worktree details in main panel** — shows name, branch, path, and status indicators (bare, detached, locked, missing, current, main) with color coding
  - **Current worktree indicator** — `*` marker and green color on the current worktree in the list
  - **Main worktree label** — `(main)` label on the main worktree
  - **Missing worktree detection** — shows `(missing)` in red for worktrees whose directories have been deleted but not pruned
  - **Uniquified worktree names** — derives short unique names from paths (e.g. `/repo/feature-foo` → `feature-foo`), disambiguating collisions by prepending parent dirs
  - **Detect branch during rebase/bisect** — reads `rebase-merge/head-name` and `BISECT_START` to find the branch name when `git worktree list` doesn't show it
- **Checkout branch → redirect to worktree** — when checking out a branch that's already checked out in another worktree, offers to switch to that worktree instead of failing
- **Detach worktree then delete branch** — when deleting a branch checked out in another worktree, offers to detach the worktree first or remove it entirely, then delete the branch
- **Fast-forward branch in worktree** — `F` on a branch checked out in another worktree runs the fast-forward fetch in that worktree's git directory without switching
- **Worktree move** — `git worktree move` support in the data layer
- **Worktree repair** — `git worktree repair` support in the data layer
- **Dialog input pre-fill** — `make-dialog` now accepts `:input-buffer` to pre-fill the input field (used by location picker)
- **Updated help text** — all new worktree keybindings documented in help overlay and context-sensitive help bars

### Changed

- Bumped version to 0.18.0

## [0.17.0] - 2026-04-16

### Added

- **Multi-arch CI release builds** - GitHub Actions now builds binaries for Linux x86_64, macOS arm64, and macOS x86_64
  - Triggered automatically on tag push or manually via workflow dispatch
  - macOS x86_64 built via Rosetta on arm64 runner

### Fixed

- **Pre-built binaries not accepting keyboard input on macOS** - CI-built saved images had stale terminal state baked in from the build environment
  - All terminal state now deferred to runtime initialization
  - Raw mode and ioctl use an explicit `/dev/tty` fd opened at runtime instead of relying on SBCL's internal `sb-sys:*stdin*`
  - TTY path, escape timeout, terminal mode, and input reader all initialized fresh on startup

## [0.16.0] - 2026-04-15

### Added

- **SIGWINCH terminal resize handling** - UI now redraws automatically when the terminal window is resized
  - Installs a SIGWINCH signal handler during raw mode
  - Main loop polls for resize events between keypresses
  - No manual refresh needed after resize
- **Lazygit-style repo init prompt** - When launched outside a git repository, prompts to create one instead of crashing
  - Answers "y" to run `git init` and launch gilt
  - Answers "n" to exit cleanly

### Fixed

- **macOS terminal size detection** - Use correct TIOCGWINSZ ioctl constant for Darwin (`0x40087468` vs Linux `0x5413`)
- **Terminal size returning garbage values** - Use `unsigned 16`-bit fields matching `struct winsize` layout, check ioctl return code, add sanity bounds
- **stty fallback for terminal size** - Added portable `stty size` fallback when ioctl fails, with cross-platform path lookup
- **Crash when launched outside git repo** - No longer attempts to change directory to empty string

## [0.15.0] - 2026-02-09

### Added

- **Line-level staging** - Stage individual lines within a hunk
  - Press `e` on a modified file to enter hunk mode
  - Press `Enter` on a hunk to see individual diff lines
  - `Space` toggles line selection (● selected, ○ unselected)
  - `a` selects all changed lines, `n` deselects all
  - `Enter` stages only the selected lines
  - `Escape` returns to hunk list
  - Builds valid partial patches with corrected line counts
- **Stash individual files** - Press `S` on files panel to stash selected file(s)
  - Opens dialog for optional stash message
  - Supports untracked files (`--include-untracked`)
  - Works with range selection (`v`) for multiple files
- **Commit graph** - Press `g` on commits panel to toggle graph view
  - Shows `git log --graph --oneline --decorate --all` in main panel
  - Toggle on/off with `g` key
- **Stash diff view** - Main panel shows colored diff when stash panel is focused
- **Mouse scroll wheel** - Scroll wheel navigates items up/down in focused panel
  - SGR mouse tracking enabled (works in terminals that support it)
  - Click support implemented for terminals that pass click events through

### Fixed

- **Blue bar artifacts on backspace in dialogs** - Old characters now cleared when input text shrinks
- **Stash individual files failing for untracked files** - Added `--include-untracked` flag

## [0.14.0] - 2026-02-09

### Added

- **Interactive rebase** - Press `i` on commits panel to enter rebase mode
  - Select a commit to define the range (HEAD to selected)
  - `p` pick, `r` reword, `s` squash, `f` fixup, `d` drop
  - `J`/`K` to reorder commits
  - `Enter` to execute, `q` to cancel
  - Color-coded display for each action type
- **Rebase branch** - Press `R` on branches panel to rebase current branch onto selected branch
- **Rename branch** - Press `N` on branches panel to rename a local branch
- **Fast-forward branch** - Press `F` on branches panel to fast-forward a branch to match upstream
- **Push tag** - Press `T` in tags view to push a tag (or all tags) to remote
- **Stage all / unstage all toggle** - `a` key now toggles between staging and unstaging all files
- **New branch from stash** - Press `B` in stashes view to create a branch from a stash
- **Commit with editor** - Press `C` on files panel to open `$EDITOR` for commit message
- **Commit without hook** - Press `w` on files panel to commit bypassing pre-commit hooks
- **Undo/redo** - Press `z` to undo last git command, `Z` to redo (uses reflog)
- **Checkout tag** - Press `Space` in tags view to checkout tag as detached HEAD
- **Rename stash** - Press `R` in stashes view to rename a stash entry
- **Set upstream** - Press `u` on branches panel to set/unset upstream tracking
- **Diff context size** - Press `{`/`}` to decrease/increase diff context lines
- **Whitespace toggle** - Press `W` to toggle whitespace visibility in diffs
- **Squash merge** - Merge dialog now offers Merge or Squash options
- **Sort branches** - Press `s` on branches panel to cycle sort: name/date/recent
- **Bisect** - Press `b` on commits panel to start bisect, then `b`:bad `g`:good `Q`:reset
- **Search / filter** - Press `/` on files, branches, or stash panels to filter items
- **Copy to clipboard** - Press `y` to copy file path, branch name, or commit hash
- **Open in browser** - Press `o` to open commit or branch URL in browser
- **Shell command** - Press `:` to run arbitrary shell commands (vim-style)
- **External diff tool** - Press `x` on files panel to launch `git difftool`
- **Ignore file** - Press `I` on files panel to add file to `.gitignore`
- **File tree view** - Press `T` on files panel to toggle flat/tree layout
- **Range select** - Press `v` on files panel to start/end range, stage/unstage range
- **Page up/down** - `PgUp`/`PgDn` for page navigation in long lists
- **Enter submodule** - Press `Enter` on submodule to navigate into it as nested repo
- **Recent repo switching** - Press `L` to show recent repos, Enter to switch
- **Screen mode cycling** - Press `+` to cycle normal/half/full layout modes
- **Create pull request** - Press `O` on branches panel to open PR in browser
- **Git-flow integration** - Press `E` for git-flow menu (feature/release/hotfix)
- **Custom command keybindings** - Define in `~/.config/gilt/commands.conf` (key=command)
- **Custom patch building** - `git apply --cached` support for building patches
- **Force push** - Push dialog now includes Force Push (with lease) option
- **Context-sensitive hints bar** - Branches panel hints change for Local/Remotes/Tags/Submodules views
- **ROADMAP.md** - Feature roadmap tracking implemented and planned features

### Fixed

- **ESC key not working** - Fixed escape key handling for closing overlays and dialogs
- **Arrow keys not working** - Fixed arrow key input handling across all panels
- **`w` key cycling wrong panel** - Fixed `w` to cycle Files/Worktrees/Stashes on files panel (was on commits panel)
- **Staged files not visually distinct** - Staged files now display in green
- **Key handlers consuming keys for wrong panels** - Systematic fix of panel guards on all key handlers to prevent keys from being swallowed by the wrong panel's handler
- **`x` (difftool) crash** - Fixed undefined function error for alternate screen functions
- **Browser URL wrong for SSH host aliases** - Resolves SSH host aliases (e.g., `github-parenworks` to `github.com`) via `~/.ssh/config`
- **`X`, `F`, `C`, `R` not working on commits panel** - Earlier handlers for other panels were consuming these keys
- **`g` (bisect good) not working** - Stash pop handler was consuming the key on commits panel
- **`o` (resolve conflict ours) not working** - Merged conflict resolution and open-in-browser into single context-sensitive handler
- **`A` (add remote) not working in remotes view** - Add Worktree handler lacked panel guard
- **`R` (rename remote) not working in remotes view** - Rebase handler consumed key in remotes view
- **Git Flow dialog buttons overflow** - Dialog width now accounts for total button width
- **Screen resize (`+`) crash** - Fixed `UNSIGNED-BYTE -1` error from negative dimensions in draw functions
- **Copy file path only copying filename** - Now copies full path (repo root + relative path)
- **Shell command output staggered** - Multi-line output now split into individual log lines
- **D key not working on branches panel** - Duplicate key handler made branch/tag/remote deletion unreachable; merged into single dispatcher
- **Duplicate function definitions** - Removed old `git-stash-list` and `git-stash-pop` that were superseded by enhanced versions

## [0.13.0] - 2026-02-07

### Changed

- **Terminal control via FFI** - Replaced all `stty` subprocess calls with direct POSIX termios FFI
  - Uses `sb-posix:tcgetattr`/`tcsetattr` for raw mode control
  - Uses `sb-alien` ioctl with `TIOCGWINSZ` for terminal size queries
  - Eliminates `stty` dependency entirely (fixes NixOS, containers, non-standard paths)
  - Faster startup and keypress handling (no subprocess forks)
- `--debug` mode now tests termios FFI directly instead of stty
- `diagnose.lisp` rewritten to use FFI-based diagnostics

### Removed

- `*stty-path*` parameter and `GILT_STTY_PATH` environment variable (no longer needed)
- `find-stty`, `find-tty`, `detect-terminal-type` utility functions (replaced by FFI)
- `NIXOS_SUPPORT.md` - no longer needed since stty dependency is eliminated

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

[Unreleased]: https://github.com/parenworks/gilt/compare/v0.18.0...HEAD
[0.18.0]: https://github.com/parenworks/gilt/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/parenworks/gilt/compare/v0.16.0...v0.17.0
[0.16.0]: https://github.com/parenworks/gilt/compare/v0.15.0...v0.16.0
[0.15.0]: https://github.com/parenworks/gilt/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/parenworks/gilt/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/parenworks/gilt/releases/tag/v0.13.0
[0.12.0]: https://github.com/parenworks/gilt/releases/tag/v0.12.0
[0.11.0]: https://github.com/parenworks/gilt/releases/tag/v0.11.0
[0.10.0]: https://github.com/parenworks/gilt/releases/tag/v0.10.0
[0.1.0]: https://github.com/parenworks/gilt/releases/tag/v0.1.0
