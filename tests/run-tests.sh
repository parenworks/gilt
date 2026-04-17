#!/usr/bin/env bash
#
# run-tests.sh — Automated smoke tests for Gilt's git backend functions
#
# These tests exercise the non-TUI parts: git backend, clipboard, config,
# recent repos, and verify the binary launches without crashing.
#
# Usage:
#   ./tests/run-tests.sh [gilt-test-repo-path]
#
# If no path given, creates a temporary test repo.
#
set -uo pipefail

# ─── Colors ───────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

PASS=0
FAIL=0
SKIP=0
TOTAL=0

pass() {
    ((TOTAL++))
    ((PASS++))
    echo -e "  ${GREEN}[PASS]${NC} $1"
}

fail() {
    ((TOTAL++))
    ((FAIL++))
    echo -e "  ${RED}[FAIL]${NC} $1"
    [ -n "${2:-}" ] && echo -e "        ${RED}→ $2${NC}"
}

skip() {
    ((TOTAL++))
    ((SKIP++))
    echo -e "  ${YELLOW}[SKIP]${NC} $1 — $2"
}

section() {
    echo ""
    echo -e "${CYAN}━━━ $1 ━━━${NC}"
}

# ─── Setup ────────────────────────────────────────────────────────
GILT_BIN=$(which gilt 2>/dev/null || echo "")
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="${1:-}"

if [ -z "$REPO_DIR" ]; then
    REPO_DIR=$(mktemp -d /tmp/gilt-test-XXXXXX)
    echo -e "${CYAN}Creating temporary test repo at $REPO_DIR${NC}"
    bash "$SCRIPT_DIR/setup-test-repo.sh" "$REPO_DIR" 2>/dev/null
    CLEANUP=true
else
    CLEANUP=false
fi

if [ ! -d "$REPO_DIR/.git" ]; then
    echo -e "${RED}Error: $REPO_DIR is not a git repository${NC}"
    echo "Run ./tests/setup-test-repo.sh first"
    exit 1
fi

echo ""
echo "═══════════════════════════════════════════════════"
echo "  Gilt Automated Smoke Tests"
echo "  Repo: $REPO_DIR"
echo "  Gilt: ${GILT_BIN:-NOT FOUND}"
echo "  Date: $(date)"
echo "═══════════════════════════════════════════════════"

# ─── 1. Binary Tests ─────────────────────────────────────────────
section "1. Binary & Build"

if [ -n "$GILT_BIN" ]; then
    pass "gilt binary found at $GILT_BIN"
else
    fail "gilt binary not found in PATH"
fi

# Verify the binary is executable and is an SBCL core
if [ -n "$GILT_BIN" ]; then
    if [ -x "$GILT_BIN" ]; then
        pass "gilt binary is executable"
    else
        fail "gilt binary is not executable"
    fi
    FILE_TYPE=$(file "$GILT_BIN" 2>/dev/null || echo "unknown")
    if echo "$FILE_TYPE" | grep -qi "executable\|ELF\|data"; then
        pass "gilt binary is a valid executable ($FILE_TYPE)"
    else
        fail "gilt binary type unexpected: $FILE_TYPE"
    fi
fi

# ─── 2. Test Repo State ──────────────────────────────────────────
section "2. Test Repo State"

cd "$REPO_DIR"

# Branches
BRANCH_COUNT=$(git branch | wc -l)
if [ "$BRANCH_COUNT" -ge 4 ]; then
    pass "Multiple branches exist ($BRANCH_COUNT branches)"
else
    fail "Expected ≥4 branches, got $BRANCH_COUNT"
fi

# Check specific branches
for branch in master feature/login feature/dashboard bugfix/typo conflict-branch; do
    if git branch | grep -q "$branch"; then
        pass "Branch '$branch' exists"
    else
        fail "Branch '$branch' missing"
    fi
done

# Tags
TAG_COUNT=$(git tag | wc -l)
if [ "$TAG_COUNT" -ge 2 ]; then
    pass "Tags exist ($TAG_COUNT tags)"
else
    fail "Expected ≥2 tags, got $TAG_COUNT"
fi

if git tag | grep -q "v0.1.0"; then
    pass "Lightweight tag v0.1.0 exists"
else
    fail "Lightweight tag v0.1.0 missing"
fi

if git tag | grep -q "v1.0.0"; then
    pass "Annotated tag v1.0.0 exists"
else
    fail "Annotated tag v1.0.0 missing"
fi

# Commits
COMMIT_COUNT=$(git log --oneline | wc -l)
if [ "$COMMIT_COUNT" -ge 8 ]; then
    pass "Sufficient commit history ($COMMIT_COUNT commits)"
else
    fail "Expected ≥8 commits, got $COMMIT_COUNT"
fi

# Stashes
STASH_COUNT=$(git stash list | wc -l)
if [ "$STASH_COUNT" -ge 1 ]; then
    pass "Stashes exist ($STASH_COUNT stashes)"
else
    fail "Expected ≥1 stash, got $STASH_COUNT"
fi

# Submodule
if [ -f .gitmodules ]; then
    pass "Submodule configured (.gitmodules exists)"
else
    fail "No submodule found"
fi

# Modified files
MODIFIED=$(git status --porcelain | grep "^ M" | wc -l)
if [ "$MODIFIED" -ge 1 ]; then
    pass "Modified files present ($MODIFIED files)"
else
    fail "No modified files found"
fi

# Staged files
STAGED=$(git status --porcelain | grep "^A " | wc -l)
if [ "$STAGED" -ge 1 ]; then
    pass "Staged files present ($STAGED files)"
else
    fail "No staged files found"
fi

# Untracked files
UNTRACKED=$(git status --porcelain | grep "^??" | wc -l)
if [ "$UNTRACKED" -ge 1 ]; then
    pass "Untracked files present ($UNTRACKED files)"
else
    fail "No untracked files found"
fi

# .gitignore
if [ -f .gitignore ]; then
    if grep -q "*.log" .gitignore; then
        pass ".gitignore contains *.log pattern"
    else
        fail ".gitignore missing *.log pattern"
    fi
else
    fail ".gitignore not found"
fi

# ─── 3. Git Operations ───────────────────────────────────────────
section "3. Git Operations (non-TUI)"

# Stage/unstage
git add untracked-file.txt 2>/dev/null
if git status --porcelain | grep -q "^A.*untracked-file.txt"; then
    pass "git add (stage) works"
else
    fail "git add failed"
fi

git reset HEAD untracked-file.txt 2>/dev/null
if git status --porcelain | grep -q "^??.*untracked-file.txt"; then
    pass "git reset (unstage) works"
else
    fail "git reset failed"
fi

# Branch operations
git checkout -b test-branch-smoke 2>/dev/null
if [ "$(git branch --show-current)" = "test-branch-smoke" ]; then
    pass "Branch create + checkout works"
else
    fail "Branch create failed"
fi

git checkout master 2>/dev/null
git branch -d test-branch-smoke 2>/dev/null
if ! git branch | grep -q "test-branch-smoke"; then
    pass "Branch delete works"
else
    fail "Branch delete failed"
fi

# Stash operations
echo "smoke test content" > smoke-test-file.txt
git stash push -u -m "smoke-test-stash" 2>/dev/null
if git stash list | grep -q "smoke-test-stash"; then
    pass "Stash push works"
else
    fail "Stash push failed"
fi

git stash pop 2>/dev/null
if [ -f smoke-test-file.txt ]; then
    pass "Stash pop works"
    rm -f smoke-test-file.txt
else
    fail "Stash pop failed"
fi

# Diff
DIFF_OUTPUT=$(git diff -- src/core/main.lisp 2>/dev/null)
if [ -n "$DIFF_OUTPUT" ]; then
    pass "git diff produces output for modified file"
else
    fail "git diff produced no output"
fi

# Log
LOG_OUTPUT=$(git log --oneline -5 2>/dev/null)
if [ -n "$LOG_OUTPUT" ]; then
    pass "git log works"
else
    fail "git log failed"
fi

# Blame
BLAME_OUTPUT=$(git blame src/core/main.lisp 2>/dev/null)
if [ -n "$BLAME_OUTPUT" ]; then
    pass "git blame works"
else
    fail "git blame failed"
fi

# Remote URL parsing (simulates what gilt does)
if git remote get-url origin 2>/dev/null | grep -qE "(github|gitlab|git)"; then
    pass "Remote URL retrievable"
else
    skip "Remote URL" "no remote configured (run with GITHUB_REMOTE)"
fi

# ─── 4. Clipboard Tools ──────────────────────────────────────────
section "4. System Tools"

# Check clipboard tools
CLIP_TOOL=""
for tool in xclip xsel wl-copy pbcopy; do
    if command -v "$tool" >/dev/null 2>&1; then
        CLIP_TOOL="$tool"
        break
    fi
done

if [ -n "$CLIP_TOOL" ]; then
    pass "Clipboard tool available: $CLIP_TOOL"
    # Test clipboard round-trip
    echo -n "gilt-test-clipboard" | $CLIP_TOOL -selection clipboard 2>/dev/null || true
    CLIP_CONTENT=$(xclip -selection clipboard -o 2>/dev/null || xsel --clipboard --output 2>/dev/null || echo "")
    if [ "$CLIP_CONTENT" = "gilt-test-clipboard" ]; then
        pass "Clipboard round-trip works"
    else
        skip "Clipboard round-trip" "could not verify (display/wayland issue?)"
    fi
else
    skip "Clipboard tool" "none of xclip/xsel/wl-copy/pbcopy found"
fi

# Check browser tools
BROWSER_TOOL=""
for tool in xdg-open open wslview; do
    if command -v "$tool" >/dev/null 2>&1; then
        BROWSER_TOOL="$tool"
        break
    fi
done

if [ -n "$BROWSER_TOOL" ]; then
    pass "Browser launcher available: $BROWSER_TOOL"
else
    skip "Browser launcher" "none of xdg-open/open/wslview found"
fi

# Check git-flow
if command -v git-flow >/dev/null 2>&1 || git flow version >/dev/null 2>&1; then
    pass "git-flow available"
else
    skip "git-flow" "not installed (apt install git-flow)"
fi

# Check difftool config
DIFFTOOL=$(git config diff.tool 2>/dev/null || echo "")
if [ -n "$DIFFTOOL" ]; then
    pass "Diff tool configured: $DIFFTOOL"
else
    skip "Diff tool" "not configured (git config diff.tool <tool>)"
fi

# ─── 5. Config Directory ─────────────────────────────────────────
section "5. Gilt Config"

GILT_CONFIG_DIR="$HOME/.config/gilt"

if [ -d "$GILT_CONFIG_DIR" ]; then
    pass "Config directory exists: $GILT_CONFIG_DIR"
else
    skip "Config directory" "$GILT_CONFIG_DIR not created yet (run gilt once)"
fi

if [ -f "$GILT_CONFIG_DIR/recent-repos.txt" ]; then
    REPO_COUNT=$(wc -l < "$GILT_CONFIG_DIR/recent-repos.txt")
    pass "Recent repos file exists ($REPO_COUNT entries)"
else
    skip "Recent repos file" "not created yet (run gilt once)"
fi

if [ -f "$GILT_CONFIG_DIR/commands.conf" ]; then
    CMD_COUNT=$(grep -v "^#" "$GILT_CONFIG_DIR/commands.conf" | grep -c "=" 2>/dev/null || echo 0)
    pass "Custom commands file exists ($CMD_COUNT commands)"
else
    skip "Custom commands file" "not created (create ~/.config/gilt/commands.conf)"
fi

# ─── 6. Merge Conflict Setup Verification ────────────────────────
section "6. Merge Conflict Readiness"

# Verify conflict-branch exists and will conflict
CONFLICT_DIFF=$(git diff master..conflict-branch -- README.md 2>/dev/null)
if [ -n "$CONFLICT_DIFF" ]; then
    pass "conflict-branch diverges from master on README.md"
else
    fail "conflict-branch does not diverge (conflict test won't work)"
fi

# ─── 7. File Structure ───────────────────────────────────────────
section "7. File Structure (for tree view testing)"

DIR_COUNT=$(find . -type d -not -path './.git*' | wc -l)
FILE_COUNT=$(find . -type f -not -path './.git*' | wc -l)

if [ "$DIR_COUNT" -ge 4 ]; then
    pass "Multiple directories for tree view ($DIR_COUNT dirs)"
else
    fail "Not enough directories for tree view testing"
fi

if [ "$FILE_COUNT" -ge 8 ]; then
    pass "Multiple files across directories ($FILE_COUNT files)"
else
    fail "Not enough files for testing"
fi

# ─── 8. New Feature Tests (Phase 1 & 2) ─────────────────────────
section "8. New Features (Phase 1 & 2)"

GILT_SRC="$(cd "$SCRIPT_DIR/.." && pwd)"
SBCL_INIT="(progn (require :asdf) (load \"$HOME/quicklisp/setup.lisp\") (push #p\"$GILT_SRC/\" asdf:*central-registry*))"

# Test fuzzy matching via Lisp evaluation
FUZZY_RESULT=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval '(format t "~A ~A ~A ~A"
            (gilt.views::fuzzy-match-p "abc" "aXbYcZ")
            (gilt.views::fuzzy-match-p "abc" "AbBcC")
            (gilt.views::fuzzy-match-p "xyz" "abc")
            (gilt.views::fuzzy-match-p "" "anything"))' \
  2>/dev/null | tail -1)

if [ "$FUZZY_RESULT" = "T T NIL T" ]; then
    pass "Fuzzy matching works correctly (4/4 cases)"
else
    fail "Fuzzy matching returned: '$FUZZY_RESULT' (expected 'T T NIL T')"
fi

# Test git-commit-files returns files for HEAD commit
cd "$REPO_DIR"
COMMIT_FILES=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let ((files (gilt.git:git-commit-files \"HEAD\")))
              (format t \"~D\" (length files))))" \
  2>/dev/null | tail -1)

if [ -n "$COMMIT_FILES" ] && [ "$COMMIT_FILES" -gt 0 ] 2>/dev/null; then
    pass "git-commit-files returns files for HEAD ($COMMIT_FILES files)"
else
    fail "git-commit-files returned: '$COMMIT_FILES'"
fi

# Test git-diff-refs between branches
DIFF_REFS=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let ((diff (gilt.git:git-diff-refs \"master\" \"feature/login\")))
              (format t \"~A\" (if (and diff (> (length diff) 0)) \"OK\" \"EMPTY\"))))" \
  2>/dev/null | tail -1)

if [ "$DIFF_REFS" = "OK" ]; then
    pass "git-diff-refs produces diff between branches"
else
    fail "git-diff-refs returned: '$DIFF_REFS'"
fi

# Test git-diff-refs-stat
DIFF_STAT=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let ((stat (gilt.git:git-diff-refs-stat \"master\" \"feature/login\")))
              (format t \"~A\" (if (and stat (> (length stat) 0)) \"OK\" \"EMPTY\"))))" \
  2>/dev/null | tail -1)

if [ "$DIFF_STAT" = "OK" ]; then
    pass "git-diff-refs-stat produces stat summary"
else
    fail "git-diff-refs-stat returned: '$DIFF_STAT'"
fi

# Test git-move-commits-to-new-branch in temp branch
cd "$REPO_DIR"
git checkout -b test-move-source 2>/dev/null
echo "move test" > move-test.txt
git add move-test.txt 2>/dev/null
git commit -m "commit to move" 2>/dev/null

MOVE_RESULT=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let ((old (gilt.git:git-move-commits-to-new-branch \"test-move-dest\" 1)))
              (format t \"~A\" old)))" \
  2>/dev/null | tail -1)

CURRENT_BRANCH=$(git branch --show-current)
DEST_EXISTS=$(git branch | grep -c "test-move-dest")

if [ "$CURRENT_BRANCH" = "test-move-dest" ] && [ "$DEST_EXISTS" -ge 1 ]; then
    pass "Move commits to new branch works (moved 1 commit to test-move-dest)"
else
    fail "Move commits failed: current=$CURRENT_BRANCH, dest_exists=$DEST_EXISTS, result=$MOVE_RESULT"
fi

# Cleanup move test
git checkout master 2>/dev/null
git branch -D test-move-source 2>/dev/null
git branch -D test-move-dest 2>/dev/null

# Test custom pager detection (should return nil when not configured)
PAGER_RESULT=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let ((pager (gilt.views::detect-custom-pager)))
              (format t \"~A\" (if pager pager \"NONE\"))))" \
  2>/dev/null | tail -1)

if [ -n "$PAGER_RESULT" ]; then
    if [ "$PAGER_RESULT" = "NONE" ]; then
        pass "Custom pager detection works (no pager configured)"
    else
        pass "Custom pager detection works (found: $PAGER_RESULT)"
    fi
else
    fail "Custom pager detection failed"
fi

# Test toast notification functions exist and work
TOAST_RESULT=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (let ((view (make-instance (quote gilt.views::main-view))))
              (gilt.views::show-toast view \"test\" 0.1)
              (format t \"~A ~A\"
                (if (gilt.views::toast-message view) \"MSG-OK\" \"MSG-FAIL\")
                (if (> (gilt.views::toast-expiry view) 0) \"EXP-OK\" \"EXP-FAIL\"))))" \
  2>/dev/null | tail -1)

if [ "$TOAST_RESULT" = "MSG-OK EXP-OK" ]; then
    pass "Toast notification system works"
else
    fail "Toast notification returned: '$TOAST_RESULT'"
fi

# Test format-diff-lines with built-in colorizer
DIFF_FMT=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.views::*custom-pager* nil)
            (let ((lines (gilt.views::format-diff-lines
                          (format nil \"+added~%-removed~%@@ hunk~%plain\"))))
              (format t \"~D\" (length lines))))" \
  2>/dev/null | tail -1)

if [ "$DIFF_FMT" = "4" ]; then
    pass "format-diff-lines produces colored output (4 lines)"
else
    fail "format-diff-lines returned: '$DIFF_FMT'"
fi

# Test nerd font icon system
NERD_RESULT=$(GILT_NERD_FONTS=1 sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.views::*nerd-fonts* :unset)
            (let ((enabled (gilt.views::nerd-fonts-p))
                  (ico (gilt.views::icon :branches)))
              (format t \"~A ~A\"
                (if enabled \"ON\" \"OFF\")
                (if (> (length ico) 0) \"HAS-ICON\" \"NO-ICON\"))))" \
  2>/dev/null | tail -1)

if [ "$NERD_RESULT" = "ON HAS-ICON" ]; then
    pass "Nerd font icons work when enabled (GILT_NERD_FONTS=1)"
else
    fail "Nerd font icons returned: '$NERD_RESULT' (expected 'ON HAS-ICON')"
fi

# Test nerd font icons disabled by default (override git config too)
NERD_OFF=$(GILT_NERD_FONTS="" sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.views::*nerd-fonts* nil)
            (let ((ico (gilt.views::icon :branches)))
              (format t \"~A\" (if (= (length ico) 0) \"EMPTY\" \"NOT-EMPTY\"))))" \
  2>/dev/null | tail -1)

if [ "$NERD_OFF" = "EMPTY" ]; then
    pass "Nerd font icons disabled when not configured"
else
    fail "Nerd font icons should be empty when disabled: '$NERD_OFF'"
fi

# ─── Custom Patch Builder Tests ──────────────────────────────────
section "Custom Patch Builder"

# Test parse-commit-hunks returns hunks for a known commit
cd "$REPO_DIR"
# Make a multi-hunk commit for testing
echo "line1" > patchtest.txt
echo "line2" >> patchtest.txt
echo "line3" >> patchtest.txt
git add patchtest.txt
git commit -m "patch test base" --quiet

echo "changed1" > patchtest.txt
echo "line2" >> patchtest.txt
echo "changed3" >> patchtest.txt
git add patchtest.txt
git commit -m "patch test changes" --quiet

PATCH_HASH=$(git rev-parse HEAD)

PATCH_PARSE=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let ((hunks (gilt.git:parse-commit-hunks \"$PATCH_HASH\" \"patchtest.txt\")))
              (format t \"~D\" (length hunks))))" \
  2>/dev/null | tail -1)

if [ -n "$PATCH_PARSE" ] && [ "$PATCH_PARSE" -gt 0 ] 2>/dev/null; then
    pass "parse-commit-hunks returns $PATCH_PARSE hunk(s) from commit"
else
    fail "parse-commit-hunks returned: '$PATCH_PARSE' (expected >0 hunks)"
fi

# Test build-accumulated-patch produces valid output
PATCH_BUILD=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (let* ((hunks (gilt.git:parse-commit-hunks \"$PATCH_HASH\" \"patchtest.txt\"))
                   (patch (gilt.git:build-full-patch \"patchtest.txt\" hunks)))
              (format t \"~A\" (if (and patch (> (length patch) 0)
                                       (search \"---\" patch)
                                       (search \"+++ \" patch))
                                  \"VALID\" \"INVALID\"))))" \
  2>/dev/null | tail -1)

if [ "$PATCH_BUILD" = "VALID" ]; then
    pass "build-full-patch produces valid patch with file headers"
else
    fail "build-full-patch returned: '$PATCH_BUILD' (expected VALID)"
fi

# Test patch builder state slots exist on main-view
PATCH_SLOTS=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (let ((slots '(gilt.views::patch-builder-mode
                           gilt.views::patch-builder-hash
                           gilt.views::patch-builder-files
                           gilt.views::patch-builder-selected
                           gilt.views::patch-builder-current-file
                           gilt.views::patch-builder-hunk-view)))
              (format t \"~D\"
                (count-if (lambda (s)
                            (find-method (fdefinition s) nil
                              (list (find-class 'gilt.views::main-view)) nil))
                          slots))))" \
  2>/dev/null | tail -1)

if [ "$PATCH_SLOTS" = "6" ]; then
    pass "All 6 patch builder slots exist on main-view"
else
    fail "Patch builder slots: '$PATCH_SLOTS' found (expected 6)"
fi

# ─── Auto-Refresh / Auto-Fetch Tests ────────────────────────────
section "Auto-Refresh / Auto-Fetch"

# Test detect-auto-fetch-config returns defaults
AUTO_CONFIG=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (multiple-value-bind (enabled fetch-int refresh-int)
                (gilt.views::detect-auto-fetch-config)
              (format t \"~A ~D ~D\"
                (if enabled \"ON\" \"OFF\")
                fetch-int refresh-int)))" \
  2>/dev/null | tail -1)

if echo "$AUTO_CONFIG" | grep -qE "^OFF [0-9]+ [0-9]+$"; then
    pass "detect-auto-fetch-config returns defaults ($AUTO_CONFIG)"
else
    fail "detect-auto-fetch-config returned: '$AUTO_CONFIG' (expected 'OFF <int> <int>')"
fi

# Test auto-fetch enabled via env var
AUTO_ENABLED=$(GILT_AUTO_FETCH=1 GILT_FETCH_INTERVAL=30 sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (setf gilt.git:*current-repo* (make-instance (quote gilt.git::git-repository) :path \"$REPO_DIR/\" :name \"test\"))
            (multiple-value-bind (enabled fetch-int refresh-int)
                (gilt.views::detect-auto-fetch-config)
              (format t \"~A ~D\"
                (if enabled \"ON\" \"OFF\")
                fetch-int)))" \
  2>/dev/null | tail -1)

if [ "$AUTO_ENABLED" = "ON 30" ]; then
    pass "Auto-fetch enabled via GILT_AUTO_FETCH=1, interval=30"
else
    fail "Auto-fetch env config returned: '$AUTO_ENABLED' (expected 'ON 30')"
fi

# Test auto-refresh/fetch slots exist on main-view
AUTO_SLOTS=$(sbcl --noinform --non-interactive \
  --eval "$SBCL_INIT" \
  --eval '(ql:quickload :gilt :silent t)' \
  --eval "(progn
            (let ((slots '(gilt.views::auto-refresh-interval
                           gilt.views::auto-fetch-interval
                           gilt.views::auto-fetch-enabled
                           gilt.views::last-refresh-time
                           gilt.views::last-fetch-time
                           gilt.views::fetch-thread
                           gilt.views::fetch-result
                           gilt.views::fetch-in-progress)))
              (format t \"~D\"
                (count-if (lambda (s)
                            (find-method (fdefinition s) nil
                              (list (find-class 'gilt.views::main-view)) nil))
                          slots))))" \
  2>/dev/null | tail -1)

if [ "$AUTO_SLOTS" = "8" ]; then
    pass "All 8 auto-refresh/fetch slots exist on main-view"
else
    fail "Auto-refresh/fetch slots: '$AUTO_SLOTS' found (expected 8)"
fi

# ─── Summary ─────────────────────────────────────────────────────
echo ""
echo "═══════════════════════════════════════════════════"
echo -e "  Results: ${GREEN}$PASS passed${NC}, ${RED}$FAIL failed${NC}, ${YELLOW}$SKIP skipped${NC} / $TOTAL total"
echo "═══════════════════════════════════════════════════"

if [ "$FAIL" -gt 0 ]; then
    echo -e "  ${RED}⚠ Some tests failed. Fix issues before manual TUI testing.${NC}"
else
    echo -e "  ${GREEN}✓ All automated tests passed. Proceed to manual TUI testing.${NC}"
    echo -e "  ${CYAN}  Open tests/TESTING.md and work through the checklist.${NC}"
fi

# Cleanup
if [ "$CLEANUP" = true ]; then
    echo ""
    echo "Temporary repo at: $REPO_DIR"
    echo "To keep it: export GILT_TEST_REPO=$REPO_DIR"
    echo "To clean up: rm -rf $REPO_DIR"
fi

echo ""
exit $FAIL
