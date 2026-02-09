(in-package #:gilt.views)

;;; Views - Main UI screens
;;; LazyGit-style layout: 5 stacked panels on left, main content on right

;;; View base class

(defclass view ()
  ((panels :initarg :panels :accessor view-panels :initform nil)
   (focused-panel :initarg :focused-panel :accessor view-focused-panel :initform 0)
   (needs-refresh :initarg :needs-refresh :accessor view-needs-refresh :initform t)))

(defmethod print-object ((view view) stream)
  (print-unreadable-object (view stream :type t)
    (format stream "~D panels, focused=~D"
            (length (view-panels view))
            (view-focused-panel view))))

(defgeneric draw-view (view width height)
  (:documentation "Draw the view to the terminal"))

(defgeneric handle-key (view key)
  (:documentation "Handle a key event, return :quit to exit, :switch to change view"))

(defgeneric refresh-data (view)
  (:documentation "Refresh data from git"))

(defun show-status-message (message width height)
  "Display a centered status message on screen"
  (let* ((msg-len (length message))
         (x (floor (- width msg-len) 2))
         (y (floor height 2)))
    (cursor-to y x)
    (bg (color-code :bright-yellow))
    (fg (color-code :black))
    (bold)
    (write-string (format nil " ~A " message) *terminal-io*)
    (reset)
    (finish-output *terminal-io*)))

;;; Helper to format panel title with number like LazyGit: [1] Status
(defun numbered-title (num title &optional tabs)
  "Format panel title with number prefix and optional tabs"
  (if tabs
      (format nil "[~D] ~A ~{─ ~A~^ ~}" num title tabs)
      (format nil "[~D] ~A" num title)))

;;; Main View - LazyGit-style with all panels

(defclass main-view (view)
  (;; Left side panels (stacked)
   (status-panel :accessor status-panel)      ; [1] Status
   (files-panel :accessor files-panel)        ; [2] Files  
   (branches-panel :accessor branches-panel)  ; [3] Local branches
   (commits-panel :accessor commits-panel)    ; [4] Commits
   (stash-panel :accessor stash-panel)        ; [5] Stash
   ;; Right side
   (main-panel :accessor main-panel)          ; [0] Main content (diff, etc)
   (cmdlog-panel :accessor cmdlog-panel)      ; Command log panel (lower right)
   ;; Data
   (status-entries :accessor status-entries :initform nil)
   (branch-list :accessor branch-list :initform nil)
   (commit-list :accessor commit-list :initform nil)
   (stash-list :accessor stash-list :initform nil)
   (current-branch :accessor current-branch :initform nil)
   (command-log :accessor command-log :initform nil)  ; List of recent commands
   ;; Dialog state
   (active-dialog :accessor active-dialog :initform nil)
   (screen-width :accessor screen-width :initform 80)
   (screen-height :accessor screen-height :initform 24)
   ;; Hunk staging mode
   (hunk-list :accessor hunk-list :initform nil)
   (hunk-mode :accessor hunk-mode :initform nil)
   ;; Spinner for fetch animation
   (spinner-frame :accessor spinner-frame :initform 0)
   (spinner-active :accessor spinner-active :initform nil)
   ;; Remote branches
   (remote-branch-list :accessor remote-branch-list :initform nil)
   (show-remote-branches :accessor show-remote-branches :initform nil)
   ;; Tags
   (tag-list :accessor tag-list :initform nil)
   (show-tags :accessor show-tags :initform nil)
   ;; Submodules
   (submodule-list :accessor submodule-list :initform nil)
   (show-submodules :accessor show-submodules :initform nil)
   ;; Async process runner for push/pull
   (active-runner :accessor active-runner :initform nil)
   (runner-title :accessor runner-title :initform nil)
   ;; Help overlay
   (help-visible :accessor help-visible :initform nil)
   ;; Blame mode
   (blame-mode :accessor blame-mode :initform nil)
   (blame-data :accessor blame-data :initform nil)
   (blame-file :accessor blame-file :initform nil)
   ;; Cherry-pick mode (from other branches)
   (cherry-pick-mode :accessor cherry-pick-mode :initform nil)
   (cherry-pick-branch :accessor cherry-pick-branch :initform nil)
   (cherry-pick-commits :accessor cherry-pick-commits :initform nil)
   ;; Search commits mode
   (search-mode :accessor search-mode :initform nil)
   (search-query :accessor search-query :initform nil)
   (search-results :accessor search-results :initform nil)
   ;; Config viewer mode
   (config-mode :accessor config-mode :initform nil)
   (config-list :accessor config-list :initform nil)
   (config-scope-filter :accessor config-scope-filter :initform nil)  ; nil=all, :local, :global, :system
   ;; Worktree list
   (worktree-list :accessor worktree-list :initform nil)
   (show-worktrees :accessor show-worktrees :initform nil)
   ;; Stash view toggle (stash-list already exists above)
   (show-stashes :accessor show-stashes :initform nil)
   ;; Interactive rebase mode
   (rebase-mode :accessor rebase-mode :initform nil)
   (rebase-entries :accessor rebase-entries :initform nil)
   (rebase-base-commit :accessor rebase-base-commit :initform nil)
   ;; Diff display options
   (diff-context-size :accessor diff-context-size :initform 3)
   (diff-ignore-whitespace :accessor diff-ignore-whitespace :initform nil)
   ;; Branch sort mode
   (branch-sort-mode :accessor branch-sort-mode :initform :name)
   ;; Bisect mode
   (bisect-mode :accessor bisect-mode :initform nil)
   ;; Panel filter
   (filter-query :accessor filter-query :initform nil)
   (filter-panel :accessor filter-panel :initform nil)
   ;; File tree view mode
   (file-tree-mode :accessor file-tree-mode :initform nil)
   ;; Range select
   (range-select-start :accessor range-select-start :initform nil)
   ;; Screen mode
   (screen-mode :accessor screen-mode :initform :normal)))

(defmethod initialize-instance :after ((view main-view) &key)
  ;; Create all panels
  (setf (status-panel view)
        (make-panel :title (numbered-title 1 "Status") :focused t))
  (setf (files-panel view)
        (make-panel :title (numbered-title 2 "Files" '("Worktrees" "Submodules"))))
  (setf (branches-panel view)
        (make-panel :title (numbered-title 3 "Local branches" '("Remotes" "Tags"))))
  (setf (commits-panel view)
        (make-panel :title (numbered-title 4 "Commits" '("Reflog"))))
  (setf (stash-panel view)
        (make-panel :title (numbered-title 5 "Stash")))
  (setf (main-panel view)
        (make-panel :title "[0] Main"))
  (setf (cmdlog-panel view)
        (make-panel :title "Command Log"))
  ;; Set panel list (order matters for focus cycling) - cmdlog not in cycle
  (setf (view-panels view)
        (list (status-panel view)
              (files-panel view)
              (branches-panel view)
              (commits-panel view)
              (stash-panel view)
              (main-panel view)))
  (refresh-data view))

(defun log-command (view cmd)
  "Add a command to the command log"
  (let ((timestamp (multiple-value-bind (sec min hour) (get-decoded-time)
                     (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec))))
    (push (format nil "~A ~A" timestamp cmd) (command-log view))
    ;; Keep only last 10 commands
    (when (> (length (command-log view)) 10)
      (setf (command-log view) (subseq (command-log view) 0 10)))
    ;; Update cmdlog panel
    (setf (panel-items (cmdlog-panel view)) (command-log view))))

(defmethod refresh-data ((view main-view))
  ;; Status panel - repo info with tracking and state
  (let ((branch (git-current-branch))
        (repo (git-repo-name))
        (state (git-repo-state)))
    (setf (current-branch view) branch)
    ;; Build status items
    (multiple-value-bind (upstream ahead behind)
        (git-branch-tracking-info)
      (let ((items (list (format nil "~A → ~A" repo branch))))
        ;; Add tracking info if available
        (when upstream
          (let ((tracking-str
                  (cond
                    ((and (> ahead 0) (> behind 0))
                     (format nil "  ↑~D ↓~D ~A" ahead behind upstream))
                    ((> ahead 0)
                     (format nil "  ↑~D ~A" ahead upstream))
                    ((> behind 0)
                     (format nil "  ↓~D ~A" behind upstream))
                    (t
                     (format nil "  ≡ ~A" upstream)))))
            (push (list :colored :cyan tracking-str) (cdr (last items)))))
        ;; Add state indicator if in special state
        (when state
          (push (list :colored :bright-red
                      (format nil "  ~A"
                              (case state
                                (:merging "MERGING")
                                (:rebasing "REBASING")
                                (:cherry-picking "CHERRY-PICKING")
                                (:reverting "REVERTING")
                                (:bisecting "BISECTING")
                                (t "OPERATION IN PROGRESS"))))
                (cdr (last items))))
        (setf (panel-items (status-panel view)) items))))
  ;; Files panel - git status, worktrees, or stashes based on toggle
  (let ((entries (git-status))
        (worktrees (git-worktree-list))
        (stashes (git-stash-list)))
    (setf (status-entries view) entries)
    (setf (worktree-list view) worktrees)
    (setf (stash-list view) stashes)
    (cond
      ;; Stashes view
      ((show-stashes view)
       (setf (panel-title (files-panel view))
             (numbered-title 2 "Stashes" '("Files" "Worktrees")))
       (setf (panel-items (files-panel view))
             (if stashes
                 (loop for st in stashes
                       collect (let ((idx (stash-index st))
                                     (branch (stash-branch st))
                                     (msg (stash-message st)))
                                 `(:multi-colored
                                   (:cyan ,(format nil "  ~D: " idx))
                                   (:bright-black ,(if branch (format nil "[~A] " branch) ""))
                                   (:white ,(or msg "WIP")))))
                 (list (list :colored :bright-black "  No stashes")))))
      ;; Worktrees view
      ((show-worktrees view)
       (setf (panel-title (files-panel view))
             (numbered-title 2 "Worktrees" '("Files" "Stashes")))
       (setf (panel-items (files-panel view))
             (if worktrees
                 (loop for wt in worktrees
                       collect (let ((path (worktree-path wt))
                                     (branch (worktree-branch wt))
                                     (bare (worktree-bare wt))
                                     (detached (worktree-detached wt))
                                     (locked (worktree-locked wt)))
                                 (cond
                                   (bare
                                    `(:multi-colored
                                      (:bright-black ,(format nil "  ~A" path))
                                      (:bright-black " (bare)")))
                                   (detached
                                    `(:multi-colored
                                      (:yellow ,(format nil "  ~A" path))
                                      (:bright-black " (detached)")))
                                   (locked
                                    `(:multi-colored
                                      (:red ,(format nil "  ~A" path))
                                      (:bright-black ,(format nil " [~A] (locked)" branch))))
                                   (t
                                    `(:multi-colored
                                      (:green ,(format nil "  ~A" path))
                                      (:bright-black ,(format nil " [~A]" branch)))))))
                 (list (list :colored :bright-black "  No worktrees")))))
      ;; Files view (default)
      (t
       (let ((filtered-entries (if (and (filter-query view) (eql (filter-panel view) 1))
                                   (remove-if-not
                                    (lambda (e) (search (filter-query view)
                                                        (string-downcase (status-entry-file e))))
                                    entries)
                                   entries)))
         (setf (panel-title (files-panel view))
               (cond
                 ((and (filter-query view) (eql (filter-panel view) 1))
                  (format nil "[2] Files (filter: ~A)" (filter-query view)))
                 ((file-tree-mode view)
                  (numbered-title 2 "Files [tree]" '("Worktrees" "Stashes")))
                 (t (numbered-title 2 "Files" '("Worktrees" "Stashes")))))
         (setf (panel-items (files-panel view))
               (if (file-tree-mode view)
                   (format-file-tree filtered-entries)
                   (loop for e in filtered-entries
                         collect (format-status-entry e))))))))
  ;; Branches panel - local, remote, tags, or submodules based on toggle
  (let ((branches (git-branches (branch-sort-mode view)))
        (remote-branches (git-remote-branches))
        (tags (git-tags))
        (submodules (git-submodules)))
    (setf (branch-list view) branches)
    (setf (remote-branch-list view) remote-branches)
    (setf (tag-list view) tags)
    (setf (submodule-list view) submodules)
    ;; Update panel title and items based on mode
    (cond
      ((show-submodules view)
       (setf (panel-title (branches-panel view))
             (numbered-title 3 "Submodules" '("Local" "Remotes")))
       (setf (panel-items (branches-panel view))
             (if submodules
                 (loop for sm in submodules
                       collect (let ((name (submodule-name sm))
                                     (status (submodule-status sm))
                                     (commit (submodule-commit sm)))
                                 (case status
                                   (:uninitialized
                                    `(:multi-colored
                                      (:red ,(format nil "- ~A" name))
                                      (:bright-black " (not initialized)")))
                                   (:modified
                                    `(:multi-colored
                                      (:yellow ,(format nil "+ ~A" name))
                                      (:bright-black ,(format nil " ~A (modified)" commit))))
                                   (t
                                    `(:multi-colored
                                      (:green ,(format nil "  ~A" name))
                                      (:bright-black ,(format nil " ~A" commit)))))))
                 (list (list :colored :bright-black "  No submodules")))))
      ((show-tags view)
       (setf (panel-title (branches-panel view))
             (numbered-title 3 "Tags" '("Submodules" "Local")))
       (setf (panel-items (branches-panel view))
             (loop for tag in tags
                   collect (let ((name (tag-name tag))
                                 (date (or (tag-date tag) ""))
                                 (annotated (eq (tag-type tag) :annotated)))
                             (if annotated
                                 `(:multi-colored
                                   (:bright-yellow ,(format nil "  ~A" name))
                                   (:bright-black ,(format nil " ~A" date)))
                                 `(:multi-colored
                                   (:yellow ,(format nil "  ~A" name))
                                   (:bright-black ,(format nil " ~A" date))))))))
      ((show-remote-branches view)
       (setf (panel-title (branches-panel view))
             (numbered-title 3 "Remotes" '("Tags" "Submodules")))
       (setf (panel-items (branches-panel view))
             (loop for b in remote-branches
                   collect (list :colored :bright-cyan (format nil "  ~A" b)))))
      (t
       (let ((filtered-branches (if (and (filter-query view) (eql (filter-panel view) 2))
                                    (remove-if-not
                                     (lambda (b) (search (filter-query view) (string-downcase b)))
                                     branches)
                                    branches)))
         (setf (panel-title (branches-panel view))
               (if (and (filter-query view) (eql (filter-panel view) 2))
                   (format nil "[3] Local (filter: ~A)" (filter-query view))
                   (numbered-title 3 "Local" '("Remotes" "Tags" "Submodules"))))
         (setf (panel-items (branches-panel view))
               (loop for b in filtered-branches
                     collect (if (string= b (current-branch view))
                                 (list :colored :bright-green (format nil "* ~A" b))
                                 (format nil "  ~A" b))))))))
  ;; Commits panel - show hash (yellow), author initials, circle, and message
  (let ((commits (git-log :count 50)))
    (setf (commit-list view) commits)
    (setf (panel-items (commits-panel view))
          (loop for c in commits
                for i from 0
                collect (format-commit-entry c (= i 0)))))
  ;; Stash panel
  (let ((stashes (git-stash-list)))
    (setf (stash-list view) stashes)
    (setf (panel-items (stash-panel view)) stashes))
  ;; Main panel - show diff for selected file
  (update-main-content view))

(defun format-status-entry (entry)
  "Format a status entry for display with color.
   Staged files are shown in green, unstaged in their status color."
  (let* ((status (status-entry-status entry))
         (staged (status-entry-staged-p entry))
         (indicator (case status
                      (:modified "M")
                      (:added "A")
                      (:deleted "D")
                      (:untracked "?")
                      (:renamed "R")
                      (:conflict "!")
                      (t " ")))
         (color (if staged
                    :bright-green  ; All staged files shown in green
                    (case status
                      (:modified :bright-yellow)
                      (:added :bright-green)
                      (:deleted :bright-red)
                      (:untracked :bright-magenta)
                      (:renamed :bright-cyan)
                      (:conflict :bright-red)
                      (t :white))))
         (text (format nil "~A ~A" indicator (status-entry-file entry))))
    (list :colored color text)))

(defun format-file-tree (entries)
  "Format status entries as a tree view, grouping by directory."
  (let ((dirs (make-hash-table :test 'equal))
        (result nil))
    ;; Group files by directory
    (dolist (e entries)
      (let* ((file (status-entry-file e))
             (slash-pos (position #\/ file :from-end t))
             (dir (if slash-pos (subseq file 0 slash-pos) ".")))
        (push e (gethash dir dirs))))
    ;; Sort directories and format
    (let ((sorted-dirs (sort (loop for k being the hash-keys of dirs collect k) #'string<)))
      (dolist (dir sorted-dirs)
        ;; Directory header
        (unless (string= dir ".")
          (push (list :colored :bright-cyan (format nil "  ~A/" dir)) result))
        ;; Files in this directory
        (dolist (e (reverse (gethash dir dirs)))
          (let* ((file (status-entry-file e))
                 (slash-pos (position #\/ file :from-end t))
                 (basename (if slash-pos (subseq file (1+ slash-pos)) file))
                 (status (status-entry-status e))
                 (indicator (case status
                              (:modified "M") (:added "A") (:deleted "D")
                              (:untracked "?") (:renamed "R") (:conflict "!") (t " ")))
                 (color (case status
                          (:modified :bright-yellow) (:added :bright-green)
                          (:deleted :bright-red) (:untracked :bright-magenta)
                          (:renamed :bright-cyan) (:conflict :bright-red) (t :white)))
                 (indent (if (string= dir ".") "  " "    ")))
            (push (list :colored color (format nil "~A~A ~A" indent indicator basename)) result)))))
    (nreverse result)))

(defun get-author-initials (author)
  "Extract initials from author name (e.g., 'Glenn Thompson' -> 'GT')"
  (if (and author (> (length author) 0))
      (let* ((clean (string-trim '(#\Space #\Tab) author))
             (parts (cl-ppcre:split "\\s+" clean)))
        (if (> (length parts) 1)
            ;; Multiple words - take first letter of each
            (format nil "~{~A~}" 
                    (loop for p in parts 
                          when (> (length p) 0)
                          collect (char-upcase (char p 0))))
            ;; Single word - take first 2 chars
            (string-upcase (subseq clean 0 (min 2 (length clean))))))
      "??"))

(defun format-commit-entry (commit is-head)
  "Format a commit entry with colored hash, initials, and indicator"
  ;; Format: hash initials ○ message
  ;; The panel renderer will handle this as a multi-segment colored item
  (let* ((hash (log-entry-short-hash commit))
         (author (log-entry-author commit))
         (initials (get-author-initials author))
         (message (log-entry-message commit))
         (indicator (if is-head "●" "○")))
    ;; Return as colored segments list for rich rendering
    (list :multi-colored
          (list :bright-yellow hash)
          (list :bright-cyan (format nil " ~A " initials))
          (list :bright-green indicator)
          (list :white (format nil " ~A" message)))))

(defun format-diff-lines (diff-text)
  "Format diff text with colors for display"
  (loop for line in (cl-ppcre:split "\\n" diff-text)
        collect (cond
                  ;; Added lines - green
                  ((and (> (length line) 0) (char= (char line 0) #\+))
                   (list :colored :green line))
                  ;; Removed lines - red
                  ((and (> (length line) 0) (char= (char line 0) #\-))
                   (list :colored :red line))
                  ;; Hunk headers - cyan
                  ((and (> (length line) 1) (string= (subseq line 0 2) "@@"))
                   (list :colored :cyan line))
                  ;; File headers - yellow
                  ((or (and (> (length line) 4) (string= (subseq line 0 4) "diff"))
                       (and (> (length line) 3) (string= (subseq line 0 3) "---"))
                       (and (> (length line) 3) (string= (subseq line 0 3) "+++")))
                   (list :colored :yellow line))
                  ;; Index/mode lines - bright-black
                  ((and (> (length line) 5) (string= (subseq line 0 5) "index"))
                   (list :colored :bright-black line))
                  ;; Plain text
                  (t line))))

(defun update-rebase-display (view)
  "Update the commits panel to show the interactive rebase todo list."
  (let ((entries (rebase-entries view)))
    (setf (panel-items (commits-panel view))
          (loop for entry in entries
                collect (let ((action (rebase-action entry))
                              (short-hash (rebase-short-hash entry))
                              (msg (rebase-message entry)))
                          (case action
                            (:pick
                             `(:multi-colored
                               (:green ,(format nil "pick   "))
                               (:yellow ,short-hash)
                               (:white ,(format nil " ~A" msg))))
                            (:reword
                             `(:multi-colored
                               (:cyan ,(format nil "reword "))
                               (:yellow ,short-hash)
                               (:white ,(format nil " ~A" 
                                                (or (rebase-new-message entry) msg)))))
                            (:squash
                             `(:multi-colored
                               (:magenta ,(format nil "squash "))
                               (:yellow ,short-hash)
                               (:white ,(format nil " ~A" msg))))
                            (:fixup
                             `(:multi-colored
                               (:magenta ,(format nil "fixup  "))
                               (:yellow ,short-hash)
                               (:white ,(format nil " ~A" msg))))
                            (:drop
                             `(:multi-colored
                               (:red ,(format nil "drop   "))
                               (:bright-black ,short-hash)
                               (:bright-black ,(format nil " ~A" msg))))
                            (t
                             `(:multi-colored
                               (:white ,(format nil "~6A " action))
                               (:yellow ,short-hash)
                               (:white ,(format nil " ~A" msg))))))))))

(defun update-main-content (view)
  "Update main panel based on focused panel and selection"
  (let* ((focused-idx (view-focused-panel view))
         (panel (nth focused-idx (view-panels view)))
         (selected (panel-selected panel)))
    (cond
      ;; Files panel focused - show diff, worktree info, or stash diff
      ((= focused-idx 1)
       (cond
         ;; Stashes view - show stash diff
         ((show-stashes view)
          (let ((stashes (stash-list view)))
            (if (and stashes (< selected (length stashes)))
                (let* ((st (nth selected stashes))
                       (diff (git-stash-show (stash-index st))))
                  (setf (panel-title (main-panel view)) "[0] Stash Diff")
                  (setf (panel-items (main-panel view))
                        (format-diff-lines diff)))
                (progn
                  (setf (panel-title (main-panel view)) "[0] Stash")
                  (setf (panel-items (main-panel view)) nil)))))
         ;; Worktrees view - show worktree info
         ((show-worktrees view)
          (setf (panel-title (main-panel view)) "[0] Worktree")
          (setf (panel-items (main-panel view)) nil))
         ;; Files view - show file diff
         (t
          (let ((entries (status-entries view)))
            (when (and entries (< selected (length entries)))
              (let* ((entry (nth selected entries))
                     (file (status-entry-file entry))
                     (diff (if (status-entry-staged-p entry)
                               (git-diff-staged file
                                                :context-size (diff-context-size view)
                                                :ignore-whitespace (diff-ignore-whitespace view))
                               (git-diff file
                                         :context-size (diff-context-size view)
                                         :ignore-whitespace (diff-ignore-whitespace view)))))
                (setf (panel-title (main-panel view)) "[0] Diff")
                (setf (panel-items (main-panel view))
                      (format-diff-lines diff))))))))
      ;; Commits panel focused - show commit details with full message
      ((= focused-idx 3)
       (let ((commits (commit-list view)))
         (when (and commits (< selected (length commits)))
           (let* ((commit (nth selected commits))
                  (full-message (git-commit-message (log-entry-hash commit)))
                  (message-lines (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Space) full-message))))
             (setf (panel-title (main-panel view)) "[0] Commit")
             (setf (panel-items (main-panel view))
                   (append (list (format nil "Hash: ~A" (log-entry-hash commit))
                                 (format nil "Author: ~A" (log-entry-author commit))
                                 (format nil "Date: ~A" (log-entry-date commit))
                                 "")
                           message-lines))))))
      ;; Branches panel focused
      ((= focused-idx 2)
       (setf (panel-title (main-panel view)) "[0] Branch Info")
       (setf (panel-items (main-panel view)) nil))
      ;; Default
      (t
       (setf (panel-title (main-panel view)) "[0] Main")
       (setf (panel-items (main-panel view)) nil)))))

(defun get-panel-help (focused-idx)
  "Return context-specific help bindings based on focused panel"
  (case focused-idx
    (0 ; Status panel
     '(("j/k" . "navigate") ("Tab" . "panels") ("r" . "refresh") ("q" . "quit")))
    (1 ; Files panel
     '(("j/k" . "navigate") ("Space" . "stage/unstage") ("e" . "edit/hunks") ("d" . "discard")
       ("c" . "commit") ("v" . "range") ("T" . "tree") ("I" . "ignore") ("x" . "difftool") ("y" . "copy") ("r" . "refresh") ("q" . "quit")))
    (2 ; Branches panel
     '(("j/k" . "navigate") ("Enter" . "checkout/track") ("n" . "new") ("N" . "rename")
       ("w" . "local/remote") ("M" . "merge") ("R" . "rebase") ("F" . "ff") ("s" . "sort") ("D" . "delete") ("r" . "refresh") ("q" . "quit")))
    (3 ; Commits panel
     '(("j/k" . "navigate") ("i" . "rebase") ("X" . "reset") ("A" . "amend") ("C" . "cherry-pick") ("R" . "revert") ("S" . "squash") ("F" . "fixup") ("t" . "tag") ("b" . "bisect") ("o" . "browser") ("r" . "refresh") ("q" . "quit")))
    (4 ; Stash panel
     '(("j/k" . "navigate") ("s" . "stash") ("g" . "pop") ("r" . "refresh") ("q" . "quit")))
    (t ; Default
     '(("j/k" . "navigate") ("Tab" . "panels") ("r" . "refresh") ("q" . "quit")))))

(defmethod draw-view ((view main-view) width height)
  ;; LazyGit layout:
  ;; Left column: 5 stacked panels - focused panel expands (wider ~40%)
  ;; Right column: main content panel (top) + command log (bottom)
  (let* ((left-width (case (screen-mode view)
                       (:half (max 50 (floor width 2)))        ; 50% left
                       (:full (- width 2))                      ; Nearly full width (no right panel)
                       (t (max 50 (floor (* width 2) 5)))))    ; Normal: ~40% left
         (right-width (max 0 (- width left-width)))
         (usable-height (max 1 (- height 1)))  ; Leave 1 row for help bar
         (focused-idx (view-focused-panel view))
         ;; Right side split: main panel gets most, cmdlog gets 9 rows
         (cmdlog-height 9)
         (main-height (max 3 (- usable-height cmdlog-height)))
         ;; Base heights for left panels - small for unfocused, larger for focused
         (min-h 3)  ; Minimum height for collapsed panels
         ;; Calculate expanded height for focused panel
         (total-min (* 5 min-h))
         (extra-space (max 0 (- usable-height total-min)))
         (expanded-extra (floor extra-space 2))  ; Focused gets half the extra
         (other-extra (floor (- extra-space expanded-extra) 4))  ; Rest split among others
         ;; Calculate individual heights
         (heights (loop for i from 0 below 5
                        collect (if (= i focused-idx)
                                    (+ min-h expanded-extra)
                                    (+ min-h other-extra))))
         ;; Adjust last panel to fill remaining space
         (total-h (reduce #'+ heights))
         (adjustment (- usable-height total-h)))
    ;; Add adjustment to last panel
    (setf (nth 4 heights) (+ (nth 4 heights) adjustment))
    ;; Calculate Y positions - accumulate heights
    (let ((y-positions (let ((y 1)
                             (positions nil))
                         (dolist (h heights (nreverse positions))
                           (push y positions)
                           (setf y (+ y h))))))
      ;; Position left panels with dynamic heights
      (setf (panel-x (status-panel view)) 1
            (panel-y (status-panel view)) (nth 0 y-positions)
            (panel-width (status-panel view)) left-width
            (panel-height (status-panel view)) (nth 0 heights))
      (setf (panel-x (files-panel view)) 1
            (panel-y (files-panel view)) (nth 1 y-positions)
            (panel-width (files-panel view)) left-width
            (panel-height (files-panel view)) (nth 1 heights))
      (setf (panel-x (branches-panel view)) 1
            (panel-y (branches-panel view)) (nth 2 y-positions)
            (panel-width (branches-panel view)) left-width
            (panel-height (branches-panel view)) (nth 2 heights))
      (setf (panel-x (commits-panel view)) 1
            (panel-y (commits-panel view)) (nth 3 y-positions)
            (panel-width (commits-panel view)) left-width
            (panel-height (commits-panel view)) (nth 3 heights))
      (setf (panel-x (stash-panel view)) 1
            (panel-y (stash-panel view)) (nth 4 y-positions)
            (panel-width (stash-panel view)) left-width
            (panel-height (stash-panel view)) (nth 4 heights)))
    ;; Position main panel (right side, upper portion)
    (setf (panel-x (main-panel view)) (1+ left-width)
          (panel-y (main-panel view)) 1
          (panel-width (main-panel view)) right-width
          (panel-height (main-panel view)) main-height)
    ;; Position command log panel (right side, lower portion)
    (setf (panel-x (cmdlog-panel view)) (1+ left-width)
          (panel-y (cmdlog-panel view)) (1+ main-height)
          (panel-width (cmdlog-panel view)) right-width
          (panel-height (cmdlog-panel view)) cmdlog-height)
    ;; Update focus state - but only if main panel is not explicitly focused
    (let ((main-panel-focused (panel-focused (main-panel view))))
      (if main-panel-focused
          ;; Main panel is focused - unfocus all LEFT panels only (not main panel)
          (loop for panel in (view-panels view)
                for i from 0 below 5  ; Only first 5 panels (left side)
                do (setf (panel-focused panel) nil))
          ;; Normal mode - focus the selected left panel
          (progn
            (loop for panel in (view-panels view)
                  for i from 0
                  do (setf (panel-focused panel) (= i focused-idx)))
            ;; Main panel gets focus in blame mode or cherry-pick mode
            (setf (panel-focused (main-panel view))
                  (or (blame-mode view) (cherry-pick-mode view))))))
    ;; Draw all panels (includes main-panel which is 6th in view-panels)
    (dolist (panel (view-panels view))
      (draw-panel panel))
    ;; Draw command log panel (not in focus cycle)
    (draw-panel (cmdlog-panel view))
    ;; Draw context-specific help bar at bottom (with version from gilt package)
    (draw-help-bar height width (get-panel-help focused-idx) 
                   (when (find-package :gilt) 
                     (symbol-value (find-symbol "*VERSION*" :gilt))))
    ;; Draw spinner in bottom left if active
    (when (spinner-active view)
      (draw-spinner view height))
    ;; Store screen dimensions for dialogs
    (setf (screen-width view) width
          (screen-height view) height)
    ;; Draw active dialog on top if present
    (when (active-dialog view)
      (draw-dialog (active-dialog view) width height))
    ;; Draw help overlay on top of everything
    (when (help-visible view)
      (draw-help-overlay width height))))

(defun draw-help-overlay (width height)
  "Draw the help overlay showing all keybindings"
  (let* ((content-width 52)  ; Fixed content width
         (box-width (+ content-width 4))  ; Add space for borders and padding
         (help-lines '("        GILT - Git Interface for Lisp Terminal"
                       ""
                       " NAVIGATION"
                       "   1-5        Switch to panel by number"
                       "   Tab/l      Next panel    h  Previous panel"
                       "   j/Down     Move down     k/Up  Move up"
                       "   Enter      Select/expand item"
                       ""
                       " FILES (panel 2)"
                       "   Space      Stage/unstage file"
                       "   a          Stage all files"
                       "   d          Discard changes (unstaged)"
                       "   e          Edit file / enter hunk mode"
                       "   b          Blame view (Enter for commit info)"
                       "   o          Resolve conflict: keep ours"
                       "   t          Resolve conflict: keep theirs"
                       "   X          Abort merge"
                       "   w          Cycle: Files/Worktrees/Stashes"
                       "   A          Add worktree (in Worktrees view)"
                       "   D          Remove worktree/Drop stash"
                       "   P          Pop stash (in Stashes view)"
                       "   Enter      Apply stash (in Stashes view)"
                       "   T          Toggle file tree view (flat/tree)"
                       "   v          Range select (start/end, then stage/unstage)"
                       "   I          Add file to .gitignore"
                       "   x          Launch external diff tool"
                       "   y          Copy file path to clipboard"
                       "   W          Commit without pre-commit hook"
                       ""
                       " COMMITS (panel 4)"
                       "   /          Search commits"
                       "   t          Create tag on commit"
                       "   c          New commit (Ctrl+D to submit)"
                       "   C          Commit with $EDITOR"
                       "   A          Amend HEAD commit"
                       "   X          Reset to commit (soft/mixed/hard)"
                       "   F          Create fixup! commit"
                       "   S          Squash commits"
                       "   C          Cherry-pick commit"
                       "   R          Revert commit"
                       "   i          Interactive rebase (select range)"
                       "   b          Bisect (start, then b:bad g:good Q:reset)"
                       ""
                       " BRANCHES (panel 3)"
                       "   n          New branch"
                       "   N          Rename branch"
                       "   Enter      Checkout branch"
                       "   M          Merge branch into current"
                       "   R          Rebase current onto branch"
                       "   F          Fast-forward branch to upstream"
                       "   u          Set/unset upstream tracking branch"
                       "   s          Sort branches (name/date/recent)"
                       "   D          Delete branch/tag/remote branch"
                       "   C          Cherry-pick from branch"
                       "   y          Copy branch name to clipboard"
                       "   o          Open branch in browser"
                       "   w          Cycle: Local/Remotes/Tags/Submodules"
                       "   Enter      Enter submodule (in Submodules view)"
                       "   t          Create tag (in Tags view)"
                       "   T          Push tag to remote (in Tags view)"
                       "   Space      Checkout tag (detached HEAD, in Tags view)"
                       "   f          Fetch (select remote)"
                       "   A          Add remote (in Remotes view)"
                       "   R          Rename remote (in Remotes view)"
                       "   U          Update submodule (in Submodules view)"
                       ""
                       " CONFIG"
                       "   G          Toggle config viewer"
                       "   w          Cycle scope (All/Local/Global/System)"
                       ""
                       " REMOTE"
                       "   p          Pull from origin"
                       "   P          Push to origin"
                       ""
                       " STASH (panel 5)"
                       "   s          Stash changes"
                       "   g          Pop stash"
                       "   Enter      Apply stash"
                       "   B          New branch from stash (in Stashes view)"
                       "   R          Rename stash (in Stashes view)"
                       ""
                       " SEARCH / FILTER"
                       "   /          Search commits (panel 4) or filter (panels 2,3,5)"
                       ""
                       " CLIPBOARD / BROWSER"
                       "   y          Copy (file path, branch name, or commit hash)"
                       "   o          Open in browser (commit or branch URL)"
                       ""
                       " SHELL"
                       "   :          Run shell command (vim-style)"
                       ""
                       " NAVIGATION"
                       "   PgUp/PgDn  Page up/down in lists"
                       ""
                       " LAYOUT"
                       "   +          Cycle screen mode: normal/half/full"
                       ""
                       " RECENT REPOS"
                       "   L          Show recent repos (Enter to switch)"
                       ""
                       " PULL REQUEST"
                       "   O          Create PR for selected branch (opens browser)"
                       ""
                       " GIT FLOW"
                       "   E          Git-flow menu (feature/release/hotfix)"
                       ""
                       " CUSTOM COMMANDS"
                       "              Define in ~/.config/gilt/commands.conf"
                       "              Format: key=shell command (one per line)"
                       ""
                       " DIFF OPTIONS"
                       "   }          Increase diff context lines"
                       "   {          Decrease diff context lines"
                       "   W          Toggle whitespace in diffs"
                       ""
                       " UNDO/REDO"
                       "   z          Undo last git command (via reflog)"
                       "   Z          Redo last undone command"
                       ""
                       " OTHER"
                       "   r          Refresh all panels"
                       "   ?          Toggle this help"
                       "   q          Quit"
                       ""
                       "            Press any key to close"))
         (help-height (min (+ (length help-lines) 2) (- height 2)))
         (start-x (max 1 (floor (- width box-width) 2)))
         (start-y (max 1 (floor (- height help-height) 2))))
    ;; Set style - cyan on black (less harsh than blue)
    (fg (color-code :cyan))
    (bg (color-code :black))
    ;; Top border
    (cursor-to start-y start-x)
    (write-string (concatenate 'string "╔" (make-string (+ content-width 2) :initial-element #\═) "╗") *terminal-io*)
    ;; Content lines
    (loop for line in help-lines
          for y from (1+ start-y)
          for i from 0
          while (< i (- help-height 2))
          do (cursor-to y start-x)
             (write-string "║ " *terminal-io*)
             ;; Check if this is a section header (starts with space then uppercase)
             (let* ((is-header (and (> (length line) 1)
                                    (char= (char line 0) #\Space)
                                    (upper-case-p (char line 1))))
                    (max-len content-width)
                    (content (if (> (length line) max-len)
                                (subseq line 0 max-len)
                                line))
                    (padding (- max-len (length content))))
               (when is-header (bold))
               (write-string content *terminal-io*)
               (when is-header (reset) (fg (color-code :cyan)) (bg (color-code :black)))
               (write-string (make-string padding :initial-element #\Space) *terminal-io*))
             (write-string " ║" *terminal-io*))
    ;; Bottom border
    (cursor-to (+ start-y help-height -1) start-x)
    (write-string (concatenate 'string "╚" (make-string (+ content-width 2) :initial-element #\═) "╝") *terminal-io*)
    (reset)
    (finish-output *terminal-io*)))

(defgeneric format-blame-line (blame-line)
  (:documentation "Format a blame line for display with colors"))

(defmethod format-blame-line ((bl blame-line))
  "Format a blame-line object as a multi-colored panel item"
  (let* ((short-hash (blame-line-short-hash bl))
         (author (blame-line-author bl))
         (line-num (blame-line-num bl))
         (content (blame-line-content bl))
         ;; Truncate author to 12 chars
         (author-display (if (> (length author) 12)
                             (subseq author 0 12)
                             (format nil "~12A" author))))
    `(:multi-colored
      (:yellow ,short-hash)
      (:white " ")
      (:cyan ,author-display)
      (:white " ")
      (:bright-black ,(format nil "~4D" line-num))
      (:white " │ ")
      (:white ,content))))

(defgeneric format-cherry-pick-commit (commit)
  (:documentation "Format a commit for cherry-pick display with colors"))

(defmethod format-cherry-pick-commit ((c log-entry))
  "Format a log-entry for cherry-pick selection"
  (let* ((short-hash (log-entry-short-hash c))
         (author (log-entry-author c))
         (date (log-entry-date c))
         (msg (log-entry-message c))
         ;; Truncate author to 10 chars
         (author-display (if (> (length author) 10)
                             (subseq author 0 10)
                             (format nil "~10A" author))))
    `(:multi-colored
      (:yellow ,short-hash)
      (:white " ")
      (:cyan ,author-display)
      (:white " ")
      (:bright-black ,date)
      (:white " ")
      (:green ,msg))))

(defgeneric format-search-result (commit query)
  (:documentation "Format a commit for search results with query highlighted"))

(defmethod format-search-result ((c log-entry) query)
  "Format a log-entry for search results display"
  (let* ((short-hash (log-entry-short-hash c))
         (author (log-entry-author c))
         (date (log-entry-date c))
         (msg (log-entry-message c))
         ;; Truncate author to 10 chars
         (author-display (if (> (length author) 10)
                             (subseq author 0 10)
                             (format nil "~10A" author)))
         ;; Check if query matches in message or author
         (msg-match (search (string-downcase query) (string-downcase msg)))
         (author-match (search (string-downcase query) (string-downcase author))))
    `(:multi-colored
      (:yellow ,short-hash)
      (:white " ")
      ,(if author-match
           `(:bright-cyan ,author-display)
           `(:cyan ,author-display))
      (:white " ")
      (:bright-black ,date)
      (:white " ")
      ,(if msg-match
           `(:bright-green ,msg)
           `(:green ,msg)))))

(defparameter *spinner-chars* '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner animation frames")

(defun update-runner (view)
  "Poll active runner and update main panel with output. Returns t if runner is active."
  (when (active-runner view)
    (let ((runner (active-runner view)))
      ;; Poll for new output
      (runner-poll runner)
      ;; Update main panel with output
      (let ((output (runner-get-output runner)))
        (setf (panel-items (main-panel view))
              (if output
                  (append output
                          (if (runner-finished-p runner)
                              (list "" 
                                    (format nil "--- Finished (exit code: ~D) ---" 
                                            (runner-exit-code runner))
                                    "Press any key to continue...")
                              (list "" (runner-title view))))
                  (list (runner-title view)))))
      ;; Check if finished and user pressed key
      (when (runner-finished-p runner)
        ;; Runner stays active until user dismisses
        t)
      t)))

(defun draw-spinner (view row)
  "Draw spinning fetch indicator in bottom left"
  (cursor-to row 2)
  (fg (color-code :bright-magenta))
  (let ((frame (mod (spinner-frame view) (length *spinner-chars*))))
    (write-string (nth frame *spinner-chars*) *terminal-io*)
    (write-string " Fetching..." *terminal-io*))
  (reset)
  (finish-output *terminal-io*)
  ;; Advance spinner frame
  (incf (spinner-frame view)))

(defmethod handle-key ((view main-view) key)
  ;; If help overlay is visible, any key dismisses it
  (when (help-visible view)
    (setf (help-visible view) nil)
    (return-from handle-key nil))
  
  ;; Toggle help with ?
  (when (and (key-event-char key) (char= (key-event-char key) #\?))
    (setf (help-visible view) t)
    (return-from handle-key nil))
  
  ;; If runner is active, handle runner state
  (when (active-runner view)
    (let ((runner (active-runner view)))
      ;; Poll for updates
      (runner-poll runner)
      ;; Update display
      (let ((output (runner-get-output runner)))
        (setf (panel-items (main-panel view))
              (append output
                      (if (runner-finished-p runner)
                          (list "" 
                                (format nil "--- Finished (exit code: ~D) ---" 
                                        (runner-exit-code runner))
                                "Press any key to continue...")
                          (list "" (runner-title view))))))
      ;; If finished and any key pressed, dismiss runner
      (when (runner-finished-p runner)
        (runner-stop runner)
        (setf (active-runner view) nil)
        (setf (runner-title view) nil)
        (log-command view "Command completed")
        (refresh-data view)
        ;; Return nil to trigger full re-render (not :dialog)
        (return-from handle-key nil))
      ;; Runner still running - consume the key
      (return-from handle-key :dialog)))  ; :dialog prevents re-render since we handle it
  
  ;; If dialog is active, ALL keys go to dialog - nothing else processes them
  (when (active-dialog view)
    (let ((result (handle-dialog-key (active-dialog view) key)))
      (cond
        ((eq result :ok)
         ;; Handle dialog confirmation based on dialog type
         (let ((dlg (active-dialog view)))
           (cond
             ;; Commit dialog
             ((string= (dialog-title dlg) "Commit Message")
              (let ((msg (dialog-get-text dlg)))
                (when (> (length msg) 0)
                  (log-command view (format nil "git commit -m \"~A\"" 
                                            (first (dialog-input-lines dlg))))
                  (git-commit msg)
                  (refresh-data view))))
             ;; Commit (no hook) dialog
             ((string= (dialog-title dlg) "Commit (no hook)")
              (let ((msg (dialog-get-text dlg)))
                (when (> (length msg) 0)
                  (log-command view (format nil "git commit --no-verify -m \"~A\""
                                            (first (dialog-input-lines dlg))))
                  (git-commit-no-verify msg)
                  (refresh-data view))))
             ;; New Branch dialog
             ((string= (dialog-title dlg) "New Branch")
              (let ((name (first (dialog-input-lines dlg))))
                (when (and name (> (length name) 0))
                  (log-command view (format nil "git checkout -b ~A" name))
                  (git-create-branch name)
                  (refresh-data view))))
             ;; Merge Branch dialog
             ((string= (dialog-title dlg) "Merge Branch")
              (let* ((msg (dialog-message dlg))
                     (parts (cl-ppcre:split "\\|" msg))
                     (branch (first parts))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when branch
                  (cond
                    ((string= selected-button "Merge")
                     (log-command view (format nil "git merge ~A" branch))
                     (git-merge branch)
                     (refresh-data view))
                    ((string= selected-button "Squash")
                     (log-command view (format nil "git merge --squash ~A" branch))
                     (git-merge-squash branch)
                     (refresh-data view))))))
             ;; Delete Branch dialog
             ((string= (dialog-title dlg) "Delete Branch")
              (let* ((msg (dialog-message dlg))
                     (branch (cl-ppcre:scan-to-strings "Delete branch ([^?]+)" msg)))
                (when branch
                  ;; Extract branch name from message
                  (multiple-value-bind (match regs)
                      (cl-ppcre:scan-to-strings "Delete branch ([^?]+)" msg)
                    (declare (ignore match))
                    (when regs
                      (let ((branch-name (string-trim " " (aref regs 0))))
                        (log-command view (format nil "git branch -d ~A" branch-name))
                        (git-delete-branch branch-name)
                        (refresh-data view)))))))
             ;; Delete Tag dialog
             ((string= (dialog-title dlg) "Delete Tag")
              (let ((tag-name (getf (dialog-data dlg) :tag-name)))
                (when tag-name
                  (log-command view (format nil "git tag -d ~A" tag-name))
                  (git-delete-tag tag-name)
                  (refresh-data view))))
             ;; Create Tag dialog
             ((string= (dialog-title dlg) "Create Tag")
              (let ((name (first (dialog-input-lines dlg)))
                    (commit-hash (getf (dialog-data dlg) :commit-hash)))
                (when (and name (> (length name) 0))
                  (if commit-hash
                      (progn
                        (log-command view (format nil "git tag ~A ~A" name commit-hash))
                        (git-run "tag" name commit-hash))
                      (progn
                        (log-command view (format nil "git tag ~A" name))
                        (git-create-tag name)))
                  (refresh-data view))))
             ;; Push dialog - start async runner
             ((string= (dialog-title dlg) "Push")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-button (nth (dialog-selected-button dlg) buttons))
                     (force-p (string= selected-button "Force Push"))
                     (has-upstream (git-branch-has-upstream-p))
                     (branch (git-current-branch))
                     (push-cmd (cond
                                 ((and force-p has-upstream)
                                  '("git" "push" "--force-with-lease"))
                                 (force-p
                                  (list "git" "push" "--force-with-lease"
                                        "--set-upstream" "origin" branch))
                                 (has-upstream
                                  '("git" "push"))
                                 (t
                                  (list "git" "push" "--set-upstream" "origin" branch)))))
                (log-command view (format nil "~{~A~^ ~}" push-cmd))
                (setf (active-runner view) (make-process-runner))
                (setf (runner-title view) (if force-p "Force Pushing..." "Pushing..."))
                (runner-start (active-runner view) push-cmd)
                (setf (panel-title (main-panel view)) "[0] Push Output")
                (setf (panel-items (main-panel view))
                      (list (cond
                              (force-p "Starting git push --force-with-lease...")
                              ((not has-upstream)
                               (format nil "Setting upstream and pushing ~A..." branch))
                              (t "Starting git push..."))))))
             ;; Pull dialog - start async runner
             ((string= (dialog-title dlg) "Pull")
              (log-command view "git pull")
              (setf (active-runner view) (make-process-runner))
              (setf (runner-title view) "Pulling...")
              (runner-start (active-runner view) '("git" "pull"))
              (setf (panel-title (main-panel view)) "[0] Pull Output")
              (setf (panel-items (main-panel view)) (list "Starting git pull...")))
             ;; Squash dialog
             ((string= (dialog-title dlg) "Squash Commits")
              (let ((msg (dialog-get-text dlg)))
                (when (> (length msg) 0)
                  ;; Get count from stored data in message
                  (let* ((stored (getf (dialog-data dlg) :count))
                         (count (or stored 2)))
                    (log-command view (format nil "git reset --soft HEAD~~~D && git commit" count))
                    (git-squash-commits count msg)
                    (refresh-data view)))))
             ;; Cherry-pick dialog
             ((string= (dialog-title dlg) "Cherry Pick")
              (let ((hash (getf (dialog-data dlg) :hash)))
                (when hash
                  (log-command view (format nil "git cherry-pick ~A" hash))
                  (git-cherry-pick hash)
                  ;; Exit cherry-pick mode if active
                  (when (cherry-pick-mode view)
                    (setf (cherry-pick-mode view) nil)
                    (setf (cherry-pick-branch view) nil)
                    (setf (cherry-pick-commits view) nil))
                  (refresh-data view))))
             ;; Revert dialog
             ((string= (dialog-title dlg) "Revert Commit")
              (let ((hash (getf (dialog-data dlg) :hash)))
                (when hash
                  (log-command view (format nil "git revert ~A" hash))
                  (git-revert hash)
                  (refresh-data view))))
             ;; Amend Commit dialog
             ((string= (dialog-title dlg) "Amend Commit")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Amend")
                   (log-command view "git commit --amend --no-edit")
                   (git-amend)
                   (refresh-data view))
                  ((string= selected-button "Amend with new message")
                   ;; Show another dialog for the new message
                   (let ((current-msg (git-commit-message "HEAD")))
                     (setf (active-dialog view)
                           (make-dialog :title "Amend Message"
                                        :message "Enter new commit message:"
                                        :input-mode t
                                        :multiline t
                                        :buttons '("Amend" "Cancel")))
                     ;; Pre-fill with current message
                     (setf (dialog-input-lines (active-dialog view))
                           (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Space) current-msg))))))))
             ;; Amend Message dialog (second step)
             ((string= (dialog-title dlg) "Amend Message")
              (let* ((lines (dialog-input-lines dlg))
                     (message (format nil "~{~A~^~%~}" lines)))
                (when (> (length message) 0)
                  (log-command view "git commit --amend -m ...")
                  (git-amend-message message)
                  (refresh-data view))))
             ;; Reset to Commit dialog
             ((string= (dialog-title dlg) "Reset to Commit")
              (let* ((hash (getf (dialog-data dlg) :hash))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when hash
                  (cond
                    ((string= selected-button "Soft")
                     (log-command view (format nil "git reset --soft ~A" hash))
                     (git-reset-soft hash)
                     (refresh-data view))
                    ((string= selected-button "Mixed")
                     (log-command view (format nil "git reset --mixed ~A" hash))
                     (git-reset-mixed hash)
                     (refresh-data view))
                    ((string= selected-button "Hard")
                     (log-command view (format nil "git reset --hard ~A" hash))
                     (git-reset-hard hash)
                     (refresh-data view))))))
             ;; Fixup Commit dialog
             ((string= (dialog-title dlg) "Fixup Commit")
              (let* ((hash (getf (dialog-data dlg) :hash))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when (and hash (string= selected-button "Fixup"))
                  (log-command view (format nil "git commit --fixup ~A" hash))
                  (git-fixup-commit hash)
                  (refresh-data view))))
             ;; Search Commits dialog
             ((string= (dialog-title dlg) "Search Commits")
              (let ((query (first (dialog-input-lines dlg))))
                (when (and query (> (length query) 0))
                  (let ((results (git-log-search query)))
                    (log-command view (format nil "git log --grep=~A" query))
                    (if results
                          (progn
                            (setf (search-mode view) t)
                            (setf (search-query view) query)
                            (setf (search-results view) results)
                            (setf (panel-selected (commits-panel view)) 0)
                            ;; Update commits panel with search results
                            (setf (panel-items (commits-panel view))
                                  (loop for c in results
                                        collect (format-search-result c query)))
                            ;; Update panel title to show search
                            (setf (panel-title (commits-panel view))
                                  (format nil "[4] Search: ~A (~D results)"
                                          query (length results))))
                          ;; No results
                          (setf (active-dialog view)
                                (make-dialog :title "Search"
                                             :message (format nil "No commits found matching '~A'" query)
                                             :buttons '("OK"))))))))
             ;; Abort Merge dialog
             ((string= (dialog-title dlg) "Abort Merge")
              (log-command view "git merge --abort")
              (git-merge-abort)
              (refresh-data view))
             ;; Fetch dialog - handle remote selection
             ((string= (dialog-title dlg) "Fetch")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (unless (string= selected-button "Cancel")
                  (if (string= selected-button "All")
                      (progn
                        (log-command view "git fetch --all")
                        (show-status-message "Fetching all..." (screen-width view) (screen-height view))
                        (git-fetch)
                        (log-command view "git fetch completed"))
                      (progn
                        (log-command view (format nil "git fetch ~A" selected-button))
                        (show-status-message (format nil "Fetching ~A..." selected-button) 
                                             (screen-width view) (screen-height view))
                        (git-fetch selected-button)
                        (log-command view (format nil "git fetch ~A completed" selected-button))))
                  (refresh-data view))))
             ;; Delete Remote Branch dialog
             ((string= (dialog-title dlg) "Delete Remote Branch")
              (let ((remote-branch (getf (dialog-data dlg) :remote-branch)))
                (when remote-branch
                  (log-command view (format nil "git push --delete ~A" remote-branch))
                  (git-delete-remote-branch remote-branch)
                  (refresh-data view))))
             ;; Rename Remote dialog
             ((string= (dialog-title dlg) "Rename Remote")
              (let ((old-name (getf (dialog-data dlg) :old-name))
                    (new-name (first (dialog-input-lines dlg))))
                (when (and old-name new-name (> (length new-name) 0))
                  (log-command view (format nil "git remote rename ~A ~A" old-name new-name))
                  (git-remote-rename old-name new-name)
                  (refresh-data view))))
             ;; Add Remote dialog - two-step process
             ((string= (dialog-title dlg) "Add Remote")
              (let ((step (getf (dialog-data dlg) :step))
                    (input (first (dialog-input-lines dlg))))
                (cond
                  ;; Step 1: Got the name, now ask for URL
                  ((and (eq step :name) input (> (length input) 0))
                   (setf (active-dialog view)
                         (make-dialog :title "Add Remote"
                                      :message (format nil "URL for '~A':" input)
                                      :input-mode t
                                      :data (list :step :url :name input)
                                      :buttons '("Add" "Cancel"))))
                  ;; Step 2: Got the URL, create the remote
                  ((eq step :url)
                   (let ((name (getf (dialog-data dlg) :name)))
                     (when (and name input (> (length input) 0))
                       (log-command view (format nil "git remote add ~A ~A" name input))
                       (git-remote-add name input)
                       (refresh-data view)))))))
             ;; Update Submodule dialog
             ((or (string= (dialog-title dlg) "Update Submodule")
                  (string= (dialog-title dlg) "Update Submodules"))
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons))
                     (path (getf (dialog-data dlg) :submodule-path)))
                (cond
                  ((string= selected-button "Update")
                   (when path
                     (log-command view (format nil "git submodule update --init ~A" path))
                     (show-status-message (format nil "Updating ~A..." path) 
                                          (screen-width view) (screen-height view))
                     (git-submodule-update path)
                     (refresh-data view)))
                  ((string= selected-button "Update All")
                   (log-command view "git submodule update --init --recursive")
                   (show-status-message "Updating all submodules..." 
                                        (screen-width view) (screen-height view))
                   (git-submodule-update)
                   (refresh-data view)))))
             ;; Add Worktree dialog
             ((string= (dialog-title dlg) "Add Worktree")
              (let* ((path (first (dialog-input-lines dlg)))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when (and path (> (length path) 0))
                  (cond
                    ((string= selected-button "Add")
                     ;; Add worktree for current branch (detached)
                     (log-command view (format nil "git worktree add --detach ~A" path))
                     (git-worktree-add path)
                     (refresh-data view))
                    ((string= selected-button "New Branch")
                     ;; Ask for branch name
                     (setf (active-dialog view)
                           (make-dialog :title "Add Worktree"
                                        :message (format nil "Branch name for ~A:" path)
                                        :input-mode t
                                        :data (list :path path :step :branch)
                                        :buttons '("Create" "Cancel"))))))))
             ;; Add Worktree - step 2 (branch name)
             ((and (string= (dialog-title dlg) "Add Worktree")
                   (eq (getf (dialog-data dlg) :step) :branch))
              (let ((path (getf (dialog-data dlg) :path))
                    (branch (first (dialog-input-lines dlg))))
                (when (and path branch (> (length branch) 0))
                  (log-command view (format nil "git worktree add -b ~A ~A" branch path))
                  (git-worktree-add-new-branch path branch)
                  (refresh-data view))))
             ;; Remove Worktree dialog
             ((string= (dialog-title dlg) "Remove Worktree")
              (let* ((path (getf (dialog-data dlg) :worktree-path))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Remove")
                   (log-command view (format nil "git worktree remove ~A" path))
                   (git-worktree-remove path)
                   (refresh-data view))
                  ((string= selected-button "Force Remove")
                   (log-command view (format nil "git worktree remove --force ~A" path))
                   (git-worktree-remove path t)
                   (refresh-data view)))))
             ;; Drop Stash dialog
             ((string= (dialog-title dlg) "Drop Stash")
              (let* ((idx (getf (dialog-data dlg) :stash-index))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when (string= selected-button "Drop")
                  (log-command view (format nil "git stash drop stash@{~D}" idx))
                  (git-stash-drop idx)
                  (refresh-data view))))
             ;; Pop Stash dialog
             ((string= (dialog-title dlg) "Pop Stash")
              (let* ((idx (getf (dialog-data dlg) :stash-index))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when (string= selected-button "Pop")
                  (log-command view (format nil "git stash pop stash@{~D}" idx))
                  (git-stash-pop idx)
                  (refresh-data view))))
             ;; Git Flow dialog
             ((string= (dialog-title dlg) "Git Flow")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Init")
                   (log-command view "git flow init -d")
                   (log-command view (git-flow-init))
                   (refresh-data view))
                  ((or (string= selected-button "Feature Start")
                       (string= selected-button "Feature Finish")
                       (string= selected-button "Release Start")
                       (string= selected-button "Release Finish")
                       (string= selected-button "Hotfix Start")
                       (string= selected-button "Hotfix Finish"))
                   ;; Open a name input dialog
                   (setf (active-dialog view)
                         (make-dialog :title (format nil "Git Flow: ~A" selected-button)
                                      :input-mode t
                                      :data (list :flow-action selected-button)
                                      :buttons '("OK" "Cancel")))))))
             ;; Git Flow name input dialog
             ((and (>= (length (dialog-title dlg)) 9)
                   (string= (subseq (dialog-title dlg) 0 9) "Git Flow:"))
              (let ((name (first (dialog-input-lines dlg)))
                    (action (getf (dialog-data dlg) :flow-action)))
                (when (and name (> (length name) 0) action)
                  (cond
                    ((string= action "Feature Start")
                     (log-command view (format nil "git flow feature start ~A" name))
                     (log-command view (git-flow-feature-start name)))
                    ((string= action "Feature Finish")
                     (log-command view (format nil "git flow feature finish ~A" name))
                     (log-command view (git-flow-feature-finish name)))
                    ((string= action "Release Start")
                     (log-command view (format nil "git flow release start ~A" name))
                     (log-command view (git-flow-release-start name)))
                    ((string= action "Release Finish")
                     (log-command view (format nil "git flow release finish ~A" name))
                     (log-command view (git-flow-release-finish name)))
                    ((string= action "Hotfix Start")
                     (log-command view (format nil "git flow hotfix start ~A" name))
                     (log-command view (git-flow-hotfix-start name)))
                    ((string= action "Hotfix Finish")
                     (log-command view (format nil "git flow hotfix finish ~A" name))
                     (log-command view (git-flow-hotfix-finish name))))
                  (refresh-data view))))
             ;; Ignore File dialog
             ((string= (dialog-title dlg) "Ignore File")
              (let ((file (getf (dialog-data dlg) :file)))
                (when file
                  (let ((msg (git-ignore-file file)))
                    (log-command view msg)
                    (refresh-data view)))))
             ;; Shell Command dialog
             ((string= (dialog-title dlg) "Shell Command")
              (let ((cmd (first (dialog-input-lines dlg))))
                (when (and cmd (> (length cmd) 0))
                  (log-command view (format nil "$ ~A" cmd))
                  (let ((output (git-shell-command cmd)))
                    (when (and output (> (length output) 0))
                      (log-command view (string-trim '(#\Newline) output)))
                    (refresh-data view)))))
             ;; Filter Files dialog
             ((string= (dialog-title dlg) "Filter Files")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Filter")
                   (let ((query (first (dialog-input-lines dlg))))
                     (when (and query (> (length query) 0))
                       (setf (filter-query view) (string-downcase query))
                       (setf (filter-panel view) 1)
                       (refresh-data view))))
                  ((string= selected-button "Clear")
                   (setf (filter-query view) nil)
                   (setf (filter-panel view) nil)
                   (refresh-data view)))))
             ;; Filter Branches dialog
             ((string= (dialog-title dlg) "Filter Branches")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Filter")
                   (let ((query (first (dialog-input-lines dlg))))
                     (when (and query (> (length query) 0))
                       (setf (filter-query view) (string-downcase query))
                       (setf (filter-panel view) 2)
                       (refresh-data view))))
                  ((string= selected-button "Clear")
                   (setf (filter-query view) nil)
                   (setf (filter-panel view) nil)
                   (refresh-data view)))))
             ;; Filter Stashes dialog
             ((string= (dialog-title dlg) "Filter Stashes")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Filter")
                   (let ((query (first (dialog-input-lines dlg))))
                     (when (and query (> (length query) 0))
                       (setf (filter-query view) (string-downcase query))
                       (setf (filter-panel view) 4)
                       (refresh-data view))))
                  ((string= selected-button "Clear")
                   (setf (filter-query view) nil)
                   (setf (filter-panel view) nil)
                   (refresh-data view)))))
             ;; Bisect dialog
             ((string= (dialog-title dlg) "Bisect")
              (let ((commit-hash (getf (dialog-data dlg) :commit-hash)))
                (when commit-hash
                  (log-command view "git bisect start")
                  (git-bisect-start)
                  (log-command view (format nil "git bisect bad ~A" (subseq commit-hash 0 (min 7 (length commit-hash)))))
                  (git-bisect-bad commit-hash)
                  (setf (bisect-mode view) t)
                  (setf (panel-title (commits-panel view))
                        "[4] BISECT  b:bad g:good Q:reset")
                  (refresh-data view))))
             ;; Checkout Tag dialog
             ((string= (dialog-title dlg) "Checkout Tag")
              (let ((tag-name (getf (dialog-data dlg) :tag-name)))
                (when tag-name
                  (log-command view (format nil "git checkout ~A" tag-name))
                  (git-checkout-tag tag-name)
                  (refresh-data view))))
             ;; Set Upstream dialog
             ((string= (dialog-title dlg) "Set Upstream")
              (let* ((branch (getf (dialog-data dlg) :branch))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Set")
                   (let ((remote-branch (first (dialog-input-lines dlg))))
                     (when (and remote-branch (> (length remote-branch) 0))
                       (log-command view (format nil "git branch --set-upstream-to=~A ~A" remote-branch branch))
                       (git-set-upstream branch remote-branch)
                       (refresh-data view))))
                  ((string= selected-button "Unset")
                   (log-command view (format nil "git branch --unset-upstream ~A" branch))
                   (git-unset-upstream branch)
                   (refresh-data view)))))
             ;; Rename Stash dialog
             ((string= (dialog-title dlg) "Rename Stash")
              (let ((new-msg (first (dialog-input-lines dlg)))
                    (idx (getf (dialog-data dlg) :stash-index)))
                (when (and new-msg (> (length new-msg) 0))
                  (log-command view (format nil "git stash rename stash@{~D} \"~A\"" idx new-msg))
                  (git-rename-stash idx new-msg)
                  (refresh-data view))))
             ;; Rename Branch dialog
             ((string= (dialog-title dlg) "Rename Branch")
              (let ((old-name (getf (dialog-data dlg) :old-name))
                    (new-name (first (dialog-input-lines dlg))))
                (when (and old-name new-name (> (length new-name) 0))
                  (log-command view (format nil "git branch -m ~A ~A" old-name new-name))
                  (git-rename-branch old-name new-name)
                  (refresh-data view))))
             ;; Fast-Forward Branch dialog
             ((string= (dialog-title dlg) "Fast-Forward Branch")
              (let ((branch (getf (dialog-data dlg) :branch)))
                (when branch
                  (log-command view (format nil "git fetch origin ~A:~A" branch branch))
                  (git-fast-forward branch)
                  (refresh-data view))))
             ;; Push Tag dialog
             ((string= (dialog-title dlg) "Push Tag")
              (let* ((tag-name (getf (dialog-data dlg) :tag-name))
                     (buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Push")
                   (when tag-name
                     (log-command view (format nil "git push origin ~A" tag-name))
                     (git-push-tag tag-name)
                     (refresh-data view)))
                  ((string= selected-button "Push All")
                   (log-command view "git push origin --tags")
                   (git-push-all-tags)
                   (refresh-data view)))))
             ;; Branch from Stash dialog
             ((string= (dialog-title dlg) "Branch from Stash")
              (let ((branch-name (first (dialog-input-lines dlg)))
                    (idx (getf (dialog-data dlg) :stash-index)))
                (when (and branch-name (> (length branch-name) 0))
                  (log-command view (format nil "git stash branch ~A stash@{~D}" branch-name idx))
                  (git-stash-branch branch-name idx)
                  (refresh-data view))))
             ;; Reword Commit dialog (in rebase mode)
             ((string= (dialog-title dlg) "Reword Commit")
              (let ((new-msg (first (dialog-input-lines dlg)))
                    (idx (getf (dialog-data dlg) :rebase-index)))
                (when (and new-msg (> (length new-msg) 0) idx)
                  (let ((entry (nth idx (rebase-entries view))))
                    (when entry
                      (setf (rebase-action entry) :reword)
                      (setf (rebase-new-message entry) new-msg)
                      (update-rebase-display view))))))
             ;; Execute Rebase dialog
             ((string= (dialog-title dlg) "Execute Rebase")
              (when (getf (dialog-data dlg) :rebase-execute)
                (let ((entries (rebase-entries view))
                      (base (rebase-base-commit view)))
                  (when (and entries base)
                    (log-command view (format nil "git rebase -i ~A" base))
                    (let ((result (git-rebase-interactive entries base)))
                      (declare (ignore result))
                      ;; Exit rebase mode
                      (setf (rebase-mode view) nil)
                      (setf (rebase-entries view) nil)
                      (setf (rebase-base-commit view) nil)
                      (setf (panel-title (commits-panel view))
                            (numbered-title 4 "Commits" '("Reflog")))
                      (refresh-data view))))))
             ;; Rebase Branch dialog
             ((string= (dialog-title dlg) "Rebase Branch")
              (let ((branch (getf (dialog-data dlg) :branch)))
                (when branch
                  (log-command view (format nil "git rebase ~A" branch))
                  (git-rebase-onto branch)
                  (refresh-data view))))))
         (setf (active-dialog view) nil))
        ((eq result :cancel)
         (setf (active-dialog view) nil))
        (t
         ;; Dialog is still active, just redraw the dialog only
         (draw-dialog (active-dialog view) (screen-width view) (screen-height view))
         ;; Return :dialog to skip full re-render while dialog is active
         (return-from handle-key :dialog))))
    ;; Dialog was closed - return nil to trigger full re-render
    (return-from handle-key nil))
  
  ;; Handle config mode - consume keys in this mode
  (when (config-mode view)
    (cond
      ;; Escape or G exits config mode
      ((or (eq (key-event-code key) +key-escape+)
           (and (key-event-char key) (char= (key-event-char key) #\G)))
       (setf (config-mode view) nil)
       (setf (config-list view) nil)
       (setf (config-scope-filter view) nil)
       (setf (panel-title (main-panel view)) (numbered-title 0 "Main"))
       (setf (panel-items (main-panel view)) nil)
       (setf (panel-selected (main-panel view)) 0)
       (setf (panel-focused (main-panel view)) nil)
       (refresh-data view)
       (return-from handle-key nil))
      ;; w cycles scope filter: All -> Local -> Global -> System -> All
      ((and (key-event-char key) (char= (key-event-char key) #\w))
       (setf (config-scope-filter view)
             (case (config-scope-filter view)
               ((nil) :local)
               (:local :global)
               (:global :system)
               (:system nil)))
       (let* ((scope (config-scope-filter view))
              (configs (git-config-list scope)))
         (setf (config-list view) configs)
         (setf (panel-title (main-panel view))
               (numbered-title 0 "Config" 
                               (list (case scope
                                       ((nil) "All")
                                       (:local "Local")
                                       (:global "Global")
                                       (:system "System"))
                                     "w:cycle")))
         (setf (panel-items (main-panel view))
               (loop for cfg in configs
                     when cfg
                     collect (let ((cfg-scope (config-scope cfg))
                                   (key (config-key cfg))
                                   (val (config-value cfg)))
                               `(:multi-colored
                                 (,(case cfg-scope
                                     (:local :green)
                                     (:global :yellow)
                                     (:system :cyan)
                                     (t :white))
                                  ,(format nil "~A" key))
                                 (:bright-black " = ")
                                 (:white ,val)))))
         (setf (panel-selected (main-panel view)) 0))
       (return-from handle-key nil))
      ;; Navigation
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next (main-panel view))
       (return-from handle-key nil))
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev (main-panel view))
       (return-from handle-key nil))
      ;; q quits
      ((and (key-event-char key) (char= (key-event-char key) #\q))
       (return-from handle-key :quit))))

  ;; Handle blame mode - consume ALL keys in this mode
  (when (blame-mode view)
    (cond
      ;; Escape exits blame mode
      ((eq (key-event-code key) +key-escape+)
       (setf (blame-mode view) nil)
       (setf (blame-data view) nil)
       (setf (blame-file view) nil)
       (setf (panel-focused (main-panel view)) nil)
       (update-main-content view))
      ;; Enter shows commit details for selected line
      ((eq (key-event-code key) +key-enter+)
       (let* ((blame-lines (blame-data view))
              (selected (panel-selected (main-panel view))))
         (when (and blame-lines (< selected (length blame-lines)))
           (let* ((bl (nth selected blame-lines))
                  (hash (blame-line-hash bl)))
             (when (and hash (> (length hash) 0))
               ;; Show commit message in a dialog
               (let ((msg (git-commit-message hash)))
                 (setf (active-dialog view)
                       (make-dialog :title (format nil "Commit ~A" (blame-line-short-hash bl))
                                    :message (format nil "Author: ~A~%Date: ~A~%~%~A"
                                                     (blame-line-author bl)
                                                     (blame-line-date bl)
                                                     (string-trim '(#\Newline #\Space) msg))
                                    :buttons '("OK")))))))))
      ;; Navigation in blame view
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next (main-panel view)))
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev (main-panel view))))
    ;; Always return nil in blame mode to trigger re-render
    (return-from handle-key nil))
  
  ;; Handle cherry-pick mode - viewing commits from another branch
  (when (cherry-pick-mode view)
    (cond
      ;; Escape exits cherry-pick mode
      ((eq (key-event-code key) +key-escape+)
       (setf (cherry-pick-mode view) nil)
       (setf (cherry-pick-branch view) nil)
       (setf (cherry-pick-commits view) nil)
       (update-main-content view))
      ;; Enter or C cherry-picks the selected commit
      ((or (eq (key-event-code key) +key-enter+)
           (and (key-event-char key) (char= (key-event-char key) #\C)))
       (let* ((commits (cherry-pick-commits view))
              (selected (panel-selected (main-panel view))))
         (when (and commits (< selected (length commits)))
           (let* ((commit (nth selected commits))
                  (hash (log-entry-hash commit))
                  (short-hash (log-entry-short-hash commit))
                  (msg (log-entry-message commit)))
             (setf (active-dialog view)
                   (make-dialog :title "Cherry Pick"
                                :message (format nil "Cherry-pick ~A from ~A?~%~%~A"
                                                 short-hash 
                                                 (cherry-pick-branch view)
                                                 msg)
                                :data (list :hash hash :from-branch (cherry-pick-branch view))
                                :buttons '("Pick" "Cancel")))))))
      ;; Navigation
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next (main-panel view)))
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev (main-panel view))))
    ;; Always return nil in cherry-pick mode to trigger re-render
    (return-from handle-key nil))
  
  ;; Handle search mode - viewing filtered commits
  (when (search-mode view)
    (cond
      ;; Escape exits search mode
      ((eq (key-event-code key) +key-escape+)
       (setf (search-mode view) nil)
       (setf (search-query view) nil)
       (setf (search-results view) nil)
       ;; Reset panel title
       (setf (panel-title (commits-panel view))
             (numbered-title 4 "Commits" '("Reflog")))
       (refresh-data view))
      ;; Enter shows commit details
      ((eq (key-event-code key) +key-enter+)
       (let* ((commits (search-results view))
              (selected (panel-selected (commits-panel view))))
         (when (and commits (< selected (length commits)))
           (let* ((commit (nth selected commits))
                  (hash (log-entry-hash commit))
                  (short-hash (log-entry-short-hash commit)))
             (when hash
               (let ((msg (git-commit-message hash)))
                 (setf (active-dialog view)
                       (make-dialog :title (format nil "Commit ~A" short-hash)
                                    :message (format nil "Author: ~A~%Date: ~A~%~%~A"
                                                     (log-entry-author commit)
                                                     (log-entry-date commit)
                                                     (string-trim '(#\Newline #\Space) msg))
                                    :buttons '("OK")))))))))
      ;; / to search again
      ((and (key-event-char key) (char= (key-event-char key) #\/))
       (setf (active-dialog view)
             (make-dialog :title "Search Commits"
                          :message "Enter search term:"
                          :input-mode t
                          :buttons '("Search" "Cancel"))))
      ;; Navigation
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next (commits-panel view)))
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev (commits-panel view))))
    ;; Always return nil in search mode to trigger re-render
    (return-from handle-key nil))
  
  ;; Handle hunk mode
  (when (hunk-mode view)
    (cond
      ;; Escape exits hunk mode
      ((eq (key-event-code key) +key-escape+)
       (setf (hunk-mode view) nil)
       (setf (hunk-list view) nil)
       (update-main-content view)
       (return-from handle-key nil))
      ;; Space stages selected hunk
      ((and (key-event-char key) (char= (key-event-char key) #\Space))
       (let* ((hunks (hunk-list view))
              (selected (panel-selected (main-panel view))))
         (when (and hunks (< selected (length hunks)))
           (let ((hunk (nth selected hunks)))
             (log-command view (format nil "git apply --cached (hunk ~D)" (1+ selected)))
             (git-stage-hunk hunk)
             ;; Remove staged hunk from list
             (setf (hunk-list view) (remove hunk hunks))
             (if (hunk-list view)
                 (setf (panel-items (main-panel view))
                       (loop for h in (hunk-list view)
                             for i from 1
                             collect (format nil "Hunk ~D: ~A (+~D lines)"
                                             i (hunk-header h) (hunk-line-count h))))
                 (progn
                   (setf (hunk-mode view) nil)
                   (refresh-data view))))))
       (return-from handle-key nil))
      ;; Navigation in hunk list
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next (main-panel view))
       (return-from handle-key nil))
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev (main-panel view))
       (return-from handle-key nil))))

  ;; Interactive rebase mode key handling
  (when (rebase-mode view)
    (let ((entries (rebase-entries view))
          (selected (panel-selected (commits-panel view))))
      (cond
        ;; Quit rebase mode without executing
        ((or (and (key-event-char key) (char= (key-event-char key) #\q))
             (eq (key-event-code key) +key-escape+))
         (setf (rebase-mode view) nil)
         (setf (rebase-entries view) nil)
         (setf (rebase-base-commit view) nil)
         (setf (panel-title (commits-panel view))
               (numbered-title 4 "Commits" '("Reflog")))
         (refresh-data view))
        ;; Navigate
        ((or (eq (key-event-code key) +key-down+)
             (and (key-event-char key) (char= (key-event-char key) #\j)))
         (panel-select-next (commits-panel view)))
        ((or (eq (key-event-code key) +key-up+)
             (and (key-event-char key) (char= (key-event-char key) #\k)))
         (panel-select-prev (commits-panel view)))
        ;; Mark as pick
        ((and (key-event-char key) (char= (key-event-char key) #\p))
         (when (and entries (< selected (length entries)))
           (setf (rebase-action (nth selected entries)) :pick)
           (update-rebase-display view)))
        ;; Mark as reword
        ((and (key-event-char key) (char= (key-event-char key) #\r))
         (when (and entries (< selected (length entries)))
           (let ((entry (nth selected entries)))
             (setf (active-dialog view)
                   (make-dialog :title "Reword Commit"
                                :message (format nil "New message for ~A:"
                                                 (rebase-short-hash entry))
                                :input-mode t
                                :data (list :rebase-index selected
                                            :old-message (rebase-message entry))
                                :buttons '("Reword" "Cancel"))))))
        ;; Mark as squash
        ((and (key-event-char key) (char= (key-event-char key) #\s))
         (when (and entries (< selected (length entries)) (> selected 0))
           (setf (rebase-action (nth selected entries)) :squash)
           (update-rebase-display view)))
        ;; Mark as fixup
        ((and (key-event-char key) (char= (key-event-char key) #\f))
         (when (and entries (< selected (length entries)) (> selected 0))
           (setf (rebase-action (nth selected entries)) :fixup)
           (update-rebase-display view)))
        ;; Mark as drop
        ((and (key-event-char key) (char= (key-event-char key) #\d))
         (when (and entries (< selected (length entries)))
           (setf (rebase-action (nth selected entries)) :drop)
           (update-rebase-display view)))
        ;; Move commit down in order
        ((and (key-event-char key) (char= (key-event-char key) #\J))
         (when (and entries (< selected (1- (length entries))))
           (rotatef (nth selected entries) (nth (1+ selected) entries))
           (panel-select-next (commits-panel view))
           (update-rebase-display view)))
        ;; Move commit up in order
        ((and (key-event-char key) (char= (key-event-char key) #\K))
         (when (and entries (> selected 0))
           (rotatef (nth selected entries) (nth (1- selected) entries))
           (panel-select-prev (commits-panel view))
           (update-rebase-display view)))
        ;; Execute rebase
        ((eq (key-event-code key) +key-enter+)
         (setf (active-dialog view)
               (make-dialog :title "Execute Rebase"
                            :message (format nil "Execute interactive rebase on ~D commits?"
                                             (length entries))
                            :data (list :rebase-execute t)
                            :buttons '("Execute" "Cancel"))))))
    (return-from handle-key nil))
  
  (let* ((focused-idx (view-focused-panel view))
         (panel (nth focused-idx (view-panels view))))
    (cond
      ;; Quit
      ((and (key-event-char key) (char= (key-event-char key) #\q))
       :quit)
      ;; j/k navigation when main panel is focused (must be before regular j/k)
      ((and (panel-focused (main-panel view))
            (or (eq (key-event-code key) +key-down+)
                (and (key-event-char key) (char= (key-event-char key) #\j))))
       (panel-select-next (main-panel view))
       nil)
      ((and (panel-focused (main-panel view))
            (or (eq (key-event-code key) +key-up+)
                (and (key-event-char key) (char= (key-event-char key) #\k))))
       (panel-select-prev (main-panel view))
       nil)
      ;; Escape unfocuses main panel
      ((and (panel-focused (main-panel view))
            (eq (key-event-code key) +key-escape+))
       (setf (panel-focused (main-panel view)) nil)
       ;; Re-focus the previously focused left panel
       (let ((p (nth focused-idx (view-panels view))))
         (setf (panel-focused p) t))
       nil)
      ;; Navigation within panel - down
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (panel-select-next panel)
       (update-main-content view)
       nil)
      ;; Navigation within panel - up  
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (panel-select-prev panel)
       (update-main-content view)
       nil)
      ;; Switch to next panel - Tab or l
      ((or (eq (key-event-code key) +key-tab+)
           (and (key-event-char key) (char= (key-event-char key) #\l)))
       (setf (panel-focused (main-panel view)) nil)  ; Unfocus main panel
       (setf (view-focused-panel view)
             (mod (1+ focused-idx) 5))  ; Cycle through 5 left panels
       (update-main-content view)
       nil)
      ;; Switch to previous panel - h
      ((and (key-event-char key) (char= (key-event-char key) #\h))
       (setf (panel-focused (main-panel view)) nil)  ; Unfocus main panel
       (setf (view-focused-panel view)
             (mod (1- focused-idx) 5))
       (update-main-content view)
       nil)
      ;; Direct panel selection with number keys
      ((and (key-event-char key) (char= (key-event-char key) #\1))
       (setf (panel-focused (main-panel view)) nil)
       (setf (view-focused-panel view) 0)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\2))
       (setf (panel-focused (main-panel view)) nil)
       (setf (view-focused-panel view) 1)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\3))
       (setf (panel-focused (main-panel view)) nil)
       (setf (view-focused-panel view) 2)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\4))
       (setf (panel-focused (main-panel view)) nil)
       (setf (view-focused-panel view) 3)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\5))
       (setf (panel-focused (main-panel view)) nil)
       (setf (view-focused-panel view) 4)
       (update-main-content view)
       nil)
      ;; Focus main panel with 0 key for scrolling
      ((and (key-event-char key) (char= (key-event-char key) #\0))
       ;; Toggle main panel focus - if already focused, unfocus
       (if (panel-focused (main-panel view))
           (progn
             (setf (panel-focused (main-panel view)) nil)
             ;; Re-focus the left panel
             (let ((p (nth focused-idx (view-panels view))))
               (setf (panel-focused p) t)))
           (progn
             ;; Unfocus all left panels
             (dolist (p (view-panels view))
               (setf (panel-focused p) nil))
             ;; Focus main panel
             (setf (panel-focused (main-panel view)) t)))
       nil)
      ;; Page down
      ((eq (key-event-code key) +key-page-down+)
       (if (panel-focused (main-panel view))
           (panel-select-page-down (main-panel view))
           (progn (panel-select-page-down panel)
                  (update-main-content view)))
       nil)
      ;; Page up
      ((eq (key-event-code key) +key-page-up+)
       (if (panel-focused (main-panel view))
           (panel-select-page-up (main-panel view))
           (progn (panel-select-page-up panel)
                  (update-main-content view)))
       nil)
      ;; Stage/unstage with space (when on files panel) or checkout tag (tags view)
      ((and (key-event-char key) (char= (key-event-char key) #\Space))
       (cond
         ;; Files panel - stage/unstage
         ((= focused-idx 1)
          (let* ((entries (status-entries view))
                 (selected (panel-selected panel)))
            (when (and entries (< selected (length entries)))
              (let ((entry (nth selected entries)))
                (if (status-entry-staged-p entry)
                    (progn
                      (log-command view (format nil "git reset HEAD -- ~A" (status-entry-file entry)))
                      (git-unstage-file (status-entry-file entry)))
                    (progn
                      (log-command view (format nil "git add -- ~A" (status-entry-file entry)))
                      (git-stage-file (status-entry-file entry))))
                (refresh-data view)))))
         ;; Tags view - checkout tag as detached HEAD
         ((and (= focused-idx 2) (show-tags view))
          (let* ((tags (tag-list view))
                 (selected (panel-selected panel)))
            (when (and tags (< selected (length tags)))
              (let ((tag (nth selected tags)))
                (setf (active-dialog view)
                      (make-dialog :title "Checkout Tag"
                                   :message (format nil "Checkout tag '~A' as detached HEAD?" (tag-name tag))
                                   :data (list :tag-name (tag-name tag))
                                   :buttons '("Checkout" "Cancel"))))))))
       nil)
      ;; Enter on branches - checkout local or track remote
      ;; Enter on stashes view - apply stash
      ((eq (key-event-code key) +key-enter+)
       (cond
         ;; Stashes view - apply stash
         ((and (= focused-idx 1) (show-stashes view))
          (let* ((stashes (stash-list view))
                 (selected (panel-selected (files-panel view))))
            (when (and stashes (< selected (length stashes)))
              (let ((st (nth selected stashes)))
                (log-command view (format nil "git stash apply stash@{~D}" (stash-index st)))
                (git-stash-apply (stash-index st))
                (refresh-data view)))))
         ;; Submodules view - enter submodule
         ((and (= focused-idx 2) (show-submodules view))
          (let* ((submodules (submodule-list view))
                 (selected (panel-selected panel)))
            (when (and submodules (< selected (length submodules)))
              (let ((sm (nth selected submodules)))
                (log-command view (format nil "Entering submodule: ~A" (submodule-name sm)))
                (enter-submodule (submodule-path sm))
                (refresh-data view)))))
         ;; Branches panel
         ((= focused-idx 2)
          (if (show-remote-branches view)
              ;; Remote mode - track the remote branch
              (let* ((branches (remote-branch-list view))
                     (selected (panel-selected panel)))
                (when (and branches (< selected (length branches)))
                  (let ((branch (nth selected branches)))
                    (log-command view (format nil "git checkout -b ... --track ~A" branch))
                    (git-track-remote-branch branch)
                    (setf (show-remote-branches view) nil)  ; Switch back to local
                    (refresh-data view))))
              ;; Local mode - checkout
              (let* ((branches (branch-list view))
                     (selected (panel-selected panel)))
                (when (and branches (< selected (length branches)))
                  (let ((branch (nth selected branches)))
                    (log-command view (format nil "git checkout ~A" branch))
                    (git-checkout branch)
                    (refresh-data view)))))))
       nil)
      ;; Refresh
      ((and (key-event-char key) (char= (key-event-char key) #\r))
       (refresh-data view)
       nil)
      ;; Config viewer - 'G' (capital) toggles config mode
      ((and (key-event-char key) (char= (key-event-char key) #\G))
       (if (config-mode view)
           ;; Exit config mode
           (progn
             (setf (config-mode view) nil)
             (setf (config-list view) nil)
             (setf (config-scope-filter view) nil)
             (setf (panel-title (main-panel view)) (numbered-title 0 "Main"))
             (setf (panel-items (main-panel view)) nil)
             (setf (panel-selected (main-panel view)) 0)
             (refresh-data view))
           ;; Enter config mode
           (progn
             (setf (config-mode view) t)
             (setf (config-scope-filter view) nil)  ; Show all
             (let ((configs (git-config-list)))
               (setf (config-list view) configs)
               (setf (panel-title (main-panel view)) 
                     (numbered-title 0 "Config" '("All" "w:cycle scope")))
               (setf (panel-items (main-panel view))
                     (loop for cfg in configs
                           when cfg
                           collect (let ((scope (config-scope cfg))
                                         (key (config-key cfg))
                                         (val (config-value cfg)))
                                     `(:multi-colored
                                       (,(case scope
                                           (:local :green)
                                           (:global :yellow)
                                           (:system :cyan)
                                           (t :white))
                                        ,(format nil "~A" key))
                                       (:bright-black " = ")
                                       (:white ,val)))))
               (setf (panel-selected (main-panel view)) 0)
               (setf (panel-focused (main-panel view)) t))))
       nil)
      ;; Commit - 'c' opens commit dialog (multiline)
      ((and (key-event-char key) (char= (key-event-char key) #\c))
       (setf (active-dialog view)
             (make-dialog :title "Commit Message"
                          :input-mode t
                          :multiline t
                          :buttons '("Commit" "Cancel")))
       nil)
      ;; Commit with editor - 'C' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\C)
            (= focused-idx 1))
       (when (= focused-idx 1)  ; Files panel
         (log-command view (format nil "git commit  # $EDITOR=~A"
                                   (or (uiop:getenv "EDITOR") "vi")))
         ;; Suspend TUI, spawn editor, restore TUI
         (gilt.terminal:restore-terminal)
         (let ((success (git-commit-with-editor)))
           (gilt.terminal:setup-terminal)
           (clear-screen)
           (when success
             (refresh-data view))))
       nil)
      ;; Commit without hook - 'W' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\W)
            (= focused-idx 1))
       (setf (active-dialog view)
             (make-dialog :title "Commit (no hook)"
                          :message "Commit bypassing pre-commit hooks"
                          :input-mode t
                          :multiline t
                          :buttons '("Commit" "Cancel")))
       nil)
      ;; Push - 'P' (capital) opens push confirmation (not in stashes view)
      ((and (key-event-char key) (char= (key-event-char key) #\P)
            (not (and (= focused-idx 1) (show-stashes view))))
       (setf (active-dialog view)
             (make-dialog :title "Push"
                          :message "Push to origin?"
                          :buttons '("Push" "Force Push" "Cancel")))
       nil)
      ;; Pull - 'p' (lowercase) opens pull confirmation (not in stashes view)
      ((and (key-event-char key) (char= (key-event-char key) #\p)
            (not (and (= focused-idx 1) (show-stashes view))))
       (setf (active-dialog view)
             (make-dialog :title "Pull"
                          :message "Pull from origin?"
                          :buttons '("Pull" "Cancel")))
       nil)
      ;; Undo - 'z' (global)
      ((and (key-event-char key) (char= (key-event-char key) #\z))
       (let ((msg (git-undo)))
         (if msg
             (progn
               (log-command view (format nil "undo: ~A" msg))
               (refresh-data view))
             (log-command view "Nothing to undo")))
       nil)
      ;; Redo - 'Z' (capital, global)
      ((and (key-event-char key) (char= (key-event-char key) #\Z))
       (let ((msg (git-redo)))
         (if msg
             (progn
               (log-command view (format nil "redo: ~A" msg))
               (refresh-data view))
             (log-command view "Nothing to redo")))
       nil)
      ;; Stage all / unstage all toggle - 'a'
      ((and (key-event-char key) (char= (key-event-char key) #\a))
       (let* ((entries (status-entries view))
              (all-staged (and entries (every #'status-entry-staged-p entries))))
         (if all-staged
             (progn
               (log-command view "git reset HEAD")
               (git-unstage-all))
             (progn
               (log-command view "git add -A")
               (git-stage-all))))
       (refresh-data view)
       nil)
      ;; Screen mode cycling - '+' (global)
      ((and (key-event-char key) (char= (key-event-char key) #\+))
       (setf (screen-mode view)
             (case (screen-mode view)
               (:normal :half)
               (:half :full)
               (:full :normal)))
       (log-command view (format nil "Screen mode: ~A"
                                  (case (screen-mode view)
                                    (:normal "normal") (:half "half") (:full "full"))))
       nil)
      ;; Recent repos - 'L' (capital, global)
      ((and (key-event-char key) (char= (key-event-char key) #\L))
       (let ((repos (load-recent-repos)))
         (if repos
             (progn
               (setf (panel-title (main-panel view)) "[0] Recent Repos")
               (setf (panel-items (main-panel view))
                     (loop for r in repos
                           for i from 0
                           collect (format nil "  ~D. ~A" (1+ i) r)))
               (setf (panel-selected (main-panel view)) 0)
               (setf (panel-focused (main-panel view)) t)
               (log-command view "Recent repos (Enter to switch)"))
             (log-command view "No recent repos found")))
       nil)
      ;; Create pull request - 'O' (capital, when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\O))
       (when (= focused-idx 2)
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let* ((branch (nth selected branches))
                    (url (create-pull-request branch)))
               (if url
                   (log-command view (format nil "Opening PR: ~A" url))
                   (log-command view "No remote URL found"))))))
       nil)
      ;; Git-flow - 'E' (capital, global) opens git-flow dialog
      ((and (key-event-char key) (char= (key-event-char key) #\E))
       (setf (active-dialog view)
             (make-dialog :title "Git Flow"
                          :message "Select git-flow action:"
                          :buttons '("Feature Start" "Feature Finish" "Release Start" "Release Finish" "Hotfix Start" "Hotfix Finish" "Init" "Cancel")))
       nil)
      ;; Copy to clipboard - 'y' (context-sensitive yank)
      ((and (key-event-char key) (char= (key-event-char key) #\y))
       (let ((text nil)
             (what nil))
         (cond
           ;; Files panel - copy file path
           ((= focused-idx 1)
            (let* ((entries (status-entries view))
                   (selected (panel-selected panel)))
              (when (and entries (< selected (length entries)))
                (setf text (status-entry-file (nth selected entries)))
                (setf what "file path"))))
           ;; Branches panel - copy branch name
           ((= focused-idx 2)
            (let* ((branches (branch-list view))
                   (selected (panel-selected panel)))
              (when (and branches (< selected (length branches)))
                (setf text (nth selected branches))
                (setf what "branch name"))))
           ;; Commits panel - copy commit hash
           ((= focused-idx 3)
            (let* ((commits (commit-list view))
                   (selected (panel-selected panel)))
              (when (and commits (< selected (length commits)))
                (setf text (log-entry-short-hash (nth selected commits)))
                (setf what "commit hash")))))
         (when text
           (if (copy-to-clipboard text)
               (log-command view (format nil "Copied ~A: ~A" what text))
               (log-command view (format nil "Copy failed (no clipboard tool)")))))
       nil)
      ;; Ignore file - 'I' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\I))
       (when (= focused-idx 1)
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let ((file (status-entry-file (nth selected entries))))
               (setf (active-dialog view)
                     (make-dialog :title "Ignore File"
                                  :message (format nil "Add '~A' to .gitignore?" file)
                                  :data (list :file file)
                                  :buttons '("Ignore" "Cancel")))))))
       nil)
      ;; External diff tool - 'x' (when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\x)
            (= focused-idx 1))
       (when (= focused-idx 1)
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let ((file (status-entry-file (nth selected entries))))
               (log-command view (format nil "git difftool -- ~A" file))
               (gilt.terminal:restore-terminal)
               (gilt.terminal:leave-alternate-screen)
               (git-difftool file)
               (gilt.terminal:enter-alternate-screen)
               (gilt.terminal:setup-terminal)
               (clear-screen)
               (refresh-data view)))))
       nil)
      ;; Shell command - ':' (global, vim-style)
      ((and (key-event-char key) (char= (key-event-char key) #\:))
       (setf (active-dialog view)
             (make-dialog :title "Shell Command"
                          :input-mode t
                          :buttons '("Run" "Cancel")))
       nil)
      ;; File tree toggle - 'T' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\T)
            (= focused-idx 1))
       (when (and (= focused-idx 1) (not (show-worktrees view)) (not (show-stashes view)))
         (setf (file-tree-mode view) (not (file-tree-mode view)))
         (log-command view (format nil "File view: ~A" (if (file-tree-mode view) "tree" "flat")))
         (refresh-data view))
       nil)
      ;; Range select - 'v' (when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\v))
       (when (= focused-idx 1)
         (if (range-select-start view)
             ;; End range select - stage/unstage the range
             (let* ((start (range-select-start view))
                    (end (panel-selected panel))
                    (lo (min start end))
                    (hi (max start end))
                    (entries (status-entries view)))
               (when entries
                 (loop for i from lo to (min hi (1- (length entries)))
                       for entry = (nth i entries)
                       do (if (status-entry-staged-p entry)
                              (git-unstage-file (status-entry-file entry))
                              (git-stage-file (status-entry-file entry))))
                 (log-command view (format nil "Toggled ~D files (range ~D-~D)" (1+ (- hi lo)) lo hi))
                 (setf (range-select-start view) nil)
                 (refresh-data view)))
             ;; Start range select
             (progn
               (setf (range-select-start view) (panel-selected panel))
               (log-command view (format nil "Range select started at ~D" (panel-selected panel))))))
       nil)
      ;; 'o' key - context dependent (resolve conflict ours, open in browser)
      ((and (key-event-char key) (char= (key-event-char key) #\o))
       (let ((base-url (git-remote-url-for-browser)))
         (cond
           ;; Files panel - resolve conflict with ours
           ((= focused-idx 1)
            (let* ((entries (status-entries view))
                   (selected (panel-selected panel)))
              (when (and entries (< selected (length entries)))
                (let ((entry (nth selected entries)))
                  (when (eq (status-entry-status entry) :conflict)
                    (log-command view (format nil "git checkout --ours ~A && git add ~A"
                                               (status-entry-file entry) (status-entry-file entry)))
                    (git-resolve-with-ours (status-entry-file entry))
                    (refresh-data view))))))
           ;; Commits panel - open commit URL
           ((and (= focused-idx 3) base-url)
            (let* ((commits (commit-list view))
                   (selected (panel-selected panel)))
              (when (and commits (< selected (length commits)))
                (let* ((commit (nth selected commits))
                       (url (format nil "~A/commit/~A" base-url (log-entry-hash commit))))
                  (if (open-in-browser url)
                      (log-command view (format nil "Opened ~A" url))
                      (log-command view "Failed to open browser"))))))
           ;; Branches panel - open branch URL
           ((and (= focused-idx 2) base-url)
            (let* ((branches (branch-list view))
                   (selected (panel-selected panel)))
              (when (and branches (< selected (length branches)))
                (let* ((branch (nth selected branches))
                       (url (format nil "~A/tree/~A" base-url branch)))
                  (if (open-in-browser url)
                      (log-command view (format nil "Opened ~A" url))
                      (log-command view "Failed to open browser"))))))
           ;; No remote URL
           ((not base-url)
            (log-command view "No remote URL found"))))
       nil)
      ;; Diff context size - '{' decrease, '}' increase (global)
      ((and (key-event-char key) (char= (key-event-char key) #\}))
       (incf (diff-context-size view))
       (log-command view (format nil "Diff context: ~D lines" (diff-context-size view)))
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\{))
       (when (> (diff-context-size view) 0)
         (decf (diff-context-size view)))
       (log-command view (format nil "Diff context: ~D lines" (diff-context-size view)))
       (update-main-content view)
       nil)
      ;; Whitespace toggle - 'W' (capital, global)
      ((and (key-event-char key) (char= (key-event-char key) #\W))
       (setf (diff-ignore-whitespace view) (not (diff-ignore-whitespace view)))
       (log-command view (format nil "Whitespace: ~A"
                                  (if (diff-ignore-whitespace view) "ignored" "shown")))
       (update-main-content view)
       nil)
      ;; Rename stash - 'R' (capital, when on files panel in stashes view)
      ((and (key-event-char key) (char= (key-event-char key) #\R)
            (= focused-idx 1) (show-stashes view))
       (when (and (= focused-idx 1) (show-stashes view))
         (let* ((stashes (stash-list view))
                (selected (panel-selected panel)))
           (when (and stashes (< selected (length stashes)))
             (let ((st (nth selected stashes)))
               (setf (active-dialog view)
                     (make-dialog :title "Rename Stash"
                                  :message (format nil "Rename stash@{~D}:" (stash-index st))
                                  :input-mode t
                                  :data (list :stash-index (stash-index st))
                                  :buttons '("Rename" "Cancel")))))))
       nil)
      ;; Sort branches - 's' (when on branches panel, local view)
      ((and (key-event-char key) (char= (key-event-char key) #\s)
            (= focused-idx 2))
       (when (and (= focused-idx 2) (not (show-remote-branches view))
                  (not (show-tags view)) (not (show-submodules view)))
         (setf (branch-sort-mode view)
               (case (branch-sort-mode view)
                 (:name :date)
                 (:date :recent)
                 (:recent :name)))
         (log-command view (format nil "Sort: ~A"
                                    (case (branch-sort-mode view)
                                      (:name "alphabetical")
                                      (:date "creation date")
                                      (:recent "most recent"))))
         (refresh-data view))
       nil)
      ;; Set upstream - 'u' (when on branches panel, local view)
      ((and (key-event-char key) (char= (key-event-char key) #\u))
       (when (and (= focused-idx 2) (not (show-remote-branches view))
                  (not (show-tags view)) (not (show-submodules view)))
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (setf (active-dialog view)
                     (make-dialog :title "Set Upstream"
                                  :message (format nil "Set upstream for '~A' to (e.g. origin/~A):" branch branch)
                                  :input-mode t
                                  :data (list :branch branch)
                                  :buttons '("Set" "Unset" "Cancel")))))))
       nil)
      ;; Rename stash - 'r' (when on files panel in stashes view)
      ;; Note: 'r' is refresh globally, but in stashes view it becomes rename
      ;; Stash - 's' (when on stash panel or files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\s))
       (when (or (= focused-idx 4) (= focused-idx 1))  ; Stash or Files panel
         (log-command view "git stash")
         (git-stash)
         (refresh-data view))
       nil)
      ;; Stash pop - 'g' (when on stash panel)
      ((and (key-event-char key) (char= (key-event-char key) #\g)
            (= focused-idx 4))
       (when (= focused-idx 4)  ; Stash panel
         (log-command view "git stash pop")
         (git-stash-pop)
         (refresh-data view))
       nil)
      ;; 't' key - context dependent (conflict resolution, create tag)
      ((and (key-event-char key) (char= (key-event-char key) #\t))
       (cond
         ;; On files panel - resolve conflict with theirs
         ((= focused-idx 1)
          (let* ((entries (status-entries view))
                 (selected (panel-selected panel)))
            (when (and entries (< selected (length entries)))
              (let ((entry (nth selected entries)))
                (when (eq (status-entry-status entry) :conflict)
                  (log-command view (format nil "git checkout --theirs ~A && git add ~A"
                                            (status-entry-file entry) (status-entry-file entry)))
                  (git-resolve-with-theirs (status-entry-file entry))
                  (refresh-data view))))))
         ;; On commits panel - create tag on selected commit
         ((= focused-idx 3)
          (let* ((commits (commit-list view))
                 (selected (panel-selected panel)))
            (when (and commits (< selected (length commits)))
              (let ((commit (nth selected commits)))
                (setf (active-dialog view)
                      (make-dialog :title "Create Tag"
                                   :input-mode t
                                   :data (list :commit-hash (log-entry-hash commit))
                                   :buttons '("Create" "Cancel")))))))
         ;; On branches panel in tags view - create tag on HEAD
         ((and (= focused-idx 2) (show-tags view))
          (setf (active-dialog view)
                (make-dialog :title "Create Tag"
                             :input-mode t
                             :buttons '("Create" "Cancel")))))
       nil)
      ;; Push tag - 'T' (capital, when on branches panel in tags view)
      ((and (key-event-char key) (char= (key-event-char key) #\T))
       (when (and (= focused-idx 2) (show-tags view))
         (let* ((tags (tag-list view))
                (selected (panel-selected panel)))
           (when (and tags (< selected (length tags)))
             (let ((tag (nth selected tags)))
               (setf (active-dialog view)
                     (make-dialog :title "Push Tag"
                                  :message (format nil "Push tag '~A' to origin?" (tag-name tag))
                                  :data (list :tag-name (tag-name tag))
                                  :buttons '("Push" "Push All" "Cancel")))))))
       nil)
      ;; New branch from stash - 'B' (capital, when on files panel in stashes view)
      ((and (key-event-char key) (char= (key-event-char key) #\B))
       (when (and (= focused-idx 1) (show-stashes view))
         (let* ((stashes (stash-list view))
                (selected (panel-selected panel)))
           (when (and stashes (< selected (length stashes)))
             (let ((st (nth selected stashes)))
               (setf (active-dialog view)
                     (make-dialog :title "Branch from Stash"
                                  :message (format nil "Create branch from stash@{~D}:" (stash-index st))
                                  :input-mode t
                                  :data (list :stash-index (stash-index st))
                                  :buttons '("Create" "Cancel")))))))
       nil)
      ;; Abort merge - 'X' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\X)
            (= focused-idx 1))
       (when (= focused-idx 1)  ; Files panel
         (setf (active-dialog view)
               (make-dialog :title "Abort Merge"
                            :message "Abort the current merge? All merge progress will be lost."
                            :buttons '("Abort" "Cancel"))))
       nil)
      ;; Discard changes - 'd' (when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\d))
       (when (= focused-idx 1)  ; Files panel
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let ((entry (nth selected entries)))
               (unless (eq (status-entry-status entry) :untracked)
                 (log-command view (format nil "git checkout -- ~A" (status-entry-file entry)))
                 (git-discard-file (status-entry-file entry))
                 (refresh-data view))))))
       nil)
      ;; New branch - 'n' (when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\n))
       (when (= focused-idx 2)  ; Branches panel
         (setf (active-dialog view)
               (make-dialog :title "New Branch"
                            :input-mode t
                            :buttons '("Create" "Cancel"))))
       nil)
      ;; Fetch - 'f' (when on branches panel) - show remote selection
      ((and (key-event-char key) (char= (key-event-char key) #\f))
       (when (= focused-idx 2)  ; Branches panel
         (let ((remotes (git-remotes)))
           (if remotes
               ;; Show selection dialog with remotes
               (setf (active-dialog view)
                     (make-dialog :title "Fetch"
                                  :message "Select remote to fetch from:"
                                  :buttons (if (> (length remotes) 1)
                                               (append remotes '("All" "Cancel"))
                                               (append remotes '("Cancel")))
                                  :data (list :remotes remotes)))
               ;; No remotes configured
               (setf (active-dialog view)
                     (make-dialog :title "Fetch"
                                  :message "No remotes configured"
                                  :buttons '("OK"))))))
       nil)
      ;; Add worktree - 'A' (capital, when on files panel in worktrees view)
      ((and (key-event-char key) (char= (key-event-char key) #\A))
       (when (and (= focused-idx 1) (show-worktrees view))
         (setf (active-dialog view)
               (make-dialog :title "Add Worktree"
                            :message "Enter path for new worktree:"
                            :input-mode t
                            :buttons '("Add" "New Branch" "Cancel"))))
       nil)
      ;; Delete / Remove / Drop - 'D' (capital)
      ((and (key-event-char key) (char= (key-event-char key) #\D))
       (cond
         ;; Files panel: worktrees view - remove worktree
         ((and (= focused-idx 1) (show-worktrees view))
          (let* ((worktrees (worktree-list view))
                 (selected (panel-selected (files-panel view))))
            (when (and worktrees (< selected (length worktrees)))
              (let ((wt (nth selected worktrees)))
                ;; Don't allow removing the main worktree (first one, usually bare or main)
                (when (> selected 0)
                  (setf (active-dialog view)
                        (make-dialog :title "Remove Worktree"
                                     :message (format nil "Remove worktree at ~A?" (worktree-path wt))
                                     :data (list :worktree-path (worktree-path wt))
                                     :buttons '("Remove" "Force Remove" "Cancel"))))))))
         ;; Files panel: stashes view - drop stash
         ((and (= focused-idx 1) (show-stashes view))
          (let* ((stashes (stash-list view))
                 (selected (panel-selected (files-panel view))))
            (when (and stashes (< selected (length stashes)))
              (let ((st (nth selected stashes)))
                (setf (active-dialog view)
                      (make-dialog :title "Drop Stash"
                                   :message (format nil "Drop stash ~D: ~A?"
                                                    (stash-index st)
                                                    (or (stash-message st) "WIP"))
                                   :data (list :stash-index (stash-index st))
                                   :buttons '("Drop" "Cancel")))))))
         ;; Branches panel: delete tag
         ((and (= focused-idx 2) (show-tags view))
          (let* ((tags (tag-list view))
                 (selected (panel-selected panel)))
            (when (and tags (< selected (length tags)))
              (let ((tag (nth selected tags)))
                (setf (active-dialog view)
                      (make-dialog :title "Delete Tag"
                                   :message (format nil "Delete tag ~A?" (tag-name tag))
                                   :data (list :tag-name (tag-name tag))
                                   :buttons '("Delete" "Cancel")))))))
         ;; Branches panel: delete remote branch
         ((and (= focused-idx 2) (show-remote-branches view))
          (let* ((remote-branches (remote-branch-list view))
                 (selected (panel-selected panel)))
            (when (and remote-branches (< selected (length remote-branches)))
              (let ((remote-branch (nth selected remote-branches)))
                (setf (active-dialog view)
                      (make-dialog :title "Delete Remote Branch"
                                   :message (format nil "Delete remote branch ~A?" remote-branch)
                                   :data (list :remote-branch remote-branch)
                                   :buttons '("Delete" "Cancel")))))))
         ;; Branches panel: delete local branch
         ((= focused-idx 2)
          (let* ((branches (branch-list view))
                 (selected (panel-selected panel)))
            (when (and branches (< selected (length branches)))
              (let ((branch (nth selected branches)))
                (unless (string= branch (current-branch view))
                  (setf (active-dialog view)
                        (make-dialog :title "Delete Branch"
                                     :message (format nil "Delete branch ~A?" branch)
                                     :buttons '("Delete" "Cancel")))))))))
       nil)
      ;; Pop stash - 'P' or 'p' (when on files panel in stashes view)
      ((and (key-event-char key)
            (or (char= (key-event-char key) #\P) (char= (key-event-char key) #\p))
            (= focused-idx 1) (show-stashes view))
       (let* ((stashes (stash-list view))
              (selected (panel-selected (files-panel view))))
         (when (and stashes (< selected (length stashes)))
           (let ((st (nth selected stashes)))
             (setf (active-dialog view)
                   (make-dialog :title "Pop Stash"
                                :message (format nil "Pop stash ~D: ~A? (apply and remove)"
                                                 (stash-index st)
                                                 (or (stash-message st) "WIP"))
                                :data (list :stash-index (stash-index st))
                                :buttons '("Pop" "Cancel"))))))
       nil)
      ;; Apply stash - 'Enter' (when on files panel in stashes view)
      ;; Note: This is handled in the existing Enter key handler below
      ;; Toggle views - 'w' (when on files or branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\w))
       (cond
         ;; Files panel - cycle Files -> Worktrees -> Stashes -> Files
         ((= focused-idx 1)
          (cond
            ((show-stashes view)
             ;; Stashes -> Files
             (setf (show-stashes view) nil)
             (setf (show-worktrees view) nil))
            ((show-worktrees view)
             ;; Worktrees -> Stashes
             (setf (show-worktrees view) nil)
             (setf (show-stashes view) t))
            (t
             ;; Files -> Worktrees
             (setf (show-worktrees view) t)))
          (setf (panel-selected (files-panel view)) 0)
          (refresh-data view))
         ;; Branches panel - cycle Local -> Remotes -> Tags -> Submodules -> Local
         ((= focused-idx 2)
          (cond
            ((show-submodules view)
             (setf (show-submodules view) nil)
             (setf (show-tags view) nil)
             (setf (show-remote-branches view) nil))
            ((show-tags view)
             (setf (show-tags view) nil)
             (setf (show-submodules view) t))
            ((show-remote-branches view)
             (setf (show-remote-branches view) nil)
             (setf (show-tags view) t))
            (t
             (setf (show-remote-branches view) t)))
          (setf (panel-selected (branches-panel view)) 0)  ; Reset selection
          (refresh-data view)))
       nil)
      ;; Merge - 'M' (capital, when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\M))
       (when (= focused-idx 2)  ; Branches panel
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (unless (string= branch (current-branch view))
                 (setf (active-dialog view)
                       (make-dialog :title "Merge Branch"
                                    :message (format nil "Merge ~A into ~A?" 
                                                     branch (current-branch view))
                                    :data (list :branch branch)
                                    :buttons '("Merge" "Squash" "Cancel")))
                 ;; Store branch name for later use
                 (setf (dialog-message (active-dialog view))
                       (format nil "~A|~A" branch (current-branch view))))))))
       nil)
      ;; Rename branch - 'N' (capital, when on branches panel, local view)
      ((and (key-event-char key) (char= (key-event-char key) #\N))
       (when (and (= focused-idx 2) (not (show-remote-branches view))
                  (not (show-tags view)) (not (show-submodules view)))
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (setf (active-dialog view)
                     (make-dialog :title "Rename Branch"
                                  :message (format nil "Rename '~A' to:" branch)
                                  :input-mode t
                                  :data (list :old-name branch)
                                  :buttons '("Rename" "Cancel")))))))
       nil)
      ;; Fast-forward branch - 'F' (capital, when on branches panel, local view)
      ((and (key-event-char key) (char= (key-event-char key) #\F)
            (= focused-idx 2))
       (when (and (= focused-idx 2) (not (show-remote-branches view))
                  (not (show-tags view)) (not (show-submodules view)))
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (unless (string= branch (current-branch view))
                 (setf (active-dialog view)
                       (make-dialog :title "Fast-Forward Branch"
                                    :message (format nil "Fast-forward ~A to match upstream?" branch)
                                    :data (list :branch branch)
                                    :buttons '("Fast-Forward" "Cancel"))))))))
       nil)
      ;; Rebase onto - 'R' (capital, when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\R)
            (= focused-idx 2))
       (when (and (= focused-idx 2) (not (show-remote-branches view))
                  (not (show-tags view)) (not (show-submodules view)))
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (unless (string= branch (current-branch view))
                 (setf (active-dialog view)
                       (make-dialog :title "Rebase Branch"
                                    :message (format nil "Rebase ~A onto ~A?"
                                                     (current-branch view) branch)
                                    :data (list :branch branch)
                                    :buttons '("Rebase" "Cancel"))))))))
       nil)
      ;; Update submodule - 'U' (capital, when on branches panel in submodules view)
      ((and (key-event-char key) (char= (key-event-char key) #\U))
       (when (and (= focused-idx 2) (show-submodules view))
         (let* ((submodules (submodule-list view))
                (selected (panel-selected panel)))
           (if (and submodules (< selected (length submodules)))
               ;; Update selected submodule
               (let ((sm (nth selected submodules)))
                 (setf (active-dialog view)
                       (make-dialog :title "Update Submodule"
                                    :message (format nil "Update submodule '~A'?" (submodule-name sm))
                                    :data (list :submodule-path (submodule-path sm))
                                    :buttons '("Update" "Update All" "Cancel"))))
               ;; No submodules or invalid selection - offer to update all
               (setf (active-dialog view)
                     (make-dialog :title "Update Submodules"
                                  :message "Update all submodules?"
                                  :buttons '("Update All" "Cancel"))))))
       nil)
      ;; Add remote - 'A' (capital, when on branches panel in remotes view)
      ((and (key-event-char key) (char= (key-event-char key) #\A))
       (when (and (= focused-idx 2) (show-remote-branches view))
         (setf (active-dialog view)
               (make-dialog :title "Add Remote"
                            :message "Remote name:"
                            :input-mode t
                            :data (list :step :name)
                            :buttons '("Next" "Cancel"))))
       nil)
      ;; Rename remote - 'R' (capital, when on branches panel in remotes view)
      ((and (key-event-char key) (char= (key-event-char key) #\R)
            (= focused-idx 2))
       (when (and (= focused-idx 2) (show-remote-branches view))
         (let* ((remote-branches (remote-branch-list view))
                (selected (panel-selected panel)))
           (when (and remote-branches (< selected (length remote-branches)))
             (let* ((remote-branch (nth selected remote-branches))
                    (remote (first (cl-ppcre:split "/" remote-branch :limit 2))))
               (setf (active-dialog view)
                     (make-dialog :title "Rename Remote"
                                  :message (format nil "New name for '~A':" remote)
                                  :input-mode t
                                  :data (list :old-name remote)
                                  :buttons '("Rename" "Cancel")))))))
       nil)
       ;; Cherry-pick from branch - 'C' (capital, when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\C)
            (= focused-idx 2))
       (when (= focused-idx 2)  ; Branches panel
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (unless (string= branch (current-branch view))
                 ;; Get commits unique to that branch
                 (let ((commits (git-log-branch-only branch :count 50)))
                   (if commits
                       (progn
                         (setf (cherry-pick-mode view) t)
                         (setf (cherry-pick-branch view) branch)
                         (setf (cherry-pick-commits view) commits)
                         (setf (panel-selected (main-panel view)) 0)
                         ;; Format commits for display
                         (setf (panel-items (main-panel view))
                               (loop for c in commits
                                     collect (format-cherry-pick-commit c))))
                       ;; No unique commits
                       (setf (active-dialog view)
                             (make-dialog :title "Cherry Pick"
                                          :message (format nil "No commits in ~A that aren't already in ~A"
                                                           branch (current-branch view))
                                          :buttons '("OK"))))))))))
       nil)
      ;; Squash commits - 'S' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\S))
       (when (= focused-idx 3)  ; Commits panel
         (let ((selected (panel-selected panel)))
           (when (> selected 0)  ; Need at least 2 commits to squash
             (let ((count (1+ selected)))  ; selected is 0-indexed, squash from HEAD to selected
               (setf (active-dialog view)
                     (make-dialog :title "Squash Commits"
                                  :message (format nil "Squash last ~D commits into one" count)
                                  :input-mode t
                                  :multiline t
                                  :data (list :count count)
                                  :buttons '("Squash" "Cancel")))))))
       nil)
      ;; Cherry-pick - 'C' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\C)
            (= focused-idx 3))
       (when (= focused-idx 3)  ; Commits panel
         (let* ((commits (commit-list view))
                (selected (panel-selected panel)))
           (when (and commits (< selected (length commits)))
             (let* ((commit (nth selected commits))
                    (hash (log-entry-hash commit))
                    (short-hash (log-entry-short-hash commit))
                    (msg (log-entry-message commit)))
               (setf (active-dialog view)
                     (make-dialog :title "Cherry Pick"
                                  :message (format nil "Cherry-pick ~A: ~A?" short-hash msg)
                                  :data (list :hash hash)
                                  :buttons '("Pick" "Cancel")))))))
       nil)
      ;; Revert - 'R' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\R)
            (= focused-idx 3))
       (when (= focused-idx 3)  ; Commits panel
         (let* ((commits (commit-list view))
                (selected (panel-selected panel)))
           (when (and commits (< selected (length commits)))
             (let* ((commit (nth selected commits))
                    (hash (log-entry-hash commit))
                    (short-hash (log-entry-short-hash commit))
                    (msg (log-entry-message commit)))
               (setf (active-dialog view)
                     (make-dialog :title "Revert Commit"
                                  :message (format nil "Revert ~A: ~A?" short-hash msg)
                                  :data (list :hash hash)
                                  :buttons '("Revert" "Cancel")))))))
       nil)
      ;; Amend commit - 'A' (capital, when on commits panel with HEAD selected)
      ((and (key-event-char key) (char= (key-event-char key) #\A))
       (when (= focused-idx 3)  ; Commits panel
         (let ((selected (panel-selected panel)))
           (when (= selected 0)  ; Only HEAD can be amended
             (setf (active-dialog view)
                   (make-dialog :title "Amend Commit"
                                :message "Amend HEAD with staged changes?"
                                :buttons '("Amend" "Amend with new message" "Cancel"))))))
       nil)
      ;; Reset to commit - 'X' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\X)
            (= focused-idx 3))
       (when (= focused-idx 3)  ; Commits panel
         (let* ((commits (commit-list view))
                (selected (panel-selected panel)))
           (when (and commits (< selected (length commits)))
             (let* ((commit (nth selected commits))
                    (hash (log-entry-hash commit))
                    (short-hash (log-entry-short-hash commit))
                    (msg (log-entry-message commit)))
               (setf (active-dialog view)
                     (make-dialog :title "Reset to Commit"
                                  :message (format nil "Reset to ~A: ~A?" short-hash msg)
                                  :data (list :hash hash)
                                  :buttons '("Soft" "Mixed" "Hard" "Cancel")))))))
       nil)
      ;; Fixup commit - 'F' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\F)
            (= focused-idx 3))
       (when (= focused-idx 3)  ; Commits panel
         (let* ((commits (commit-list view))
                (selected (panel-selected panel)))
           (when (and commits (< selected (length commits)) (> selected 0))
             (let* ((commit (nth selected commits))
                    (hash (log-entry-hash commit))
                    (short-hash (log-entry-short-hash commit)))
               (setf (active-dialog view)
                     (make-dialog :title "Fixup Commit"
                                  :message (format nil "Create fixup! commit for ~A?" short-hash)
                                  :data (list :hash hash)
                                  :buttons '("Fixup" "Cancel")))))))
       nil)
      ;; Interactive rebase - 'i' (when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\i))
       (when (= focused-idx 3)  ; Commits panel
         (let ((selected (panel-selected panel)))
           (when (> selected 0)  ; Need at least 2 commits
             (let* ((count (1+ selected))
                    (base-commit (format nil "HEAD~~~D" count))
                    (entries (git-rebase-todo-list base-commit)))
               (when entries
                 (setf (rebase-mode view) t)
                 (setf (rebase-entries view) entries)
                 (setf (rebase-base-commit view) base-commit)
                 (setf (panel-title (commits-panel view))
                       "[4] Interactive Rebase  p:pick r:reword s:squash f:fixup d:drop J/K:move Enter:go q:cancel")
                 (setf (panel-selected (commits-panel view)) 0)
                 (update-rebase-display view))))))
       nil)
      ;; Search/filter - '/' (context-sensitive)
      ((and (key-event-char key) (char= (key-event-char key) #\/))
       (cond
         ;; Commits panel - search commits (existing behavior)
         ((= focused-idx 3)
          (setf (active-dialog view)
                (make-dialog :title "Search Commits"
                             :input-mode t
                             :buttons '("Search" "Cancel"))))
         ;; Files panel - filter files
         ((= focused-idx 1)
          (setf (active-dialog view)
                (make-dialog :title "Filter Files"
                             :input-mode t
                             :buttons '("Filter" "Clear" "Cancel"))))
         ;; Branches panel - filter branches
         ((= focused-idx 2)
          (setf (active-dialog view)
                (make-dialog :title "Filter Branches"
                             :input-mode t
                             :buttons '("Filter" "Clear" "Cancel"))))
         ;; Stash panel - filter stashes
         ((= focused-idx 4)
          (setf (active-dialog view)
                (make-dialog :title "Filter Stashes"
                             :input-mode t
                             :buttons '("Filter" "Clear" "Cancel")))))
       nil)
      ;; Bisect - 'b' (when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\b)
            (= focused-idx 3))
       (when (= focused-idx 3)  ; Commits panel
         (if (bisect-mode view)
             ;; During bisect: mark current HEAD as bad
             (let ((result (git-bisect-bad)))
               (log-command view "git bisect bad")
               (log-command view result)
               (refresh-data view)
               (when (search "is the first bad commit" result)
                 (setf (bisect-mode view) nil)
                 (log-command view "Bisect complete! Found the bad commit.")))
             ;; Start bisect: open options dialog
             (let* ((commits (commit-list view))
                    (selected (panel-selected panel)))
               (when (and commits (< selected (length commits)))
                 (let ((commit (nth selected commits)))
                   (setf (active-dialog view)
                         (make-dialog :title "Bisect"
                                      :message (format nil "Start bisect with ~A as bad commit?"
                                                       (log-entry-short-hash commit))
                                      :data (list :commit-hash (log-entry-hash commit))
                                      :buttons '("Start (bad)" "Cancel"))))))))
       nil)
      ;; Bisect good - 'g' (when on commits panel in bisect mode)
      ((and (key-event-char key) (char= (key-event-char key) #\g)
            (= focused-idx 3) (bisect-mode view))
       (when (and (= focused-idx 3) (bisect-mode view))
         (let ((result (git-bisect-good)))
           (log-command view "git bisect good")
           (log-command view result)
           (refresh-data view)
           (when (search "is the first bad commit" result)
             (setf (bisect-mode view) nil)
             (log-command view "Bisect complete! Found the bad commit."))))
       nil)
      ;; Bisect skip - 'S' (capital, when on commits panel in bisect mode)
      ;; Note: 'S' is squash on commits panel normally, but in bisect mode it becomes skip
      ;; Bisect reset - 'Q' (capital, when in bisect mode on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\Q)
            (= focused-idx 3) (bisect-mode view))
       (when (and (= focused-idx 3) (bisect-mode view))
         (log-command view "git bisect reset")
         (git-bisect-reset)
         (setf (bisect-mode view) nil)
         (setf (panel-title (commits-panel view))
               (numbered-title 4 "Commits" '("Reflog")))
         (refresh-data view))
       nil)
      ;; Blame view - 'b' (when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\b)
            (= focused-idx 1))
       (when (= focused-idx 1)  ; Files panel
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let* ((entry (nth selected entries))
                    (file (status-entry-file entry)))
               ;; Get blame data for the file
               (let ((blame (git-blame file)))
                 (when blame
                   (setf (blame-mode view) t)
                   (setf (blame-data view) blame)
                   (setf (blame-file view) file)
                   (setf (panel-selected (main-panel view)) 0)
                   ;; Set main panel as focused for selection highlight
                   (setf (panel-focused (main-panel view)) t)
                   ;; Format blame lines with colors for display
                   (setf (panel-items (main-panel view))
                         (loop for bl in blame
                               collect (format-blame-line bl)))))))))
       nil)
      ;; Edit/Hunk staging - 'e' (when on files panel)
      ;; For conflicts: spawn editor. For normal files: hunk staging mode
      ((and (key-event-char key) (char= (key-event-char key) #\e))
       (when (= focused-idx 1)  ; Files panel
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let* ((entry (nth selected entries))
                    (file (status-entry-file entry))
                    (status (status-entry-status entry)))
               (cond
                 ;; Conflict - spawn editor to resolve
                 ((eq status :conflict)
                  (log-command view (format nil "$EDITOR ~A" file))
                  ;; Restore terminal before spawning editor
                  (gilt.terminal:restore-terminal)
                  (git-edit-file file)
                  ;; Re-enter raw mode after editor exits
                  (gilt.terminal:setup-terminal)
                  (clear-screen)
                  (refresh-data view))
                 ;; Normal file - hunk staging mode
                 ((not (or (status-entry-staged-p entry)
                           (eq status :untracked)))
                  (let ((hunks (parse-diff-hunks file)))
                    (when hunks
                      (setf (hunk-list view) hunks)
                      (setf (hunk-mode view) t)
                      (setf (panel-items (main-panel view))
                            (loop for hunk in hunks
                                  for i from 1
                                  collect (format nil "Hunk ~D: ~A (+~D lines)"
                                                  i (hunk-header hunk)
                                                  (hunk-line-count hunk))))))))))))
       nil)
      ;; Custom command keybindings fallback
      ((and (key-event-char key)
            (let ((custom-cmds (load-custom-commands)))
              (when custom-cmds
                (let ((entry (assoc (key-event-char key) custom-cmds)))
                  (when entry
                    (let* ((cmd (cdr entry))
                           (output (git-shell-command cmd)))
                      (log-command view (format nil "$ ~A" cmd))
                      (when (and output (> (length output) 0))
                        (log-command view (string-trim '(#\Newline) output)))
                      (refresh-data view)
                      t))))))
       nil)
      (t nil))))

