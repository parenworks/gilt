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
   (show-remote-branches :accessor show-remote-branches :initform nil)))

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
  ;; Status panel - repo info
  (let ((branch (git-current-branch))
        (repo (git-repo-name)))
    (setf (current-branch view) branch)
    (setf (panel-items (status-panel view))
          (list (format nil "~A → ~A" repo branch))))
  ;; Files panel - git status
  (let ((entries (git-status)))
    (setf (status-entries view) entries)
    (setf (panel-items (files-panel view))
          (loop for e in entries
                collect (format-status-entry e))))
  ;; Branches panel - local or remote based on toggle
  (let ((branches (git-branches))
        (remote-branches (git-remote-branches)))
    (setf (branch-list view) branches)
    (setf (remote-branch-list view) remote-branches)
    ;; Update panel title and items based on mode
    (if (show-remote-branches view)
        (progn
          (setf (panel-title (branches-panel view))
                (numbered-title 3 "Remotes" '("Local" "Tags")))
          (setf (panel-items (branches-panel view))
                (loop for b in remote-branches
                      collect (list :colored :bright-cyan (format nil "  ~A" b)))))
        (progn
          (setf (panel-title (branches-panel view))
                (numbered-title 3 "Local branches" '("Remotes" "Tags")))
          (setf (panel-items (branches-panel view))
                (loop for b in branches
                      collect (if (string= b (current-branch view))
                                  (list :colored :bright-green (format nil "* ~A" b))
                                  (format nil "  ~A" b)))))))
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
  "Format a status entry for display with color"
  (let* ((status (status-entry-status entry))
         (indicator (case status
                      (:modified "M")
                      (:added "A")
                      (:deleted "D")
                      (:untracked "?")
                      (:renamed "R")
                      (:conflict "!")
                      (t " ")))
         (color (case status
                  (:modified :bright-yellow)
                  (:added :bright-green)
                  (:deleted :bright-red)
                  (:untracked :bright-magenta)
                  (:renamed :bright-cyan)
                  (:conflict :bright-red)  ; Conflicts shown in bright red with !
                  (t :white)))
         (text (format nil "~A ~A" indicator (status-entry-file entry))))
    (list :colored color text)))

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

(defun update-main-content (view)
  "Update main panel based on focused panel and selection"
  (let* ((focused-idx (view-focused-panel view))
         (panel (nth focused-idx (view-panels view)))
         (selected (panel-selected panel)))
    (cond
      ;; Files panel focused - show diff
      ((= focused-idx 1)
       (let ((entries (status-entries view)))
         (when (and entries (< selected (length entries)))
           (let* ((entry (nth selected entries))
                  (file (status-entry-file entry))
                  (diff (if (status-entry-staged-p entry)
                            (git-diff-staged file)
                            (git-diff file))))
             (setf (panel-title (main-panel view)) "[0] Diff")
             (setf (panel-items (main-panel view))
                   (cl-ppcre:split "\\n" diff))))))
      ;; Commits panel focused - show commit details
      ((= focused-idx 3)
       (let ((commits (commit-list view)))
         (when (and commits (< selected (length commits)))
           (let ((commit (nth selected commits)))
             (setf (panel-title (main-panel view)) "[0] Commit")
             (setf (panel-items (main-panel view))
                   (list (format nil "Hash: ~A" (log-entry-hash commit))
                         (format nil "Author: ~A" (log-entry-author commit))
                         (format nil "Date: ~A" (log-entry-date commit))
                         ""
                         (log-entry-message commit)))))))
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
     '(("j/k" . "navigate") ("Tab" . "panels") ("q" . "quit")))
    (1 ; Files panel
     '(("j/k" . "navigate") ("Space" . "stage/unstage") ("e" . "edit/hunks") ("d" . "discard")
       ("o" . "use ours") ("t" . "use theirs") ("X" . "abort merge") ("c" . "commit") ("q" . "quit")))
    (2 ; Branches panel
     '(("j/k" . "navigate") ("Enter" . "checkout/track") ("n" . "new") ("f" . "fetch")
       ("w" . "local/remote") ("M" . "merge") ("D" . "delete") ("q" . "quit")))
    (3 ; Commits panel
     '(("j/k" . "navigate") ("S" . "squash") ("C" . "cherry-pick") ("R" . "revert") ("q" . "quit")))
    (4 ; Stash panel
     '(("j/k" . "navigate") ("s" . "stash") ("g" . "pop") ("Tab" . "panels") ("q" . "quit")))
    (t ; Default
     '(("j/k" . "navigate") ("Tab" . "panels") ("q" . "quit")))))

(defmethod draw-view ((view main-view) width height)
  ;; LazyGit layout:
  ;; Left column: 5 stacked panels - focused panel expands (wider ~40%)
  ;; Right column: main content panel (top) + command log (bottom)
  (let* ((left-width (max 50 (floor (* width 2) 5)))  ; Left gets ~40% of width
         (right-width (- width left-width))
         (usable-height (- height 1))  ; Leave 1 row for help bar
         (focused-idx (view-focused-panel view))
         ;; Right side split: main panel gets most, cmdlog gets 9 rows
         (cmdlog-height 9)
         (main-height (- usable-height cmdlog-height))
         ;; Base heights for left panels - small for unfocused, larger for focused
         (min-h 3)  ; Minimum height for collapsed panels
         ;; Calculate expanded height for focused panel
         (total-min (* 5 min-h))
         (extra-space (- usable-height total-min))
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
    ;; Update focus state
    (loop for panel in (view-panels view)
          for i from 0
          do (setf (panel-focused panel) (= i focused-idx)))
    ;; Draw all panels
    (dolist (panel (view-panels view))
      (draw-panel panel))
    ;; Draw command log panel (not in focus cycle)
    (draw-panel (cmdlog-panel view))
    ;; Draw context-specific help bar at bottom
    (draw-help-bar height width (get-panel-help focused-idx))
    ;; Draw spinner in bottom left if active
    (when (spinner-active view)
      (draw-spinner view height))
    ;; Store screen dimensions for dialogs
    (setf (screen-width view) width
          (screen-height view) height)
    ;; Draw active dialog on top if present
    (when (active-dialog view)
      (draw-dialog (active-dialog view) width height))))

(defparameter *spinner-chars* '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner animation frames")

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
                     (branch (first parts)))
                (when branch
                  (log-command view (format nil "git merge ~A" branch))
                  (git-merge branch)
                  (refresh-data view))))
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
             ;; Push dialog
             ((string= (dialog-title dlg) "Push")
              (log-command view "git push")
              (show-status-message "Pushing..." (screen-width view) (screen-height view))
              (finish-output *terminal-io*)
              (git-push)
              (log-command view "git push completed")
              ;; Clear dialog first, then refresh to force full redraw
              (setf (active-dialog view) nil)
              (refresh-data view)
              (clear-screen)  ; Force full screen clear
              (return-from handle-key nil))
             ;; Pull dialog
             ((string= (dialog-title dlg) "Pull")
              (log-command view "git pull")
              (show-status-message "Pulling..." (screen-width view) (screen-height view))
              (finish-output *terminal-io*)
              (git-pull)
              (log-command view "git pull completed")
              ;; Clear dialog first, then refresh to force full redraw
              (setf (active-dialog view) nil)
              (refresh-data view)
              (clear-screen)  ; Force full screen clear
              (return-from handle-key nil))
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
                  (refresh-data view))))
             ;; Revert dialog
             ((string= (dialog-title dlg) "Revert Commit")
              (let ((hash (getf (dialog-data dlg) :hash)))
                (when hash
                  (log-command view (format nil "git revert ~A" hash))
                  (git-revert hash)
                  (refresh-data view))))
             ;; Abort Merge dialog
             ((string= (dialog-title dlg) "Abort Merge")
              (log-command view "git merge --abort")
              (git-merge-abort)
              (refresh-data view))))
         (setf (active-dialog view) nil))
        ((eq result :cancel)
         (setf (active-dialog view) nil))
        (t
         ;; Dialog is still active, just redraw the dialog only
         (draw-dialog (active-dialog view) (screen-width view) (screen-height view)))))
    ;; ALWAYS return here when dialog is active - never fall through to main key handling
    (return-from handle-key :dialog))
  
  ;; Handle hunk mode first
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
  
  (let* ((focused-idx (view-focused-panel view))
         (panel (nth focused-idx (view-panels view))))
    (cond
      ;; Quit
      ((and (key-event-char key) (char= (key-event-char key) #\q))
       :quit)
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
       (setf (view-focused-panel view)
             (mod (1+ focused-idx) 5))  ; Cycle through 5 left panels
       (update-main-content view)
       nil)
      ;; Switch to previous panel - h
      ((and (key-event-char key) (char= (key-event-char key) #\h))
       (setf (view-focused-panel view)
             (mod (1- focused-idx) 5))
       (update-main-content view)
       nil)
      ;; Direct panel selection with number keys
      ((and (key-event-char key) (char= (key-event-char key) #\1))
       (setf (view-focused-panel view) 0)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\2))
       (setf (view-focused-panel view) 1)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\3))
       (setf (view-focused-panel view) 2)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\4))
       (setf (view-focused-panel view) 3)
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\5))
       (setf (view-focused-panel view) 4)
       (update-main-content view)
       nil)
      ;; Stage/unstage with space (when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\Space))
       (when (= focused-idx 1)  ; Files panel
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
       nil)
      ;; Enter on branches - checkout local or track remote
      ((eq (key-event-code key) +key-enter+)
       (when (= focused-idx 2)  ; Branches panel
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
                   (refresh-data view))))))
       nil)
      ;; Refresh
      ((and (key-event-char key) (char= (key-event-char key) #\r))
       (refresh-data view)
       nil)
      ;; Commit - 'c' opens commit dialog (multiline)
      ((and (key-event-char key) (char= (key-event-char key) #\c))
       (setf (active-dialog view)
             (make-dialog :title "Commit Message"
                          :input-mode t
                          :multiline t
                          :buttons '("Commit" "Cancel")))
       nil)
      ;; Push - 'P' (capital) opens push confirmation
      ((and (key-event-char key) (char= (key-event-char key) #\P))
       (setf (active-dialog view)
             (make-dialog :title "Push"
                          :message "Push to origin?"
                          :buttons '("Push" "Cancel")))
       nil)
      ;; Pull - 'p' (lowercase) opens pull confirmation
      ((and (key-event-char key) (char= (key-event-char key) #\p))
       (setf (active-dialog view)
             (make-dialog :title "Pull"
                          :message "Pull from origin?"
                          :buttons '("Pull" "Cancel")))
       nil)
      ;; Stage all - 'a'
      ((and (key-event-char key) (char= (key-event-char key) #\a))
       (log-command view "git add -A")
       (git-stage-all)
       (refresh-data view)
       nil)
      ;; Stash - 's' (when on stash panel or files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\s))
       (when (or (= focused-idx 4) (= focused-idx 1))  ; Stash or Files panel
         (log-command view "git stash")
         (git-stash)
         (refresh-data view))
       nil)
      ;; Stash pop - 'g' (when on stash panel)
      ((and (key-event-char key) (char= (key-event-char key) #\g))
       (when (= focused-idx 4)  ; Stash panel
         (log-command view "git stash pop")
         (git-stash-pop)
         (refresh-data view))
       nil)
      ;; Resolve conflict with ours - 'o' (when on files panel with conflict)
      ((and (key-event-char key) (char= (key-event-char key) #\o))
       (when (= focused-idx 1)  ; Files panel
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let ((entry (nth selected entries)))
               (when (eq (status-entry-status entry) :conflict)
                 (log-command view (format nil "git checkout --ours ~A && git add ~A" 
                                           (status-entry-file entry) (status-entry-file entry)))
                 (git-resolve-with-ours (status-entry-file entry))
                 (refresh-data view))))))
       nil)
      ;; Resolve conflict with theirs - 't' (when on files panel with conflict)
      ((and (key-event-char key) (char= (key-event-char key) #\t))
       (when (= focused-idx 1)  ; Files panel
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let ((entry (nth selected entries)))
               (when (eq (status-entry-status entry) :conflict)
                 (log-command view (format nil "git checkout --theirs ~A && git add ~A"
                                           (status-entry-file entry) (status-entry-file entry)))
                 (git-resolve-with-theirs (status-entry-file entry))
                 (refresh-data view))))))
       nil)
      ;; Abort merge - 'X' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\X))
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
      ;; Fetch - 'f' (when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\f))
       (when (= focused-idx 2)  ; Branches panel
         (log-command view "git fetch --all")
         (show-status-message "Fetching..." (screen-width view) (screen-height view))
         (git-fetch)
         (log-command view "git fetch completed")
         (refresh-data view))
       nil)
      ;; Toggle local/remote branches - 'w' (when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\w))
       (when (= focused-idx 2)  ; Branches panel
         (setf (show-remote-branches view) (not (show-remote-branches view)))
         (setf (panel-selected (branches-panel view)) 0)  ; Reset selection
         (refresh-data view))
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
                                    :buttons '("Merge" "Cancel")))
                 ;; Store branch name for later use
                 (setf (dialog-message (active-dialog view))
                       (format nil "~A|~A" branch (current-branch view))))))))
       nil)
      ;; Delete branch - 'D' (capital, when on branches panel)
      ((and (key-event-char key) (char= (key-event-char key) #\D))
       (when (= focused-idx 2)  ; Branches panel
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (unless (string= branch (current-branch view))
                 (setf (active-dialog view)
                       (make-dialog :title "Delete Branch"
                                    :message (format nil "Delete branch ~A?" branch)
                                    :buttons '("Delete" "Cancel"))))))))
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
      ((and (key-event-char key) (char= (key-event-char key) #\C))
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
      ((and (key-event-char key) (char= (key-event-char key) #\R))
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
                  (restore-terminal)
                  (git-edit-file file)
                  ;; Re-enter raw mode after editor exits
                  (setup-terminal)
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
      (t nil))))

;;; Legacy views for compatibility (redirect to main-view)

(defclass status-view (main-view) ())
(defclass log-view (main-view) ())
(defclass branches-view (main-view) ())
