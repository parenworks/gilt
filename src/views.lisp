(in-package #:gilt.views)

;;; Views - Main UI screens
;;; LazyGit-style layout: 5 stacked panels on left, main content on right

;;; View base class

(defclass view ()
  ((panels :initarg :panels :accessor view-panels :initform nil)
   (focused-panel :initarg :focused-panel :accessor view-focused-panel :initform 0)
   (needs-refresh :initarg :needs-refresh :accessor view-needs-refresh :initform t)))

(defgeneric draw-view (view width height)
  (:documentation "Draw the view to the terminal"))

(defgeneric handle-key (view key)
  (:documentation "Handle a key event, return :quit to exit, :switch to change view"))

(defgeneric refresh-data (view)
  (:documentation "Refresh data from git"))

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
   (screen-height :accessor screen-height :initform 24)))

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
  ;; Branches panel
  (let ((branches (git-branches)))
    (setf (branch-list view) branches)
    (setf (panel-items (branches-panel view))
          (loop for b in branches
                collect (if (string= b (current-branch view))
                            (format nil "* ~A" b)
                            (format nil "  ~A" b)))))
  ;; Commits panel
  (let ((commits (git-log :count 50)))
    (setf (commit-list view) commits)
    (setf (panel-items (commits-panel view))
          (loop for c in commits
                collect (format nil "~A ~A"
                                (log-entry-short-hash c)
                                (log-entry-message c)))))
  ;; Stash panel
  (setf (panel-items (stash-panel view)) nil)
  ;; Main panel - show diff for selected file
  (update-main-content view))

(defun format-status-entry (entry)
  "Format a status entry for display"
  (format nil "~A ~A"
          (case (status-entry-status entry)
            (:modified "M")
            (:added "A")
            (:deleted "D")
            (:untracked "?")
            (:renamed "R")
            (t " "))
          (status-entry-file entry)))

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
     '(("j/k" . "navigate") ("Space" . "stage/unstage") ("a" . "stage all")
       ("c" . "commit") ("P" . "push") ("p" . "pull") ("q" . "quit")))
    (2 ; Branches panel
     '(("j/k" . "navigate") ("Enter" . "checkout") ("n" . "new branch")
       ("Tab" . "panels") ("q" . "quit")))
    (3 ; Commits panel
     '(("j/k" . "navigate") ("Enter" . "view") ("Tab" . "panels") ("q" . "quit")))
    (4 ; Stash panel
     '(("j/k" . "navigate") ("s" . "stash") ("p" . "pop") ("Tab" . "panels") ("q" . "quit")))
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
         ;; Right side split: main panel gets most, cmdlog gets 5 rows
         (cmdlog-height 5)
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
    ;; Store screen dimensions for dialogs
    (setf (screen-width view) width
          (screen-height view) height)
    ;; Draw active dialog on top if present
    (when (active-dialog view)
      (draw-dialog (active-dialog view) width height))))

(defmethod handle-key ((view main-view) key)
  ;; If dialog is active, handle dialog keys first
  (when (active-dialog view)
    (let ((result (handle-dialog-key (active-dialog view) key)))
      (cond
        ((eq result :ok)
         ;; Handle dialog confirmation based on dialog type
         (let ((dlg (active-dialog view)))
           (cond
             ;; Commit dialog
             ((string= (dialog-title dlg) "Commit")
              (let ((msg (dialog-input-buffer dlg)))
                (when (> (length msg) 0)
                  (log-command view (format nil "git commit -m \"~A\"" msg))
                  (git-commit msg)
                  (refresh-data view))))
             ;; Push dialog
             ((string= (dialog-title dlg) "Push")
              (log-command view "git push")
              (git-push)
              (refresh-data view))
             ;; Pull dialog
             ((string= (dialog-title dlg) "Pull")
              (log-command view "git pull")
              (git-pull)
              (refresh-data view))))
         (setf (active-dialog view) nil))
        ((eq result :cancel)
         (setf (active-dialog view) nil)))
      ;; Return nil to continue (dialog handled the key)
      (return-from handle-key nil)))
  
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
      ;; Enter on branches - checkout
      ((eq (key-event-code key) +key-enter+)
       (when (= focused-idx 2)  ; Branches panel
         (let* ((branches (branch-list view))
                (selected (panel-selected panel)))
           (when (and branches (< selected (length branches)))
             (let ((branch (nth selected branches)))
               (log-command view (format nil "git checkout ~A" branch))
               (git-checkout branch)
               (refresh-data view)))))
       nil)
      ;; Refresh
      ((and (key-event-char key) (char= (key-event-char key) #\r))
       (refresh-data view)
       nil)
      ;; Commit - 'c' opens commit dialog
      ((and (key-event-char key) (char= (key-event-char key) #\c))
       (setf (active-dialog view)
             (make-dialog :title "Commit"
                          :message "Enter commit message:"
                          :input-label "Message"
                          :input-mode t
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
      (t nil))))

;;; Legacy views for compatibility (redirect to main-view)

(defclass status-view (main-view) ())
(defclass log-view (main-view) ())
(defclass branches-view (main-view) ())
