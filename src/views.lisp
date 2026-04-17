(in-package #:gilt.views)

;;; Views - Main UI screens
;;; LazyGit-style layout: 5 stacked panels on left, main content on right

;;; Fuzzy matching

(defun fuzzy-match-p (query text)
  "Return T if QUERY fuzzy-matches TEXT. Characters must appear in order
   but need not be adjacent. Both are compared case-insensitively."
  (let ((qi 0)
        (qlen (length query))
        (tlen (length text)))
    (loop for ti from 0 below tlen
          while (< qi qlen) do
          (when (char-equal (char query qi) (char text ti))
            (incf qi)))
    (= qi qlen)))

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

(defun show-toast (view message &optional (duration 2.0))
  "Show a toast notification that auto-dismisses after DURATION seconds."
  (setf (toast-message view) message)
  (setf (toast-expiry view) (+ (get-internal-real-time)
                                (* duration internal-time-units-per-second))))

(defun toast-expired-p (view)
  "Return T if the toast has expired."
  (and (toast-message view)
       (> (get-internal-real-time) (toast-expiry view))))

(defun draw-toast (view width height)
  "Draw toast notification in bottom-right above help bar."
  (when (toast-message view)
    (if (toast-expired-p view)
        (setf (toast-message view) nil)
        (let* ((msg (toast-message view))
               (padded (format nil " ~A " msg))
               (len (length padded))
               (x (max 1 (- width len 1)))
               (y (- height 1)))
          (cursor-to y x)
          (bg (color-code :bright-cyan))
          (fg (color-code :black))
          (bold)
          (write-string padded *terminal-io*)
          (reset)
          (finish-output *terminal-io*)))))

;;; Credential prompting
;;; Detect when git asks for username, password, or passphrase and show a dialog.

(defun credential-prompt-p (text)
  "Return T if TEXT looks like a git credential prompt (username, password, passphrase)."
  (and (> (length text) 0)
       (let ((lower (string-downcase text)))
         (or (search "username" lower)
             (search "password" lower)
             (search "passphrase" lower)
             ;; Match patterns like "Username for 'https://github.com':"
             (and (search ":" lower)
                  (or (search "user" lower)
                      (search "token" lower)))))))

;;; Background auto-refresh and auto-fetch
;;; Auto-refresh polls git status periodically to detect external changes.
;;; Auto-fetch periodically runs git fetch in a background thread.
;;; Enable via git config gilt.autofetch true, configure interval with gilt.fetchinterval (seconds).
;;; Auto-refresh is always on (2s default), fetch must be explicitly enabled.

(defun detect-auto-fetch-config ()
  "Detect auto-fetch settings from git config or environment.
   Returns (values enabled-p fetch-interval refresh-interval)."
  (let ((fetch-enabled
          (or (let ((env (uiop:getenv "GILT_AUTO_FETCH")))
                (and env (member env '("1" "true" "yes") :test #'string-equal)))
              (ignore-errors
                (let ((val (string-trim '(#\Newline #\Space)
                                        (gilt.git:git-run "config" "--get" "gilt.autofetch"))))
                  (and val (member val '("true" "1" "yes") :test #'string-equal))))))
        (fetch-interval
          (or (ignore-errors
                (let ((env (uiop:getenv "GILT_FETCH_INTERVAL")))
                  (when env (parse-integer env :junk-allowed t))))
              (ignore-errors
                (let ((val (string-trim '(#\Newline #\Space)
                                        (gilt.git:git-run "config" "--get" "gilt.fetchinterval"))))
                  (when (and val (> (length val) 0))
                    (parse-integer val :junk-allowed t))))
              60))
        (refresh-interval
          (or (ignore-errors
                (let ((env (uiop:getenv "GILT_REFRESH_INTERVAL")))
                  (when env (parse-integer env :junk-allowed t))))
              (ignore-errors
                (let ((val (string-trim '(#\Newline #\Space)
                                        (gilt.git:git-run "config" "--get" "gilt.refreshinterval"))))
                  (when (and val (> (length val) 0))
                    (parse-integer val :junk-allowed t))))
              2)))
    (values fetch-enabled
            (max 10 fetch-interval)      ; minimum 10 seconds between fetches
            (max 1 refresh-interval))))   ; minimum 1 second between refreshes

(defun init-auto-refresh (view)
  "Initialize auto-refresh and auto-fetch settings from config."
  (multiple-value-bind (fetch-enabled fetch-interval refresh-interval)
      (detect-auto-fetch-config)
    (setf (auto-fetch-enabled view) fetch-enabled)
    (setf (auto-fetch-interval view) fetch-interval)
    (setf (auto-refresh-interval view) refresh-interval)
    (let ((now (get-internal-real-time)))
      (setf (last-refresh-time view) now)
      (setf (last-fetch-time view) now))))

(defun seconds-since (timestamp)
  "Return the number of seconds elapsed since TIMESTAMP (internal-real-time units)."
  (/ (- (get-internal-real-time) timestamp)
     internal-time-units-per-second))

(defun start-background-fetch (view)
  "Start a background git fetch in a separate thread.
   Does nothing if a fetch is already in progress."
  (when (and (auto-fetch-enabled view)
             (not (fetch-in-progress view)))
    (setf (fetch-in-progress view) t)
    (setf (fetch-result view) nil)
    (setf (fetch-thread view)
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (progn
                   (gilt.git:git-fetch)
                   :ok)
               (error () :error)))
           :name "gilt-auto-fetch"))))

(defun check-background-fetch (view)
  "Check if a background fetch has completed. If so, process the result.
   Returns T if a fetch completed and data should be refreshed."
  (when (and (fetch-in-progress view)
             (fetch-thread view)
             (not (sb-thread:thread-alive-p (fetch-thread view))))
    ;; Thread finished - get result
    (let ((result (ignore-errors
                    (sb-thread:join-thread (fetch-thread view)))))
      (setf (fetch-result view) (or result :error))
      (setf (fetch-in-progress view) nil)
      (setf (fetch-thread view) nil)
      (setf (last-fetch-time view) (get-internal-real-time))
      (when (eq (fetch-result view) :ok)
        (show-toast view "Auto-fetch complete")
        t))))

(defun maybe-auto-refresh (view)
  "Check if it's time for an auto-refresh of git status.
   Returns T if a refresh was performed."
  (when (and (not (active-dialog view))
             (not (patch-builder-mode view))
             (not (commit-files-mode view))
             (not (rebase-mode view))
             (not (config-mode view))
             (not (blame-mode view))
             (>= (seconds-since (last-refresh-time view))
                  (auto-refresh-interval view)))
    (setf (last-refresh-time view) (get-internal-real-time))
    (refresh-data view)
    t))

(defun maybe-auto-fetch (view)
  "Check if it's time for a background fetch. Starts one if needed.
   Also checks for completed background fetches."
  ;; Check completed fetch first
  (let ((fetch-completed (check-background-fetch view)))
    ;; Start new fetch if interval elapsed
    (when (and (auto-fetch-enabled view)
               (not (fetch-in-progress view))
               (>= (seconds-since (last-fetch-time view))
                    (auto-fetch-interval view)))
      (start-background-fetch view))
    ;; If fetch completed, refresh data
    (when fetch-completed
      (refresh-data view)
      t)))

;;; Nerd Font icons
;;; Toggle with GILT_NERD_FONTS=1 env var or git config gilt.nerdfonts true

(defvar *nerd-fonts* :unset
  "Whether to use Nerd Font icons. :unset means not yet detected.")

(defun nerd-fonts-p ()
  "Return T if Nerd Font icons are enabled."
  (when (eq *nerd-fonts* :unset)
    (setf *nerd-fonts*
          (or (let ((env (uiop:getenv "GILT_NERD_FONTS")))
                (and env (member env '("1" "true" "yes") :test #'string-equal)))
              (ignore-errors
                (let ((val (string-trim '(#\Newline #\Space)
                                        (gilt.git:git-run "config" "--get" "gilt.nerdfonts"))))
                  (and val (member val '("true" "1" "yes") :test #'string-equal)))))))
  *nerd-fonts*)

;; Icon constants (Nerd Font codepoints)
(defun icon (name)
  "Return the Nerd Font icon string for NAME, or a plain fallback."
  (if (nerd-fonts-p)
      (case name
        ;; Panel titles
        (:status       (string #\UF46A))     ;; nf-oct-info
        (:files        (string #\UF0219))    ;; nf-md-file_multiple
        (:branches     (string #\UF062C))    ;; nf-md-source_branch
        (:commits      (string #\UF0718))    ;; nf-md-source_commit
        (:stash        (string #\UF01C0))    ;; nf-md-archive
        (:main         (string #\UF008E))    ;; nf-md-application
        ;; File statuses
        (:modified     (string #\UF040A))    ;; nf-md-pencil
        (:added        (string #\UF0415))    ;; nf-md-plus
        (:deleted      (string #\UF0374))    ;; nf-md-minus_circle
        (:untracked    (string #\UF02D4))    ;; nf-md-help_circle
        (:renamed      (string #\UF0453))    ;; nf-md-rename_box
        (:conflict     (string #\UF0026))    ;; nf-md-alert
        (:staged       (string #\UF0134))    ;; nf-md-check_circle
        ;; Branch indicators
        (:current      (string #\UF005D))    ;; nf-md-arrow_right
        (:branch       (string #\UF062C))    ;; nf-md-source_branch
        (:tag          (string #\UF04FE))    ;; nf-md-tag
        (:remote       (string #\UF0318))    ;; nf-md-cloud
        (:submodule    (string #\UF01DA))    ;; nf-md-book
        ;; Commit indicators
        (:head         (string #\UF192))     ;; nf-fa-circle
        (:commit       (string #\UF10C))     ;; nf-fa-circle_o
        ;; Actions
        (:search       (string #\UF002B))    ;; nf-md-magnify
        (:filter       (string #\UF0233))    ;; nf-md-filter
        (:copy         (string #\UF018D))    ;; nf-md-content_copy
        (:diff         (string #\UF0465))    ;; nf-md-compare (swap-horizontal)
        (:merge        (string #\UF0389))    ;; nf-md-merge
        (:rebase       (string #\UF0EC8))    ;; nf-md-swap_vertical
        ;; Misc
        (:folder       (string #\UF024B))    ;; nf-md-folder
        (:file         (string #\UF0214))    ;; nf-md-file
        (:lock         (string #\UF033E))    ;; nf-md-lock
        (:config       (string #\UF0493))    ;; nf-md-cog (settings)
        (:graph        (string #\UF0283))    ;; nf-md-graph (chart-line)
        (:toast        (string #\UF005D))    ;; nf-md-arrow_right
        (t ""))
      ;; Plain fallback - no icons
      ""))

;;; Helper to format panel title with number like LazyGit: [1] Status
(defun panel-icon (num)
  "Return the icon for panel number NUM when nerd fonts are enabled."
  (case num
    (0 (icon :main))
    (1 (icon :status))
    (2 (icon :files))
    (3 (icon :branches))
    (4 (icon :commits))
    (5 (icon :stash))
    (t "")))

(defun numbered-title (num title &optional tabs)
  "Format panel title with number prefix, optional icon, and optional tabs"
  (let ((ico (panel-icon num)))
    (if tabs
        (if (and (nerd-fonts-p) (> (length ico) 0))
            (format nil "[~D] ~A ~A ~{─ ~A~^ ~}" num ico title tabs)
            (format nil "[~D] ~A ~{─ ~A~^ ~}" num title tabs))
        (if (and (nerd-fonts-p) (> (length ico) 0))
            (format nil "[~D] ~A ~A" num ico title)
            (format nil "[~D] ~A" num title)))))

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
   (screen-mode :accessor screen-mode :initform :normal)
   ;; Accordion weight: how much extra space focused panel gets (0.0-1.0)
   (accordion-weight :accessor accordion-weight :initform 0.6)
   ;; Portrait mode: stack panels vertically on narrow terminals
   (portrait-mode :accessor portrait-mode :initform nil)
   (portrait-threshold :accessor portrait-threshold :initform 100)  ; auto-switch below this width
   ;; Graph mode for commits
   (graph-mode :accessor graph-mode :initform nil)
   ;; Line-level staging mode
   (line-select-mode :accessor line-select-mode :initform nil)
   (line-select-hunk :accessor line-select-hunk :initform nil)
   (line-selected-set :accessor line-selected-set :initform nil)
   ;; Toast notification
   (toast-message :accessor toast-message :initform nil)
   (toast-expiry :accessor toast-expiry :initform 0)
   ;; Commit files sub-view
   (commit-files-mode :accessor commit-files-mode :initform nil)
   (commit-files-hash :accessor commit-files-hash :initform nil)
   (commit-files-list :accessor commit-files-list :initform nil)
   (commit-files-selected :accessor commit-files-selected :initform 0)
   ;; Diff mode (compare two refs)
   (diff-mode :accessor diff-mode :initform nil)
   (diff-ref-a :accessor diff-ref-a :initform nil)
   (diff-ref-b :accessor diff-ref-b :initform nil)
   ;; Background auto-refresh and auto-fetch
   (auto-refresh-interval :accessor auto-refresh-interval :initform 2)  ; seconds between status refreshes
   (auto-fetch-interval :accessor auto-fetch-interval :initform 60)     ; seconds between background fetches
   (auto-fetch-enabled :accessor auto-fetch-enabled :initform nil)
   (last-refresh-time :accessor last-refresh-time :initform 0)
   (last-fetch-time :accessor last-fetch-time :initform 0)
   (fetch-thread :accessor fetch-thread :initform nil)
   (fetch-result :accessor fetch-result :initform nil)     ; :ok, :error, or nil (pending)
   (fetch-in-progress :accessor fetch-in-progress :initform nil)
   ;; Custom Patch Builder
   (patch-builder-mode :accessor patch-builder-mode :initform nil)
   (patch-builder-hash :accessor patch-builder-hash :initform nil)
   (patch-builder-files :accessor patch-builder-files :initform nil)   ; alist of (file . hunks)
   (patch-builder-selected :accessor patch-builder-selected :initform nil) ; hash-table: file -> set of hunk indices
   (patch-builder-current-file :accessor patch-builder-current-file :initform nil)
   (patch-builder-hunk-view :accessor patch-builder-hunk-view :initform nil)))

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
  ;; Initialize auto-refresh and auto-fetch from config
  (init-auto-refresh view)
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
                                    (lambda (e) (fuzzy-match-p (filter-query view)
                                                               (status-entry-file e)))
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
                             (let ((prefix (if (nerd-fonts-p)
                                                (format nil "  ~A " (icon :tag))
                                                "  ")))
                               (if annotated
                                   `(:multi-colored
                                     (:bright-yellow ,(format nil "~A~A" prefix name))
                                     (:bright-black ,(format nil " ~A" date)))
                                   `(:multi-colored
                                     (:yellow ,(format nil "~A~A" prefix name))
                                     (:bright-black ,(format nil " ~A" date)))))))))
      ((show-remote-branches view)
       (setf (panel-title (branches-panel view))
             (numbered-title 3 "Remotes" '("Tags" "Submodules")))
       (setf (panel-items (branches-panel view))
             (loop for b in remote-branches
                   collect (list :colored :bright-cyan
                                 (if (nerd-fonts-p)
                                     (format nil "  ~A ~A" (icon :remote) b)
                                     (format nil "  ~A" b))))))
      (t
       (let ((filtered-branches (if (and (filter-query view) (eql (filter-panel view) 2))
                                    (remove-if-not
                                     (lambda (b) (fuzzy-match-p (filter-query view) b))
                                     branches)
                                    branches)))
         (setf (panel-title (branches-panel view))
               (if (and (filter-query view) (eql (filter-panel view) 2))
                   (format nil "[3] Local (filter: ~A)" (filter-query view))
                   (numbered-title 3 "Local" '("Remotes" "Tags" "Submodules"))))
         (setf (panel-items (branches-panel view))
               (loop for b in filtered-branches
                     collect (if (string= b (current-branch view))
                                 (list :colored :bright-green
                                       (if (nerd-fonts-p)
                                           (format nil "~A ~A" (icon :current) b)
                                           (format nil "* ~A" b)))
                                 (format nil "  ~A" b))))))))
  ;; Commits panel - show hash (yellow), author initials, circle, and message
  (let* ((commits (git-log :count 50))
         (filtered-commits (if (and (filter-query view) (eql (filter-panel view) 3))
                               (remove-if-not
                                (lambda (c) (fuzzy-match-p (filter-query view)
                                                           (log-entry-message c)))
                                commits)
                               commits)))
    (setf (commit-list view) filtered-commits)
    (when (and (filter-query view) (eql (filter-panel view) 3))
      (setf (panel-title (commits-panel view))
            (format nil "[4] Commits (filter: ~A)" (filter-query view))))
    (setf (panel-items (commits-panel view))
          (loop for c in filtered-commits
                for i from 0
                collect (format-commit-entry c (= i 0)))))
  ;; Stash panel
  (let* ((stashes (git-stash-list))
         (filtered-stashes (if (and (filter-query view) (eql (filter-panel view) 4))
                               (remove-if-not
                                (lambda (s) (fuzzy-match-p (filter-query view)
                                                           (string-downcase s)))
                                stashes)
                               stashes)))
    (setf (stash-list view) stashes)
    (when (and (filter-query view) (eql (filter-panel view) 4))
      (setf (panel-title (stash-panel view))
            (format nil "[5] Stash (filter: ~A)" (filter-query view))))
    (setf (panel-items (stash-panel view)) filtered-stashes))
  ;; Main panel - show diff for selected file
  (update-main-content view))

(defun format-status-entry (entry)
  "Format a status entry for display with color.
   Staged files are shown in green, unstaged in their status color."
  (let* ((status (status-entry-status entry))
         (staged (status-entry-staged-p entry))
         (indicator (if (nerd-fonts-p)
                       (case status
                         (:modified (icon :modified))
                         (:added (icon :added))
                         (:deleted (icon :deleted))
                         (:untracked (icon :untracked))
                         (:renamed (icon :renamed))
                         (:conflict (icon :conflict))
                         (t " "))
                       (case status
                         (:modified "M")
                         (:added "A")
                         (:deleted "D")
                         (:untracked "?")
                         (:renamed "R")
                         (:conflict "!")
                         (t " "))))
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
          (push (list :colored :bright-cyan (format nil "  ~A~A/" (icon :folder) dir)) result))
        ;; Files in this directory
        (dolist (e (reverse (gethash dir dirs)))
          (let* ((file (status-entry-file e))
                 (slash-pos (position #\/ file :from-end t))
                 (basename (if slash-pos (subseq file (1+ slash-pos)) file))
                 (status (status-entry-status e))
                 (indicator (if (nerd-fonts-p)
                               (case status
                                 (:modified (icon :modified)) (:added (icon :added))
                                 (:deleted (icon :deleted)) (:untracked (icon :untracked))
                                 (:renamed (icon :renamed)) (:conflict (icon :conflict)) (t " "))
                               (case status
                                 (:modified "M") (:added "A") (:deleted "D")
                                 (:untracked "?") (:renamed "R") (:conflict "!") (t " "))))
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
         (indicator (if is-head
                       (if (nerd-fonts-p) (icon :head) "●")
                       (if (nerd-fonts-p) (icon :commit) "○"))))
    ;; Return as colored segments list for rich rendering
    (list :multi-colored
          (list :bright-yellow hash)
          (list :bright-cyan (format nil " ~A " initials))
          (list :bright-green indicator)
          (list :white (format nil " ~A" message)))))

(defvar *custom-pager* nil
  "Custom diff pager command (e.g. \"delta\" or \"diff-so-fancy\").
   When set, diff output is piped through this command instead of
   using the built-in colorizer. Set via git config gilt.pager or
   the GILT_PAGER environment variable.")

(defun detect-custom-pager ()
  "Detect a custom pager from git config or environment variable.
   Checks GILT_PAGER env var first, then git config gilt.pager."
  (or (uiop:getenv "GILT_PAGER")
      (ignore-errors
        (let ((val (string-trim '(#\Newline #\Space)
                                (gilt.git:git-run "config" "--get" "gilt.pager"))))
          (when (and val (> (length val) 0)) val)))))

(defun pipe-through-pager (text pager-cmd)
  "Pipe TEXT through PAGER-CMD and return the output as a string.
   Returns nil if the pager is not found or fails."
  (ignore-errors
    (let* ((parts (cl-ppcre:split "\\s+" pager-cmd))
           (program (first parts))
           (args (rest parts)))
      (with-input-from-string (input text)
        (with-output-to-string (output)
          (sb-ext:run-program program args
                              :input input
                              :output output
                              :error nil
                              :search t))))))

(defun format-diff-lines (diff-text)
  "Format diff text with colors for display.
   If a custom pager is configured, pipes through it first."
  (let ((pager (or *custom-pager* (detect-custom-pager))))
    (when (and pager (not *custom-pager*))
      (setf *custom-pager* pager))  ; Cache it
    (if pager
        ;; Use custom pager - output already contains ANSI codes
        (cl-ppcre:split "\\n" (or (pipe-through-pager diff-text pager)
                                   diff-text))
        ;; Built-in colorizer
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
                        (t line))))))

(defun build-accumulated-patch (view)
  "Build the full patch text from the patch builder's selections.
   Concatenates selected hunks for each file with proper patch headers."
  (with-output-to-string (out)
    (dolist (entry (patch-builder-files view))
      (let* ((file (car entry))
             (hunks (cdr entry))
             (sel (gethash file (patch-builder-selected view))))
        (when (and sel hunks)
          (format out "--- a/~A~%" file)
          (format out "+++ b/~A~%" file)
          (dolist (idx (sort (copy-list sel) #'<))
            (when (< idx (length hunks))
              (let ((hunk (nth idx hunks)))
                (format out "~{~A~%~}" (hunk-content hunk))))))))))

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
      ;; Commits panel focused - show commit details or graph
      ((= focused-idx 3)
       (if (graph-mode view)
           ;; Graph mode - show commit graph
           (let ((graph-lines (git-log-graph :count 100 :all t)))
             (setf (panel-title (main-panel view)) "[0] Graph (all branches)")
             (setf (panel-items (main-panel view))
                   (or graph-lines (list "No commits"))))
           ;; Normal mode - show commit details
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
                               message-lines)))))))
      ;; Branches panel focused
      ((= focused-idx 2)
       (setf (panel-title (main-panel view)) "[0] Branch Info")
       (setf (panel-items (main-panel view)) nil))
      ;; Stash panel focused - show stash diff
      ((= focused-idx 4)
       (let ((stashes (stash-list view)))
         (if (and stashes (< selected (length stashes)))
             (let* ((stash (nth selected stashes))
                    (diff (ignore-errors (git-stash-show (stash-index stash)))))
               (setf (panel-title (main-panel view)) "[0] Stash Diff")
               (setf (panel-items (main-panel view))
                     (if (and diff (> (length diff) 0))
                         (format-diff-lines diff)
                         (list "No diff available"))))
             (progn
               (setf (panel-title (main-panel view)) "[0] Stash")
               (setf (panel-items (main-panel view)) (list "No stashes"))))))
      ;; Default
      (t
       (setf (panel-title (main-panel view)) "[0] Main")
       (setf (panel-items (main-panel view)) nil)))))

(defun get-panel-help (focused-idx &optional view)
  "Return context-specific help bindings based on focused panel"
  (case focused-idx
    (0 ; Status panel
     '(("j/k" . "navigate") ("Tab" . "panels") ("r" . "refresh") ("q" . "quit")))
    (1 ; Files panel
     '(("j/k" . "navigate") ("Space" . "stage/unstage") ("e" . "edit/hunks") ("d" . "discard")
       ("c" . "commit") ("v" . "range") ("S" . "stash file") ("T" . "tree") ("I" . "ignore") ("x" . "difftool") ("y" . "copy") ("r" . "refresh") ("q" . "quit")))
    (2 ; Branches panel - context-sensitive based on sub-view
     (cond
       ((and view (show-remote-branches view))
        '(("j/k" . "navigate") ("w" . "local/remote") ("f" . "fetch") ("A" . "add remote")
          ("R" . "rename remote") ("D" . "delete") ("r" . "refresh") ("q" . "quit")))
       ((and view (show-tags view))
        '(("j/k" . "navigate") ("w" . "local/remote") ("D" . "delete") ("r" . "refresh") ("q" . "quit")))
       ((and view (show-submodules view))
        '(("j/k" . "navigate") ("w" . "local/remote") ("U" . "update") ("r" . "refresh") ("q" . "quit")))
       (t
        '(("j/k" . "navigate") ("Enter" . "checkout") ("n" . "new") ("N" . "rename")
          ("w" . "local/remote") ("M" . "merge") ("R" . "rebase") ("F" . "ff") ("d" . "diff") ("s" . "sort") ("D" . "delete") ("r" . "refresh") ("q" . "quit")))))
    (3 ; Commits panel
     '(("j/k" . "navigate") ("i" . "rebase") ("g" . "graph") ("X" . "reset") ("A" . "amend") ("C" . "cherry-pick") ("R" . "revert") ("S" . "squash") ("F" . "fixup") ("M" . "move to branch") ("t" . "tag") ("b" . "bisect") ("o" . "browser") ("r" . "refresh") ("q" . "quit")))
    (4 ; Stash panel
     '(("j/k" . "navigate") ("s" . "stash") ("g" . "pop") ("D" . "drop") ("r" . "refresh") ("q" . "quit")))
    (t ; Default
     '(("j/k" . "navigate") ("Tab" . "panels") ("r" . "refresh") ("q" . "quit")))))

(defun draw-landscape-layout (view width height)
  "Standard landscape layout: left side panels + right main/cmdlog."
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
         ;; Calculate expanded height for focused panel using accordion weight
         (total-min (* 5 min-h))
         (extra-space (max 0 (- usable-height total-min)))
         (weight (accordion-weight view))
         (expanded-extra (floor (* extra-space weight)))  ; Focused gets weight% of extra
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
          (panel-height (cmdlog-panel view)) cmdlog-height)))

(defun draw-portrait-layout (view width height)
  "Portrait/stacked layout for narrow terminals: focused side panel on top,
   main panel below. Other side panels get minimal height."
  (let* ((usable-height (max 1 (- height 1)))
         (focused-idx (view-focused-panel view))
         (all-panels (list (status-panel view) (files-panel view)
                           (branches-panel view) (commits-panel view)
                           (stash-panel view)))
         ;; In portrait: focused panel gets ~40% height, main gets ~50%, others get min
         (min-h 3)
         (other-panels-space (* 4 min-h))  ; 4 unfocused panels at min height
         (cmdlog-height (min 5 (floor usable-height 8)))
         (remaining (max 6 (- usable-height other-panels-space cmdlog-height)))
         (focused-h (max 5 (floor (* remaining 2) 5)))  ; 40% of remaining
         (main-h (max 3 (- remaining focused-h)))  ; Rest to main
         (cur-y 1))
    ;; Position all panels full-width, stacked vertically
    (loop for panel in all-panels
          for i from 0
          for h = (if (= i focused-idx) focused-h min-h)
          do (setf (panel-x panel) 1
                   (panel-y panel) cur-y
                   (panel-width panel) width
                   (panel-height panel) h)
             (incf cur-y h))
    ;; Main panel below all side panels
    (setf (panel-x (main-panel view)) 1
          (panel-y (main-panel view)) cur-y
          (panel-width (main-panel view)) width
          (panel-height (main-panel view)) main-h)
    (incf cur-y main-h)
    ;; Command log at bottom
    (let ((actual-cmdlog-h (max 3 (- (+ usable-height 1) cur-y))))
      (setf (panel-x (cmdlog-panel view)) 1
            (panel-y (cmdlog-panel view)) cur-y
            (panel-width (cmdlog-panel view)) width
            (panel-height (cmdlog-panel view)) actual-cmdlog-h))))

(defmethod draw-view ((view main-view) width height)
  ;; Auto-detect portrait mode based on terminal width
  (let ((use-portrait (or (portrait-mode view)
                          (and (not (portrait-mode view))
                               (< width (portrait-threshold view))))))
    ;; Layout
    (if use-portrait
        (draw-portrait-layout view width height)
        (draw-landscape-layout view width height))
    ;; Update focus state and draw
    (let ((focused-idx (view-focused-panel view))
          (main-panel-focused (panel-focused (main-panel view))))
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
                  (or (blame-mode view) (cherry-pick-mode view)))))
      ;; Draw all panels (includes main-panel which is 6th in view-panels)
      (dolist (panel (view-panels view))
        (draw-panel panel))
      ;; Draw command log panel (not in focus cycle)
      (draw-panel (cmdlog-panel view))
      ;; Draw context-specific help bar at bottom (with version from gilt package)
      (draw-help-bar height width (get-panel-help focused-idx view) 
                     (when (find-package :gilt) 
                       (symbol-value (find-symbol "*VERSION*" :gilt)))))
    ;; Draw spinner in bottom left if active
    (when (spinner-active view)
      (draw-spinner view height))
    ;; Store screen dimensions for dialogs
    (setf (screen-width view) width
          (screen-height view) height)
    ;; Draw toast notification
    (draw-toast view width height)
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
                       "   /          Search/filter commits"
                       "   Enter      Browse commit files"
                       "   M          Move commits to new branch"
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
                       "   d          Diff compare branch vs HEAD/ref"
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
                       "   /          Fuzzy filter (panels 2,3,4,5) / search commits"
                       ""
                       " CLIPBOARD / BROWSER"
                       "   y          Copy menu (file path/name, branch, hash/msg)"
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
                       "   _          Cycle screen mode reverse: full/half/normal"
                       "   |          Toggle portrait/landscape layout"
                       "              Auto-portrait: narrow terminals (<100 cols)"
                       "              Accordion: focused panel expands (weight: 0.6)"
                       ""
                       " CREDENTIAL PROMPTING"
                       "              Auto-detected during push/pull/fetch"
                       "              Shows input dialog for username/password/token"
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
                       "              Custom pager: GILT_PAGER env or git config gilt.pager"
                       ""
                       " CUSTOM PATCH BUILDER"
                       "   Enter      View commit files (from commits panel)"
                       "   p          Enter patch builder (from commit files view)"
                       "   Space      Toggle file/hunk selection"
                       "   Enter      Drill into hunks for a file"
                       "   a          Select all  /  n  Deselect all"
                       "   v          Preview accumulated patch"
                       "   P          Apply patch (to index, new commit, or reverse)"
                       "   Esc        Back (hunk view -> files, files -> exit)"
                       ""
                       " AUTO-REFRESH / AUTO-FETCH"
                       "              Auto-refresh: git status polled every 2s (default)"
                       "              Configure: git config gilt.refreshinterval <secs>"
                       "              Auto-fetch: git config gilt.autofetch true"
                       "              Interval:   git config gilt.fetchinterval <secs> (default 60)"
                       "              Env vars:   GILT_AUTO_FETCH, GILT_FETCH_INTERVAL, GILT_REFRESH_INTERVAL"
                       ""
                       " NERD FONTS"
                       "              GILT_NERD_FONTS=1 or git config gilt.nerdfonts true"
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
             ;; Stash File(s) dialog
             ((string= (dialog-title dlg) "Stash File(s)")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (when (string= selected-button "Stash")
                  (let ((files (getf (dialog-data dlg) :files))
                        (message (first (dialog-input-lines dlg))))
                    (when files
                      (let ((msg (if (and message (> (length message) 0)) message nil)))
                        (log-command view (format nil "git stash push ~@[-m \"~A\" ~]-- ~{~A~^ ~}" msg files))
                        (git-stash-push-files files msg)
                        (setf (range-select-start view) nil)
                        (refresh-data view)))))))
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
                      (dolist (line (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Return) output)))
                        (log-command view line)))
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
             ;; Diff Compare dialog
             ((string= (dialog-title dlg) "Diff Compare")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons))
                     (ref-a (getf (dialog-data dlg) :ref-a)))
                (cond
                  ((string= selected-button "HEAD")
                   (when ref-a
                     (let* ((diff (git-diff-refs "HEAD" ref-a
                                                 :context-size (diff-context-size view)
                                                 :ignore-whitespace (diff-ignore-whitespace view)))
                            (stat (git-diff-refs-stat "HEAD" ref-a)))
                       (setf (diff-mode view) t)
                       (setf (diff-ref-a view) "HEAD")
                       (setf (diff-ref-b view) ref-a)
                       (setf (panel-title (main-panel view))
                             (format nil "[0] Diff: HEAD..~A" ref-a))
                       (setf (panel-items (main-panel view))
                             (append (when stat
                                       (cl-ppcre:split "\\n" stat))
                                     (list "")
                                     (format-diff-lines diff)))
                       (setf (panel-selected (main-panel view)) 0)
                       (show-toast view (format nil "Diff: HEAD..~A" ref-a)))))
                  ((string= selected-button "Enter Ref")
                   ;; Open new dialog and skip the nil-set below
                   (setf (active-dialog view)
                         (make-dialog :title "Diff Compare Ref"
                                      :message (format nil "Compare ~A with ref:" ref-a)
                                      :input-mode t
                                      :data (list :ref-a ref-a)
                                      :buttons '("Compare" "Cancel")))
                   (return-from handle-key :dialog)))))
             ;; Diff Compare step 2 - user entered ref
             ((string= (dialog-title dlg) "Diff Compare Ref")
              (let ((ref-a (getf (dialog-data dlg) :ref-a))
                    (ref-b (first (dialog-input-lines dlg))))
                (when (and ref-a ref-b (> (length ref-b) 0))
                  (let* ((diff (git-diff-refs ref-a ref-b
                                              :context-size (diff-context-size view)
                                              :ignore-whitespace (diff-ignore-whitespace view)))
                         (stat (git-diff-refs-stat ref-a ref-b)))
                    (setf (diff-mode view) t)
                    (setf (diff-ref-a view) ref-a)
                    (setf (diff-ref-b view) ref-b)
                    (setf (panel-title (main-panel view))
                          (format nil "[0] Diff: ~A..~A" ref-a ref-b))
                    (setf (panel-items (main-panel view))
                          (append (when stat
                                    (cl-ppcre:split "\\n" stat))
                                  (list "")
                                  (format-diff-lines diff)))
                    (setf (panel-selected (main-panel view)) 0)
                    (show-toast view (format nil "Diff: ~A..~A" ref-a ref-b))))))
             ;; Filter Commits dialog
             ((string= (dialog-title dlg) "Filter Commits")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Filter")
                   (let ((query (first (dialog-input-lines dlg))))
                     (when (and query (> (length query) 0))
                       (setf (filter-query view) (string-downcase query))
                       (setf (filter-panel view) 3)
                       (show-toast view (format nil "Filtering: ~A" query))
                       (refresh-data view))))
                  ((string= selected-button "Search")
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
                               (setf (panel-items (commits-panel view))
                                     (loop for c in results
                                           collect (format-search-result c query)))
                               (setf (panel-title (commits-panel view))
                                     (format nil "[4] Search: ~A (~D results)"
                                             query (length results))))
                             (show-toast view (format nil "No commits matching '~A'" query)))))))
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
                  (refresh-data view))))
             ;; Discard Changes dialog
             ((string= (dialog-title dlg) "Discard Changes")
              (let ((file (getf (dialog-data dlg) :file)))
                (when file
                  (log-command view (format nil "git checkout -- ~A" file))
                  (git-discard-file file)
                  (show-toast view (format nil "Discarded ~A" file))
                  (refresh-data view))))
             ;; Move Commits to New Branch dialog
             ((string= (dialog-title dlg) "Move Commits to New Branch")
              (let ((branch-name (first (dialog-input-lines dlg)))
                    (count (getf (dialog-data dlg) :commit-count)))
                (when (and branch-name (> (length branch-name) 0) count)
                  (log-command view (format nil "Moving ~D commit~:P to ~A" count branch-name))
                  (let ((old-branch (git-move-commits-to-new-branch branch-name count)))
                    (show-toast view (format nil "Moved ~D commit~:P from ~A to ~A" 
                                             count old-branch branch-name)))
                  (refresh-data view))))
             ;; Apply Custom Patch dialog
             ((string= (dialog-title dlg) "Apply Custom Patch")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons)))
                (cond
                  ((string= selected-button "Apply to Index")
                   (let ((patch-text (build-accumulated-patch view)))
                     (if (and patch-text (> (length patch-text) 0))
                         (multiple-value-bind (ok msg) (apply-patch-to-index patch-text)
                           (if ok
                               (progn
                                 (log-command view "Custom patch applied to index")
                                 (show-toast view "Patch applied to index")
                                 ;; Exit patch builder
                                 (setf (patch-builder-mode view) nil)
                                 (setf (patch-builder-hash view) nil)
                                 (setf (patch-builder-files view) nil)
                                 (setf (patch-builder-selected view) nil)
                                 (setf (patch-builder-current-file view) nil)
                                 (setf (patch-builder-hunk-view view) nil)
                                 (setf (panel-focused (main-panel view)) nil)
                                 (refresh-data view))
                               (show-toast view msg)))
                         (show-toast view "No patch to apply"))))
                  ((string= selected-button "New Commit")
                   ;; Open a message input dialog
                   (setf (active-dialog view)
                         (make-dialog :title "Patch Commit Message"
                                      :message "Enter commit message for custom patch:"
                                      :input-mode t
                                      :data (list :patch-commit t)
                                      :buttons '("Commit" "Cancel")))
                   (return-from handle-key :dialog))
                  ((string= selected-button "Reverse")
                   (let ((patch-text (build-accumulated-patch view)))
                     (if (and patch-text (> (length patch-text) 0))
                         (multiple-value-bind (ok msg) (apply-reverse-patch-to-index patch-text)
                           (if ok
                               (progn
                                 (log-command view "Reverse patch applied to index")
                                 (show-toast view "Reverse patch applied")
                                 ;; Exit patch builder
                                 (setf (patch-builder-mode view) nil)
                                 (setf (patch-builder-hash view) nil)
                                 (setf (patch-builder-files view) nil)
                                 (setf (patch-builder-selected view) nil)
                                 (setf (patch-builder-current-file view) nil)
                                 (setf (patch-builder-hunk-view view) nil)
                                 (setf (panel-focused (main-panel view)) nil)
                                 (refresh-data view))
                               (show-toast view msg)))
                         (show-toast view "No patch to reverse")))))))
             ;; Patch Commit Message dialog (from patch builder)
             ((string= (dialog-title dlg) "Patch Commit Message")
              (let ((msg (first (dialog-input-lines dlg))))
                (when (and msg (> (length msg) 0))
                  (let ((patch-text (build-accumulated-patch view)))
                    (if (and patch-text (> (length patch-text) 0))
                        (multiple-value-bind (ok result-msg) (create-commit-from-patch patch-text msg)
                          (if ok
                              (progn
                                (log-command view (format nil "Created commit from custom patch: ~A" msg))
                                (show-toast view result-msg)
                                ;; Exit patch builder
                                (setf (patch-builder-mode view) nil)
                                (setf (patch-builder-hash view) nil)
                                (setf (patch-builder-files view) nil)
                                (setf (patch-builder-selected view) nil)
                                (setf (patch-builder-current-file view) nil)
                                (setf (patch-builder-hunk-view view) nil)
                                (setf (panel-focused (main-panel view)) nil)
                                (refresh-data view))
                              (show-toast view result-msg)))
                        (show-toast view "No patch to commit"))))))
             ;; Credential prompt dialog (from PTY runner)
             ((string= (dialog-title dlg) "Credential Required")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons))
                     (input (first (dialog-input-lines dlg))))
                (cond
                  ((string= selected-button "Send")
                   (when (and input (active-runner view))
                     ;; Send credential input followed by newline
                     (gilt.pty:runner-send (active-runner view)
                                           (format nil "~A~%" input))
                     (log-command view "Sent credential response")))
                  ((string= selected-button "Cancel")
                   (when (active-runner view)
                     (gilt.pty:runner-stop (active-runner view))
                     (show-toast view "Operation cancelled"))))))
             ;; Copy menu dialog
             ((string= (dialog-title dlg) "Copy")
              (let* ((buttons (dialog-buttons dlg))
                     (selected-idx (dialog-selected-button dlg))
                     (selected-button (nth selected-idx buttons))
                     (copy-panel (getf (dialog-data dlg) :copy-panel))
                     (text nil)
                     (what nil))
                (unless (string= selected-button "Cancel")
                  (cond
                    ;; Files panel
                    ((eq copy-panel :files)
                     (let ((file (getf (dialog-data dlg) :file)))
                       (cond
                         ((string= selected-button "File Path")
                          (let* ((repo (gilt.git:ensure-repo))
                                 (dir (namestring (gilt.git:repo-path-dir repo))))
                            (setf text (concatenate 'string dir file))
                            (setf what "file path")))
                         ((string= selected-button "File Name")
                          (setf text file)
                          (setf what "file name")))))
                    ;; Branches panel
                    ((eq copy-panel :branches)
                     (let ((branch (getf (dialog-data dlg) :branch)))
                       (when (string= selected-button "Branch Name")
                         (setf text branch)
                         (setf what "branch name"))))
                    ;; Commits panel
                    ((eq copy-panel :commits)
                     (let ((commit (getf (dialog-data dlg) :commit)))
                       (cond
                         ((string= selected-button "Hash")
                          (setf text (log-entry-short-hash commit))
                          (setf what "commit hash"))
                         ((string= selected-button "Message")
                          (setf text (log-entry-message commit))
                          (setf what "commit message"))
                         ((string= selected-button "Hash + Message")
                          (setf text (format nil "~A ~A" 
                                             (log-entry-short-hash commit)
                                             (log-entry-message commit)))
                          (setf what "commit info"))))))
                  (when text
                    (if (copy-to-clipboard text)
                        (progn
                          (log-command view (format nil "Copied ~A: ~A" what text))
                          (show-toast view (format nil "Copied ~A" what)))
                        (progn
                          (log-command view "Copy failed (no clipboard tool)")
                          (show-toast view "Copy failed (no clipboard tool)")))))))))
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

  ;; Handle commit-files mode - browse files changed in a commit
  (when (commit-files-mode view)
    (cond
      ;; Escape or q exits commit files mode
      ((or (eq (key-event-code key) +key-escape+)
           (and (key-event-char key) (char= (key-event-char key) #\q)))
       (setf (commit-files-mode view) nil)
       (setf (commit-files-hash view) nil)
       (setf (commit-files-list view) nil)
       (setf (panel-focused (main-panel view)) nil)
       (setf (panel-title (main-panel view)) "[0] Main")
       (update-main-content view)
       (return-from handle-key nil))
      ;; Navigation
      ((or (eq (key-event-code key) +key-down+)
           (and (key-event-char key) (char= (key-event-char key) #\j)))
       (let ((max-idx (1- (length (commit-files-list view)))))
         (when (< (commit-files-selected view) max-idx)
           (incf (commit-files-selected view))
           (panel-select-next (main-panel view))
           ;; Show diff for selected file
           (let* ((entry (nth (commit-files-selected view) (commit-files-list view)))
                  (file (cdr entry))
                  (diff (git-commit-diff (commit-files-hash view) file)))
             (when diff
               ;; We can't show inline diff while browsing files list,
               ;; so just update - the diff shows on Enter
               nil))))
       (return-from handle-key nil))
      ((or (eq (key-event-code key) +key-up+)
           (and (key-event-char key) (char= (key-event-char key) #\k)))
       (when (> (commit-files-selected view) 0)
         (decf (commit-files-selected view))
         (panel-select-prev (main-panel view)))
       (return-from handle-key nil))
      ;; Enter shows diff for selected file in a temporary view
      ((eq (key-event-code key) +key-enter+)
       (let* ((entry (nth (commit-files-selected view) (commit-files-list view)))
              (file (cdr entry))
              (diff (git-commit-diff (commit-files-hash view) file)))
         (when diff
           (setf (panel-title (main-panel view))
                 (format nil "[0] ~A (q to go back)" file))
           (setf (panel-items (main-panel view))
                 (format-diff-lines diff))
           (setf (panel-selected (main-panel view)) 0)))
       (return-from handle-key nil))
      ;; Copy file name with y
      ((and (key-event-char key) (char= (key-event-char key) #\y))
       (let* ((entry (nth (commit-files-selected view) (commit-files-list view)))
              (file (cdr entry)))
         (if (copy-to-clipboard file)
             (show-toast view "Copied file name")
             (show-toast view "Copy failed")))
       (return-from handle-key nil))
      ;; 'p' enters patch builder for this commit
      ((and (key-event-char key) (char= (key-event-char key) #\p))
       (let ((hash (commit-files-hash view))
             (files (commit-files-list view)))
         (when (and hash files)
           ;; Exit commit-files mode, enter patch builder
           (setf (commit-files-mode view) nil)
           (setf (patch-builder-mode view) t)
           (setf (patch-builder-hash view) hash)
           ;; Parse hunks for each file
           (setf (patch-builder-files view)
                 (loop for entry in files
                       for file = (cdr entry)
                       collect (cons file (parse-commit-hunks hash file))))
           ;; Initialize selection table: no hunks selected
           (setf (patch-builder-selected view) (make-hash-table :test 'equal))
           (setf (patch-builder-current-file view) nil)
           (setf (patch-builder-hunk-view view) nil)
           ;; Show file list with selection state
           (setf (panel-title (main-panel view))
                 (format nil "[0] Patch Builder (~A) - Space:toggle Enter:hunks P:apply Esc:quit"
                         (subseq hash 0 (min 7 (length hash)))))
           (setf (panel-items (main-panel view))
                 (loop for (file . hunks) in (patch-builder-files view)
                       for sel = (gethash file (patch-builder-selected view))
                       collect (let ((hunk-count (length hunks))
                                     (sel-count (if sel (length sel) 0)))
                                 (if (and sel (> sel-count 0))
                                     `(:multi-colored
                                       (:bright-green ,(format nil "● ~A" file))
                                       (:bright-black ,(format nil " (~D/~D hunks)" sel-count hunk-count)))
                                     `(:multi-colored
                                       (:white ,(format nil "○ ~A" file))
                                       (:bright-black ,(format nil " (~D hunks)" hunk-count)))))))
           (setf (panel-selected (main-panel view)) 0)
           (show-toast view "Patch builder: select files/hunks, then P to apply")))
       (return-from handle-key nil))))
  
  ;; Handle patch builder mode - select hunks from commit to build a custom patch
  (when (patch-builder-mode view)
    (cond
      ;; Escape exits patch builder (back to commit view or normal)
      ((eq (key-event-code key) +key-escape+)
       (if (patch-builder-hunk-view view)
           ;; Exit hunk view back to file list
           (progn
             (setf (patch-builder-hunk-view view) nil)
             (setf (patch-builder-current-file view) nil)
             (setf (panel-title (main-panel view))
                   (format nil "[0] Patch Builder (~A) - Space:toggle Enter:hunks P:apply Esc:quit"
                           (subseq (patch-builder-hash view) 0
                                   (min 7 (length (patch-builder-hash view))))))
             (setf (panel-items (main-panel view))
                   (loop for (file . hunks) in (patch-builder-files view)
                         for sel = (gethash file (patch-builder-selected view))
                         collect (let ((hunk-count (length hunks))
                                       (sel-count (if sel (length sel) 0)))
                                   (if (and sel (> sel-count 0))
                                       `(:multi-colored
                                         (:bright-green ,(format nil "● ~A" file))
                                         (:bright-black ,(format nil " (~D/~D hunks)" sel-count hunk-count)))
                                       `(:multi-colored
                                         (:white ,(format nil "○ ~A" file))
                                         (:bright-black ,(format nil " (~D hunks)" hunk-count)))))))
             (setf (panel-selected (main-panel view)) 0))
           ;; Exit patch builder entirely
           (progn
             (setf (patch-builder-mode view) nil)
             (setf (patch-builder-hash view) nil)
             (setf (patch-builder-files view) nil)
             (setf (patch-builder-selected view) nil)
             (setf (patch-builder-current-file view) nil)
             (setf (patch-builder-hunk-view view) nil)
             (setf (panel-focused (main-panel view)) nil)
             (setf (panel-title (main-panel view)) "[0] Main")
             (update-main-content view)))
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
      ;; Space toggles selection
      ((and (key-event-char key) (char= (key-event-char key) #\Space))
       (if (patch-builder-hunk-view view)
           ;; Toggle individual hunk selection
           (let* ((file (patch-builder-current-file view))
                  (hunk-idx (panel-selected (main-panel view)))
                  (sel (or (gethash file (patch-builder-selected view)) nil)))
             (if (member hunk-idx sel)
                 (setf (gethash file (patch-builder-selected view))
                       (remove hunk-idx sel))
                 (push hunk-idx (gethash file (patch-builder-selected view))))
             ;; Refresh hunk list display
             (let* ((hunks (cdr (assoc file (patch-builder-files view) :test #'string=)))
                    (new-sel (gethash file (patch-builder-selected view))))
               (setf (panel-items (main-panel view))
                     (loop for h in hunks
                           for i from 0
                           for selected = (member i new-sel)
                           collect (if selected
                                       `(:multi-colored
                                         (:bright-green ,(format nil "● Hunk ~D: " (1+ i)))
                                         (:cyan ,(hunk-header h))
                                         (:bright-black ,(format nil " (~D lines)" (hunk-line-count h))))
                                       `(:multi-colored
                                         (:white ,(format nil "○ Hunk ~D: " (1+ i)))
                                         (:cyan ,(hunk-header h))
                                         (:bright-black ,(format nil " (~D lines)" (hunk-line-count h)))))))))
           ;; Toggle all hunks in a file
           (let* ((idx (panel-selected (main-panel view)))
                  (file-entry (nth idx (patch-builder-files view))))
             (when file-entry
               (let* ((file (car file-entry))
                      (hunks (cdr file-entry))
                      (sel (gethash file (patch-builder-selected view))))
                 (if (and sel (= (length sel) (length hunks)))
                     ;; All selected - deselect all
                     (setf (gethash file (patch-builder-selected view)) nil)
                     ;; Select all hunks
                     (setf (gethash file (patch-builder-selected view))
                           (loop for i from 0 below (length hunks) collect i)))
                 ;; Refresh file list
                 (setf (panel-items (main-panel view))
                       (loop for (f . hs) in (patch-builder-files view)
                             for s = (gethash f (patch-builder-selected view))
                             collect (let ((hunk-count (length hs))
                                           (sel-count (if s (length s) 0)))
                                       (if (and s (> sel-count 0))
                                           `(:multi-colored
                                             (:bright-green ,(format nil "● ~A" f))
                                             (:bright-black ,(format nil " (~D/~D hunks)" sel-count hunk-count)))
                                           `(:multi-colored
                                             (:white ,(format nil "○ ~A" f))
                                             (:bright-black ,(format nil " (~D hunks)" hunk-count)))))))))))
       (return-from handle-key nil))
      ;; 'a' selects all files/hunks
      ((and (key-event-char key) (char= (key-event-char key) #\a))
       (if (patch-builder-hunk-view view)
           ;; Select all hunks in current file
           (let* ((file (patch-builder-current-file view))
                  (hunks (cdr (assoc file (patch-builder-files view) :test #'string=))))
             (setf (gethash file (patch-builder-selected view))
                   (loop for i from 0 below (length hunks) collect i))
             (let ((new-sel (gethash file (patch-builder-selected view))))
               (setf (panel-items (main-panel view))
                     (loop for h in hunks
                           for i from 0
                           for selected = (member i new-sel)
                           collect (if selected
                                       `(:multi-colored
                                         (:bright-green ,(format nil "● Hunk ~D: " (1+ i)))
                                         (:cyan ,(hunk-header h))
                                         (:bright-black ,(format nil " (~D lines)" (hunk-line-count h))))
                                       `(:multi-colored
                                         (:white ,(format nil "○ Hunk ~D: " (1+ i)))
                                         (:cyan ,(hunk-header h))
                                         (:bright-black ,(format nil " (~D lines)" (hunk-line-count h)))))))))
           ;; Select all files
           (progn
             (dolist (entry (patch-builder-files view))
               (let ((file (car entry))
                     (hunks (cdr entry)))
                 (setf (gethash file (patch-builder-selected view))
                       (loop for i from 0 below (length hunks) collect i))))
             (setf (panel-items (main-panel view))
                   (loop for (f . hs) in (patch-builder-files view)
                         for s = (gethash f (patch-builder-selected view))
                         collect (let ((hunk-count (length hs))
                                       (sel-count (if s (length s) 0)))
                                   `(:multi-colored
                                     (:bright-green ,(format nil "● ~A" f))
                                     (:bright-black ,(format nil " (~D/~D hunks)" sel-count hunk-count))))))))
       (show-toast view "Selected all")
       (return-from handle-key nil))
      ;; 'n' deselects all
      ((and (key-event-char key) (char= (key-event-char key) #\n))
       (if (patch-builder-hunk-view view)
           ;; Deselect all hunks in current file
           (let* ((file (patch-builder-current-file view))
                  (hunks (cdr (assoc file (patch-builder-files view) :test #'string=))))
             (setf (gethash file (patch-builder-selected view)) nil)
             (setf (panel-items (main-panel view))
                   (loop for h in hunks
                         for i from 0
                         collect `(:multi-colored
                                   (:white ,(format nil "○ Hunk ~D: " (1+ i)))
                                   (:cyan ,(hunk-header h))
                                   (:bright-black ,(format nil " (~D lines)" (hunk-line-count h)))))))
           ;; Deselect all files
           (progn
             (clrhash (patch-builder-selected view))
             (setf (panel-items (main-panel view))
                   (loop for (f . hs) in (patch-builder-files view)
                         collect `(:multi-colored
                                   (:white ,(format nil "○ ~A" f))
                                   (:bright-black ,(format nil " (~D hunks)" (length hs))))))))
       (show-toast view "Deselected all")
       (return-from handle-key nil))
      ;; Enter drills into hunk list for selected file
      ((eq (key-event-code key) +key-enter+)
       (unless (patch-builder-hunk-view view)
         (let* ((idx (panel-selected (main-panel view)))
                (file-entry (nth idx (patch-builder-files view))))
           (when file-entry
             (let* ((file (car file-entry))
                    (hunks (cdr file-entry))
                    (sel (gethash file (patch-builder-selected view))))
               (setf (patch-builder-current-file view) file)
               (setf (patch-builder-hunk-view view) t)
               (setf (panel-title (main-panel view))
                     (format nil "[0] Patch: ~A - Space:toggle a:all n:none Esc:back" file))
               (setf (panel-items (main-panel view))
                     (loop for h in hunks
                           for i from 0
                           for selected = (member i sel)
                           collect (if selected
                                       `(:multi-colored
                                         (:bright-green ,(format nil "● Hunk ~D: " (1+ i)))
                                         (:cyan ,(hunk-header h))
                                         (:bright-black ,(format nil " (~D lines)" (hunk-line-count h))))
                                       `(:multi-colored
                                         (:white ,(format nil "○ Hunk ~D: " (1+ i)))
                                         (:cyan ,(hunk-header h))
                                         (:bright-black ,(format nil " (~D lines)" (hunk-line-count h)))))))
               (setf (panel-selected (main-panel view)) 0)))))
       (return-from handle-key nil))
      ;; 'P' (capital) opens patch apply dialog
      ((and (key-event-char key) (char= (key-event-char key) #\P))
       ;; Count selected hunks
       (let ((total-selected 0)
             (total-files 0))
         (maphash (lambda (file indices)
                    (declare (ignore file))
                    (when indices
                      (incf total-files)
                      (incf total-selected (length indices))))
                  (patch-builder-selected view))
         (if (> total-selected 0)
             (setf (active-dialog view)
                   (make-dialog :title "Apply Custom Patch"
                                :message (format nil "~D hunk~:P from ~D file~:P selected.~%~%Apply patch from commit ~A?"
                                                 total-selected total-files
                                                 (subseq (patch-builder-hash view) 0
                                                         (min 7 (length (patch-builder-hash view)))))
                                :data (list :patch-action t)
                                :buttons '("Apply to Index" "New Commit" "Reverse" "Cancel")))
             (show-toast view "No hunks selected")))
       (return-from handle-key nil))
      ;; 'v' preview the accumulated patch
      ((and (key-event-char key) (char= (key-event-char key) #\v))
       (let ((patch-text (build-accumulated-patch view)))
         (if (and patch-text (> (length patch-text) 0))
             (progn
               (setf (panel-title (main-panel view))
                     "[0] Patch Preview (Esc to go back)")
               (setf (panel-items (main-panel view))
                     (format-diff-lines patch-text))
               (setf (panel-selected (main-panel view)) 0))
             (show-toast view "No hunks selected to preview")))
       (return-from handle-key nil))))
  
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
      ;; Enter opens line-level staging for selected hunk
      ((eq (key-event-code key) +key-enter+)
       (let* ((hunks (hunk-list view))
              (selected (panel-selected (main-panel view))))
         (when (and hunks (< selected (length hunks)))
           (let* ((hunk (nth selected hunks))
                  (body (rest (hunk-content hunk))))  ; skip @@ header
             (setf (hunk-mode view) nil)  ; exit hunk mode, enter line-select
             (setf (line-select-mode view) t)
             (setf (line-select-hunk view) hunk)
             (setf (line-selected-set view) nil)
             (setf (panel-selected (main-panel view)) 0)
             (setf (panel-title (main-panel view))
                   "[0] Line Select (Space=toggle, a=all, n=none, Enter=stage, Esc=back)")
             (setf (panel-items (main-panel view))
                   (loop for line in body
                         for idx from 0
                         for first-char = (if (> (length line) 0) (char line 0) #\Space)
                         for marker = (cond
                                        ((char= first-char #\Space) " ")
                                        (t "○"))
                         collect (format nil "~A ~A" marker line))))))
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

  ;; Handle line-select mode (line-level staging within a hunk)
  (when (line-select-mode view)
    (let* ((hunk (line-select-hunk view))
           (body (rest (hunk-content hunk)))
           (selected-set (line-selected-set view)))
      (flet ((refresh-line-display ()
               (setf (panel-items (main-panel view))
                     (loop for line in body
                           for idx from 0
                           for first-char = (if (> (length line) 0) (char line 0) #\Space)
                           for marker = (cond
                                          ((char= first-char #\Space) " ")  ; context - no marker
                                          ((member idx (line-selected-set view)) "●")
                                          (t "○"))
                           collect (format nil "~A ~A" marker line)))))
        (cond
          ;; Escape goes back to hunk list
          ((eq (key-event-code key) +key-escape+)
           (setf (line-select-mode view) nil)
           (setf (line-select-hunk view) nil)
           (setf (line-selected-set view) nil)
           (setf (hunk-mode view) t)  ; re-enter hunk mode
           ;; Restore hunk list display
           (setf (panel-selected (main-panel view)) 0)
           (setf (panel-title (main-panel view)) "[0] Hunks (Space=stage, Enter=lines, Esc=back)")
           (setf (panel-items (main-panel view))
                 (loop for h in (hunk-list view)
                       for i from 1
                       collect (format nil "Hunk ~D: ~A (+~D lines)"
                                       i (hunk-header h) (hunk-line-count h)))))
          ;; Space toggles current line selection (only for +/- lines)
          ((and (key-event-char key) (char= (key-event-char key) #\Space))
           (let* ((idx (panel-selected (main-panel view)))
                  (line (nth idx body))
                  (first-char (if (and line (> (length line) 0)) (char line 0) #\Space)))
             (when (or (char= first-char #\+) (char= first-char #\-))
               (if (member idx selected-set)
                   (setf (line-selected-set view) (remove idx selected-set))
                   (push idx (line-selected-set view)))))
           (refresh-line-display))
          ;; 'a' selects all +/- lines
          ((and (key-event-char key) (char= (key-event-char key) #\a))
           (setf (line-selected-set view)
                 (loop for line in body
                       for idx from 0
                       for fc = (if (> (length line) 0) (char line 0) #\Space)
                       when (or (char= fc #\+) (char= fc #\-))
                       collect idx))
           (refresh-line-display))
          ;; 'n' deselects all
          ((and (key-event-char key) (char= (key-event-char key) #\n))
           (setf (line-selected-set view) nil)
           (refresh-line-display))
          ;; Enter stages selected lines
          ((eq (key-event-code key) +key-enter+)
           (let ((indices (line-selected-set view)))
             (if indices
                 (progn
                   (log-command view (format nil "git apply --cached (~D lines)" (length indices)))
                   (multiple-value-bind (ok err)
                       (git-stage-lines hunk indices)
                     (if ok
                         (progn
                           ;; Success - exit line mode and refresh
                           (setf (line-select-mode view) nil)
                           (setf (hunk-mode view) nil)
                           (setf (hunk-list view) nil)
                           (refresh-data view))
                         (log-command view (format nil "Patch failed: ~A" err)))))
                 (log-command view "No lines selected"))))
          ;; Navigation
          ((or (eq (key-event-code key) +key-down+)
               (and (key-event-char key) (char= (key-event-char key) #\j)))
           (panel-select-next (main-panel view)))
          ((or (eq (key-event-code key) +key-up+)
               (and (key-event-char key) (char= (key-event-char key) #\k)))
           (panel-select-prev (main-panel view))))))
    (return-from handle-key nil))

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
  
  ;; Handle mouse click - map to panel focus and item selection
  (when (eq (key-event-code key) +key-mouse+)
    (let ((mx (key-event-ctrl-p key))   ; column (1-based)
          (my (key-event-alt-p key)))    ; row (1-based)
      ;; Find which panel was clicked
      (loop for p in (view-panels view)
            for i from 0
            when (and (> (panel-width p) 0) (> (panel-height p) 0)
                      (>= mx (panel-x p)) (< mx (+ (panel-x p) (panel-width p)))
                      (>= my (panel-y p)) (< my (+ (panel-y p) (panel-height p))))
            do (progn
                 ;; Focus this panel
                 (setf (view-focused-panel view) i)
                 (loop for pp in (view-panels view)
                       for j from 0
                       do (setf (panel-focused pp) (= j i)))
                 (setf (panel-focused (main-panel view)) nil)
                 ;; Select the clicked item (row within panel content area)
                 (let ((item-row (- my (panel-y p) 1)))  ; subtract top border
                   (when (and (>= item-row 0) (< item-row (length (panel-items p))))
                     (setf (panel-selected p) item-row)))
                 (update-main-content view)
                 (return)))
      ;; Check if main panel was clicked
      (let ((mp (main-panel view)))
        (when (and (> (panel-width mp) 0) (> (panel-height mp) 0)
                   (>= mx (panel-x mp)) (< mx (+ (panel-x mp) (panel-width mp)))
                   (>= my (panel-y mp)) (< my (+ (panel-y mp) (panel-height mp))))
          (setf (panel-focused mp) t)
          (loop for pp in (view-panels view)
                do (setf (panel-focused pp) nil))
          (let ((item-row (- my (panel-y mp) 1)))
            (when (and (>= item-row 0) (< item-row (length (panel-items mp))))
              (setf (panel-selected mp) item-row))))))
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
         ;; Commits panel - enter commit files view
         ((= focused-idx 3)
          (let* ((commits (commit-list view))
                 (selected (panel-selected (commits-panel view))))
            (when (and commits (< selected (length commits)))
              (let* ((commit (nth selected commits))
                     (hash (log-entry-hash commit))
                     (files (git-commit-files hash)))
                (when files
                  (setf (commit-files-mode view) t)
                  (setf (commit-files-hash view) hash)
                  (setf (commit-files-list view) files)
                  (setf (commit-files-selected view) 0)
                  ;; Show files in main panel
                  (setf (panel-focused (main-panel view)) t)
                  (setf (panel-title (main-panel view))
                        (format nil "[0] Commit ~A (~D files)"
                                (log-entry-short-hash commit) (length files)))
                  (setf (panel-items (main-panel view))
                        (loop for (status . file) in files
                              collect (list :multi-colored
                                           (list (case (char status 0)
                                                   (#\A :bright-green)
                                                   (#\D :bright-red)
                                                   (#\M :bright-yellow)
                                                   (#\R :bright-cyan)
                                                   (t :white))
                                                 (format nil "~A " status))
                                           (list :white file))))
                  (setf (panel-selected (main-panel view)) 0))))))
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
      ;; Screen mode cycling - '+' forward (global)
      ((and (key-event-char key) (char= (key-event-char key) #\+))
       (setf (screen-mode view)
             (case (screen-mode view)
               (:normal :half)
               (:half :full)
               (:full :normal)))
       (let ((mode-name (case (screen-mode view)
                          (:normal "normal") (:half "half") (:full "full"))))
         (log-command view (format nil "Screen mode: ~A" mode-name))
         (show-toast view (format nil "Screen: ~A" mode-name)))
       nil)
      ;; Screen mode cycling - '_' reverse (global)
      ((and (key-event-char key) (char= (key-event-char key) #\_))
       (setf (screen-mode view)
             (case (screen-mode view)
               (:normal :full)
               (:full :half)
               (:half :normal)))
       (let ((mode-name (case (screen-mode view)
                          (:normal "normal") (:half "half") (:full "full"))))
         (log-command view (format nil "Screen mode: ~A" mode-name))
         (show-toast view (format nil "Screen: ~A" mode-name)))
       nil)
      ;; Portrait mode toggle - '|' (global)
      ((and (key-event-char key) (char= (key-event-char key) #\|))
       (setf (portrait-mode view) (not (portrait-mode view)))
       (log-command view (format nil "Portrait mode: ~A" (if (portrait-mode view) "on" "off")))
       (show-toast view (format nil "Layout: ~A" (if (portrait-mode view) "portrait" "landscape")))
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
      ;; Copy menu - 'y' (context-sensitive yank with menu)
      ((and (key-event-char key) (char= (key-event-char key) #\y))
       (cond
         ;; Files panel - copy menu: file path, file name, diff
         ((= focused-idx 1)
          (let* ((entries (status-entries view))
                 (selected (panel-selected panel)))
            (when (and entries (< selected (length entries)))
              (let ((file (status-entry-file (nth selected entries))))
                (setf (active-dialog view)
                      (make-dialog :title "Copy"
                                   :message (format nil "Copy from file: ~A" file)
                                   :buttons '("File Path" "File Name" "Cancel")
                                   :data (list :copy-panel :files :file file)))))))
         ;; Branches panel - copy menu: branch name
         ((= focused-idx 2)
          (let* ((branches (branch-list view))
                 (selected (panel-selected panel)))
            (when (and branches (< selected (length branches)))
              (let ((branch (nth selected branches)))
                (setf (active-dialog view)
                      (make-dialog :title "Copy"
                                   :message (format nil "Copy from branch: ~A" branch)
                                   :buttons '("Branch Name" "Cancel")
                                   :data (list :copy-panel :branches :branch branch)))))))
         ;; Commits panel - copy menu: hash, short hash, message, full info
         ((= focused-idx 3)
          (let* ((commits (commit-list view))
                 (selected (panel-selected panel)))
            (when (and commits (< selected (length commits)))
              (let ((commit (nth selected commits)))
                (setf (active-dialog view)
                      (make-dialog :title "Copy"
                                   :message (format nil "Copy from commit: ~A" 
                                                    (log-entry-short-hash commit))
                                   :buttons '("Hash" "Message" "Hash + Message" "Cancel")
                                   :data (list :copy-panel :commits :commit commit))))))))
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
         (show-toast view (format nil "File view: ~A" (if (file-tree-mode view) "tree" "flat")))
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
       (show-toast view (format nil "Context: ~D lines" (diff-context-size view)))
       (update-main-content view)
       nil)
      ((and (key-event-char key) (char= (key-event-char key) #\{))
       (when (> (diff-context-size view) 0)
         (decf (diff-context-size view)))
       (log-command view (format nil "Diff context: ~D lines" (diff-context-size view)))
       (show-toast view (format nil "Context: ~D lines" (diff-context-size view)))
       (update-main-content view)
       nil)
      ;; Whitespace toggle - 'W' (capital, global)
      ((and (key-event-char key) (char= (key-event-char key) #\W))
       (setf (diff-ignore-whitespace view) (not (diff-ignore-whitespace view)))
       (log-command view (format nil "Whitespace: ~A"
                                  (if (diff-ignore-whitespace view) "ignored" "shown")))
       (show-toast view (format nil "Whitespace: ~A"
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
                 (setf (active-dialog view)
                       (make-dialog :title "Discard Changes"
                                    :message (format nil "Discard all changes to ~A? This cannot be undone."
                                                     (status-entry-file entry))
                                    :buttons '("Discard" "Cancel")
                                    :data (list :file (status-entry-file entry)))))))))
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
      ((and (key-event-char key) (char= (key-event-char key) #\A)
            (= focused-idx 1))
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
      ;; Diff compare - 'd' (when on branches panel, compare selected vs HEAD)
      ((and (key-event-char key) (char= (key-event-char key) #\d)
            (= focused-idx 2)
            (not (show-remote-branches view))
            (not (show-tags view))
            (not (show-submodules view)))
       (let* ((branches (branch-list view))
              (selected (panel-selected panel)))
         (when (and branches (< selected (length branches)))
           (let ((branch (nth selected branches)))
             (setf (active-dialog view)
                   (make-dialog :title "Diff Compare"
                                :message (format nil "Compare ~A with:" branch)
                                :buttons '("HEAD" "Enter Ref" "Cancel")
                                :data (list :ref-a branch))))))
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
      ;; Rebase onto - 'R' (capital, when on branches panel, local view only)
      ((and (key-event-char key) (char= (key-event-char key) #\R)
            (= focused-idx 2) (not (show-remote-branches view)))
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
      ((and (key-event-char key) (char= (key-event-char key) #\A)
            (= focused-idx 2))
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
                                  :message (format nil "Rename remote '~A' to (e.g. origin, upstream):" remote)
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
      ;; Stash file(s) - 'S' (capital, when on files panel)
      ((and (key-event-char key) (char= (key-event-char key) #\S)
            (= focused-idx 1))
       (when (= focused-idx 1)
         (let* ((entries (status-entries view))
                (selected (panel-selected panel)))
           (when (and entries (< selected (length entries)))
             (let ((files (if (range-select-start view)
                              ;; Range active - stash all files in range
                              (let* ((start (min (range-select-start view) selected))
                                     (end (max (range-select-start view) selected)))
                                (loop for i from start to end
                                      when (< i (length entries))
                                      collect (status-entry-file (nth i entries))))
                              ;; Single file
                              (list (status-entry-file (nth selected entries))))))
               (setf (active-dialog view)
                     (make-dialog :title "Stash File(s)"
                                  :message (if (= (length files) 1)
                                               (format nil "Stash '~A':" (first files))
                                               (format nil "Stash ~D files:" (length files)))
                                  :input-mode t
                                  :data (list :files files)
                                  :buttons '("Stash" "Cancel")))))))
       nil)
      ;; Squash commits - 'S' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\S)
            (= focused-idx 3))
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
      ;; Move commits to new branch - 'M' (capital, when on commits panel)
      ((and (key-event-char key) (char= (key-event-char key) #\M)
            (= focused-idx 3))
       (when (= focused-idx 3)  ; Commits panel
         (let ((selected (panel-selected panel)))
           (when (> selected 0)  ; Need at least 1 commit selected (past HEAD)
             (let ((count (1+ selected)))
               (setf (active-dialog view)
                     (make-dialog :title "Move Commits to New Branch"
                                  :message (format nil "Move last ~D commit~:P to a new branch:" count)
                                  :input-mode t
                                  :data (list :commit-count count)
                                  :buttons '("Move" "Cancel")))))))
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
      ;; Search/filter - '/' (context-sensitive, fuzzy matching)
      ((and (key-event-char key) (char= (key-event-char key) #\/))
       (cond
         ;; Commits panel - filter or search commits
         ((= focused-idx 3)
          (setf (active-dialog view)
                (make-dialog :title "Filter Commits"
                             :input-mode t
                             :buttons '("Filter" "Search" "Clear" "Cancel"))))
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
      ;; Graph toggle - 'g' (when on commits panel, NOT in bisect mode)
      ((and (key-event-char key) (char= (key-event-char key) #\g)
            (= focused-idx 3) (not (bisect-mode view)))
       (setf (graph-mode view) (not (graph-mode view)))
       (log-command view (format nil "Graph mode: ~A" (if (graph-mode view) "on" "off")))
       (update-main-content view)
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
                      (setf (panel-title (main-panel view))
                            "[0] Hunks (Space=stage, Enter=lines, Esc=back)")
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
                        (dolist (line (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Return) output)))
                          (log-command view line)))
                      (refresh-data view)
                      t))))))
       nil)
      (t nil))))

