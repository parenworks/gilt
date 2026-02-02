(in-package #:gilt.git)

;;; Git Command Interface - CLOS-based
;;; Wraps git CLI commands and parses output

;;; Git repository class

(defclass git-repository ()
  ((path :initarg :path :accessor repo-path :initform nil
         :documentation "Path to repository root")
   (name :initarg :name :accessor repo-name :initform nil
         :documentation "Repository name"))
  (:documentation "Represents a Git repository"))

(defmethod print-object ((repo git-repository) stream)
  (print-unreadable-object (repo stream :type t)
    (format stream "~A at ~A" (repo-name repo) (repo-path repo))))

(defgeneric repo-run (repo &rest args)
  (:documentation "Run a git command in the repository"))

(defgeneric repo-run-lines (repo &rest args)
  (:documentation "Run a git command and return output as lines"))

(defgeneric repo-status (repo)
  (:documentation "Get status entries for repository"))

(defgeneric repo-branches (repo)
  (:documentation "Get list of branches"))

(defgeneric repo-current-branch (repo)
  (:documentation "Get current branch name"))

(defgeneric repo-log (repo &key count)
  (:documentation "Get commit log"))

(defmethod repo-run ((repo git-repository) &rest args)
  (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/git" args
                        :output s
                        :error nil
                        :search t
                        :directory (repo-path repo))))

(defmethod repo-run-lines ((repo git-repository) &rest args)
  (let ((output (apply #'repo-run repo args)))
    (cl-ppcre:split "\\n" (string-trim '(#\Newline) output))))

(defmethod repo-current-branch ((repo git-repository))
  (string-trim '(#\Newline #\Space)
               (repo-run repo "rev-parse" "--abbrev-ref" "HEAD")))

(defmethod repo-branches ((repo git-repository))
  (repo-run-lines repo "branch" "--format=%(refname:short)"))

;;; Global current repository instance
(defparameter *current-repo* nil "The current git repository")

(defun ensure-repo ()
  "Ensure *current-repo* is initialized"
  (unless *current-repo*
    (let* ((root (string-trim '(#\Newline #\Space)
                              (with-output-to-string (s)
                                (sb-ext:run-program "/usr/bin/git" 
                                                    '("rev-parse" "--show-toplevel")
                                                    :output s :error nil :search t))))
           (name (car (last (cl-ppcre:split "/" root)))))
      (setf *current-repo* (make-instance 'git-repository :path root :name name))))
  *current-repo*)

;;; Convenience functions that delegate to *current-repo*

(defun git-run (&rest args)
  "Run a git command and return output as string"
  (apply #'repo-run (ensure-repo) args))

(defun git-run-lines (&rest args)
  "Run a git command and return output as list of lines"
  (apply #'repo-run-lines (ensure-repo) args))

;;; Status entry class

(defclass status-entry ()
  ((status :initarg :status :accessor status-entry-status :initform nil
           :documentation "Status type: :modified, :added, :deleted, :untracked, :renamed")
   (staged-p :initarg :staged-p :accessor status-entry-staged-p :initform nil
             :documentation "Whether change is staged")
   (file :initarg :file :accessor status-entry-file :initform nil
         :documentation "Filename"))
  (:documentation "Represents a file's status in git"))

(defmethod print-object ((entry status-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~A ~A~@[ staged~]"
            (status-entry-status entry)
            (status-entry-file entry)
            (status-entry-staged-p entry))))

(defun make-status-entry (&key status staged-p file)
  (make-instance 'status-entry :status status :staged-p staged-p :file file))

(defun parse-status-code (xy)
  "Parse git status --porcelain XY codes"
  (let ((x (char xy 0))
        (y (char xy 1)))
    (values
     ;; Status type (from working tree column Y, or index column X if staged)
     (cond
       ;; Conflict states - both modified, added by us/them, deleted by us/them
       ((or (string= xy "UU") (string= xy "AA") (string= xy "DD")
            (string= xy "AU") (string= xy "UA") (string= xy "DU") (string= xy "UD"))
        :conflict)
       ((char= y #\M) :modified)
       ((char= y #\A) :added)
       ((char= y #\D) :deleted)
       ((char= y #\?) :untracked)
       ((char= y #\Space)
        (cond
          ((char= x #\M) :modified)
          ((char= x #\A) :added)
          ((char= x #\D) :deleted)
          ((char= x #\R) :renamed)
          (t :unknown)))
       (t :unknown))
     ;; Staged?
     (and (not (char= x #\Space))
          (not (char= x #\?))
          (not (char= x #\U))))))

(defun git-status ()
  "Get list of status-entry for current repo"
  (let ((lines (git-run-lines "status" "--porcelain" "-u")))
    (loop for line in lines
          when (>= (length line) 3)
            collect (multiple-value-bind (status staged-p)
                        (parse-status-code (subseq line 0 2))
                      (make-status-entry
                       :status status
                       :staged-p staged-p
                       :file (subseq line 3))))))

(defun git-branch-tracking-info ()
  "Get tracking info for current branch. Returns (values upstream ahead behind) or nil if no upstream."
  (let ((lines (git-run-lines "status" "--porcelain=v2" "--branch")))
    (let ((upstream nil)
          (ahead 0)
          (behind 0))
      (dolist (line lines)
        (cond
          ((cl-ppcre:scan "^# branch\\.upstream " line)
           (setf upstream (subseq line 18)))
          ((cl-ppcre:scan "^# branch\\.ab " line)
           (multiple-value-bind (match regs)
               (cl-ppcre:scan-to-strings "\\+(\\d+) -(\\d+)" line)
             (declare (ignore match))
             (when regs
               (setf ahead (parse-integer (aref regs 0)))
               (setf behind (parse-integer (aref regs 1))))))))
      (when upstream
        (values upstream ahead behind)))))

(defun git-repo-state ()
  "Get current repository state (merge, rebase, etc). Returns keyword or nil."
  (let ((git-dir (string-trim '(#\Newline #\Space)
                              (git-run "rev-parse" "--git-dir"))))
    (cond
      ((probe-file (merge-pathnames "MERGE_HEAD" git-dir)) :merging)
      ((probe-file (merge-pathnames "rebase-merge" git-dir)) :rebasing)
      ((probe-file (merge-pathnames "rebase-apply" git-dir)) :rebasing)
      ((probe-file (merge-pathnames "CHERRY_PICK_HEAD" git-dir)) :cherry-picking)
      ((probe-file (merge-pathnames "REVERT_HEAD" git-dir)) :reverting)
      ((probe-file (merge-pathnames "BISECT_LOG" git-dir)) :bisecting)
      (t nil))))

;;; Diff

(defun git-diff (&optional file)
  "Get unstaged diff, optionally for specific file"
  (if file
      (git-run "diff" "--color=always" "--" file)
      (git-run "diff" "--color=always")))

(defun git-diff-staged (&optional file)
  "Get staged diff, optionally for specific file"
  (if file
      (git-run "diff" "--cached" "--color=always" "--" file)
      (git-run "diff" "--cached" "--color=always")))

;;; Hunk class for partial staging

(defclass diff-hunk ()
  ((file :initarg :file :accessor hunk-file :initform nil)
   (start-line :initarg :start-line :accessor hunk-start-line :initform 0)
   (line-count :initarg :line-count :accessor hunk-line-count :initform 0)
   (header :initarg :header :accessor hunk-header :initform nil)
   (content :initarg :content :accessor hunk-content :initform nil)
   (selected-p :initarg :selected-p :accessor hunk-selected-p :initform nil))
  (:documentation "Represents a diff hunk for partial staging"))

(defmethod print-object ((hunk diff-hunk) stream)
  (print-unreadable-object (hunk stream :type t)
    (format stream "~A:~D (~D lines)~@[ selected~]"
            (hunk-file hunk) (hunk-start-line hunk) 
            (hunk-line-count hunk) (hunk-selected-p hunk))))

(defun parse-diff-hunks (file)
  "Parse diff output into individual hunks for a file"
  (let* ((diff-output (git-run "diff" "-U3" "--" file))
         (lines (cl-ppcre:split "\\n" diff-output))
         (hunks nil)
         (current-hunk nil)
         (current-content nil))
    (dolist (line lines)
      (cond
        ;; Hunk header: @@ -start,count +start,count @@
        ((cl-ppcre:scan "^@@" line)
         ;; Save previous hunk if any
         (when current-hunk
           (setf (hunk-content current-hunk) (nreverse current-content))
           (push current-hunk hunks))
         ;; Parse new hunk header
         (multiple-value-bind (match regs)
             (cl-ppcre:scan-to-strings "^@@ -(\\d+)(?:,(\\d+))? \\+(\\d+)(?:,(\\d+))? @@" line)
           (declare (ignore match))
           (when regs
             (setf current-hunk (make-instance 'diff-hunk
                                               :file file
                                               :start-line (parse-integer (aref regs 2) :junk-allowed t)
                                               :line-count (if (aref regs 3)
                                                               (parse-integer (aref regs 3) :junk-allowed t)
                                                               1)
                                               :header line)
                   current-content (list line)))))
        ;; Content line (part of current hunk)
        (current-hunk
         (push line current-content))))
    ;; Save last hunk
    (when current-hunk
      (setf (hunk-content current-hunk) (nreverse current-content))
      (push current-hunk hunks))
    (nreverse hunks)))

(defun git-stage-hunk (hunk)
  "Stage a specific hunk using git apply"
  (let ((patch (format nil "~{~A~%~}" (hunk-content hunk))))
    ;; Create a proper patch with file headers
    (let ((full-patch (format nil "--- a/~A~%+++ b/~A~%~A"
                              (hunk-file hunk) (hunk-file hunk) patch)))
      (with-input-from-string (s full-patch)
        (sb-ext:run-program "/usr/bin/git" '("apply" "--cached" "-")
                            :input s :output nil :error nil)))))

;;; Log entry class

(defclass log-entry ()
  ((hash :initarg :hash :accessor log-entry-hash :initform nil)
   (short-hash :initarg :short-hash :accessor log-entry-short-hash :initform nil)
   (author :initarg :author :accessor log-entry-author :initform nil)
   (date :initarg :date :accessor log-entry-date :initform nil)
   (message :initarg :message :accessor log-entry-message :initform nil))
  (:documentation "Represents a git commit"))

(defmethod print-object ((entry log-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~A ~A" (log-entry-short-hash entry) (log-entry-message entry))))

(defun make-log-entry (&key hash short-hash author date message)
  (make-instance 'log-entry :hash hash :short-hash short-hash
                            :author author :date date :message message))

(defun git-log (&key (count 50) branch)
  "Get recent commits, optionally from a specific branch"
  (let ((lines (if branch
                   (git-run-lines "log" 
                                  (format nil "-~D" count)
                                  "--pretty=format:%H|%h|%an|%ar|%s"
                                  branch)
                   (git-run-lines "log" 
                                  (format nil "-~D" count)
                                  "--pretty=format:%H|%h|%an|%ar|%s"))))
    (loop for line in lines
          for parts = (cl-ppcre:split "\\|" line :limit 5)
          when (= (length parts) 5)
            collect (make-log-entry
                     :hash (first parts)
                     :short-hash (second parts)
                     :author (third parts)
                     :date (fourth parts)
                     :message (fifth parts)))))

(defun git-log-branch-only (branch &key (count 50))
  "Get commits that are in BRANCH but not in current branch (for cherry-picking)"
  (let* ((current (git-current-branch))
         (lines (git-run-lines "log" 
                               (format nil "-~D" count)
                               "--pretty=format:%H|%h|%an|%ar|%s"
                               (format nil "~A..~A" current branch))))
    (loop for line in lines
          for parts = (cl-ppcre:split "\\|" line :limit 5)
          when (= (length parts) 5)
            collect (make-log-entry
                     :hash (first parts)
                     :short-hash (second parts)
                     :author (third parts)
                     :date (fourth parts)
                     :message (fifth parts)))))

(defun git-log-search (query &key (count 100) author after before)
  "Search commits by message, author, or date range.
   QUERY: search term for commit message (can be nil)
   AUTHOR: filter by author name
   AFTER: commits after date (e.g. '2024-01-01')
   BEFORE: commits before date"
  (let ((args (list "log" (format nil "-~D" count) "--pretty=format:%H|%h|%an|%ar|%s")))
    (when query
      (setf args (append args (list "--regexp-ignore-case" (format nil "--grep=~A" query)))))
    (when author
      (setf args (append args (list (format nil "--author=~A" author)))))
    (when after
      (setf args (append args (list (format nil "--after=~A" after)))))
    (when before
      (setf args (append args (list (format nil "--before=~A" before)))))
    (let ((lines (apply #'git-run-lines args)))
      (loop for line in lines
            for parts = (cl-ppcre:split "\\|" line :limit 5)
            when (= (length parts) 5)
              collect (make-log-entry
                       :hash (first parts)
                       :short-hash (second parts)
                       :author (third parts)
                       :date (fourth parts)
                       :message (fifth parts))))))

(defun git-commit-message (hash)
  "Get the full commit message for a given commit hash"
  (git-run "log" "-1" "--format=%B" hash))

;;; Blame

(defclass blame-line ()
  ((hash :initarg :hash :accessor blame-line-hash)
   (short-hash :initarg :short-hash :accessor blame-line-short-hash)
   (author :initarg :author :accessor blame-line-author)
   (date :initarg :date :accessor blame-line-date)
   (line-num :initarg :line-num :accessor blame-line-num)
   (content :initarg :content :accessor blame-line-content))
  (:documentation "A single line from git blame output"))

(defun make-blame-line (&key hash short-hash author date line-num content)
  (make-instance 'blame-line
                 :hash hash
                 :short-hash short-hash
                 :author author
                 :date date
                 :line-num line-num
                 :content content))

(defun git-blame (file)
  "Get blame information for a file. Returns list of blame-line objects."
  (let ((lines (git-run-lines "blame" "--porcelain" file)))
    (when lines
      (let ((result nil)
            (current-hash nil)
            (current-author nil)
            (current-date nil)
            (line-num 0))
        ;; Parse porcelain format
        (dolist (line lines)
          (cond
            ;; Hash line (40 char hash followed by line numbers)
            ((and (>= (length line) 40)
                  (every (lambda (c) (or (digit-char-p c 16))) (subseq line 0 40)))
             (setf current-hash (subseq line 0 40))
             (incf line-num))
            ;; Author line
            ((and (> (length line) 7)
                  (string= (subseq line 0 7) "author "))
             (setf current-author (subseq line 7)))
            ;; Author time (Unix timestamp)
            ((and (> (length line) 12)
                  (string= (subseq line 0 12) "author-time "))
             (let ((timestamp (parse-integer (subseq line 12) :junk-allowed t)))
               (when timestamp
                 (setf current-date (format-relative-time timestamp)))))
            ;; Content line (starts with tab)
            ((and (> (length line) 0)
                  (char= (char line 0) #\Tab))
             (push (make-blame-line
                    :hash current-hash
                    :short-hash (if current-hash (subseq current-hash 0 (min 7 (length current-hash))) "")
                    :author (or current-author "")
                    :date (or current-date "")
                    :line-num line-num
                    :content (subseq line 1))
                   result))))
        (nreverse result)))))

(defun format-relative-time (unix-timestamp)
  "Format a Unix timestamp as relative time (e.g., '2 days ago')"
  (let* ((now (get-universal-time))
         ;; Unix epoch is 1970, CL universal time epoch is 1900
         (unix-epoch-offset 2208988800)
         (then (+ unix-timestamp unix-epoch-offset))
         (diff (- now then)))
    (cond
      ((< diff 60) "just now")
      ((< diff 3600) (format nil "~D mins ago" (floor diff 60)))
      ((< diff 86400) (format nil "~D hours ago" (floor diff 3600)))
      ((< diff 604800) (format nil "~D days ago" (floor diff 86400)))
      ((< diff 2592000) (format nil "~D weeks ago" (floor diff 604800)))
      ((< diff 31536000) (format nil "~D months ago" (floor diff 2592000)))
      (t (format nil "~D years ago" (floor diff 31536000))))))

;;; Tags

(defclass tag-entry ()
  ((name :initarg :name :accessor tag-name :initform nil)
   (type :initarg :type :accessor tag-type :initform :lightweight)  ; :lightweight or :annotated
   (date :initarg :date :accessor tag-date :initform nil)
   (message :initarg :message :accessor tag-message :initform nil))
  (:documentation "Represents a git tag"))

(defun make-tag-entry (&key name type date message)
  (make-instance 'tag-entry :name name :type type :date date :message message))

(defun git-tags ()
  "Get list of tag-entry objects, sorted by date (newest first)"
  (let ((lines (git-run-lines "tag" "-l" "--sort=-creatordate" 
                              "--format=%(refname:short)|%(objecttype)|%(creatordate:relative)|%(subject)")))
    (loop for line in lines
          when (> (length line) 0)
          collect (let ((parts (cl-ppcre:split "\\|" line :limit 4)))
                    (make-tag-entry
                     :name (first parts)
                     :type (if (string= (second parts) "tag") :annotated :lightweight)
                     :date (third parts)
                     :message (fourth parts))))))

(defun git-create-tag (name &optional message)
  "Create a tag. If MESSAGE is provided, create annotated tag."
  (if message
      (git-run "tag" "-a" name "-m" message)
      (git-run "tag" name)))

(defun git-delete-tag (name)
  "Delete a local tag"
  (git-run "tag" "-d" name))

(defun git-push-tag (name)
  "Push a tag to origin"
  (git-run "push" "origin" name))

(defun git-push-all-tags ()
  "Push all tags to origin"
  (git-run "push" "origin" "--tags"))

;;; Branches

(defun git-branches ()
  "Get list of branch names"
  (let ((lines (git-run-lines "branch" "--format=%(refname:short)")))
    lines))

(defun git-current-branch ()
  "Get current branch name"
  (string-trim '(#\Newline #\Space) 
               (git-run "rev-parse" "--abbrev-ref" "HEAD")))

(defun git-branch-has-upstream-p ()
  "Check if current branch has an upstream tracking branch"
  (let ((result (ignore-errors 
                  (git-run "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}"))))
    (and result (> (length (string-trim '(#\Newline #\Space) result)) 0))))

;;; Staging

(defun git-stage-file (file)
  "Stage a file"
  (git-run "add" "--" file))

(defun git-unstage-file (file)
  "Unstage a file"
  (git-run "reset" "HEAD" "--" file))

(defun git-stage-all ()
  "Stage all changes"
  (git-run "add" "-A"))

(defun git-discard-file (file)
  "Discard changes to a file"
  (git-run "checkout" "--" file))

;;; Commits

(defun git-commit (message)
  "Create a commit with message"
  (git-run "commit" "-m" message))

(defun git-amend ()
  "Amend the last commit"
  (git-run "commit" "--amend" "--no-edit"))

(defun git-squash-commits (count &optional message)
  "Squash the last COUNT commits into one. Uses soft reset + commit approach."
  (let ((head-ref (string-trim '(#\Newline #\Space) 
                               (git-run "rev-parse" (format nil "HEAD~~~D" count)))))
    (git-run "reset" "--soft" head-ref)
    (if message
        (git-run "commit" "-m" message)
        (git-run "commit" "--amend" "--no-edit"))))

(defun git-cherry-pick (commit-hash)
  "Cherry-pick a commit"
  (git-run "cherry-pick" commit-hash))

(defun git-revert (commit-hash)
  "Revert a commit"
  (git-run "revert" "--no-edit" commit-hash))

;;; Branches

(defun git-checkout (branch)
  "Checkout a branch"
  (git-run "checkout" branch))

(defun git-create-branch (name)
  "Create and checkout a new branch"
  (git-run "checkout" "-b" name))

(defun git-merge (branch)
  "Merge a branch into current branch"
  (git-run "merge" branch))

(defun git-merge-abort ()
  "Abort a merge in progress"
  (git-run "merge" "--abort"))

(defun git-merge-in-progress-p ()
  "Check if a merge is in progress"
  (let ((git-dir (string-trim '(#\Newline #\Space)
                              (git-run "rev-parse" "--git-dir"))))
    (probe-file (merge-pathnames "MERGE_HEAD" git-dir))))

(defun git-mark-resolved (file)
  "Mark a conflicted file as resolved by staging it"
  (git-run "add" file))

(defun git-edit-file (file)
  "Open a file in the user's preferred editor ($EDITOR or vi)"
  (let ((editor (or (uiop:getenv "EDITOR") "vi")))
    (uiop:run-program (list editor file)
                      :input :interactive
                      :output :interactive
                      :error-output :interactive)))

(defun git-resolve-with-ours (file)
  "Resolve conflict by keeping our version"
  (git-run "checkout" "--ours" file)
  (git-run "add" file))

(defun git-resolve-with-theirs (file)
  "Resolve conflict by keeping their version"
  (git-run "checkout" "--theirs" file)
  (git-run "add" file))

(defun git-delete-branch (branch)
  "Delete a branch"
  (git-run "branch" "-d" branch))

(defun git-fetch (&optional remote)
  "Fetch from remote (default: all remotes)"
  (if remote
      (git-run "fetch" remote)
      (git-run "fetch" "--all")))

(defun git-remote-branches ()
  "Get list of remote branches"
  (let ((output (git-run-lines "branch" "-r")))
    (loop for line in output
          for trimmed = (string-trim '(#\Space #\Tab) line)
          when (and (> (length trimmed) 0)
                    (not (search "->" trimmed)))  ; Skip HEAD pointer
          collect trimmed)))

(defun git-track-remote-branch (remote-branch)
  "Create a local branch tracking a remote branch"
  (let* ((parts (cl-ppcre:split "/" remote-branch :limit 2))
         (local-name (if (> (length parts) 1)
                         (second parts)
                         remote-branch)))
    (git-run "checkout" "-b" local-name "--track" remote-branch)))

(defun git-delete-remote-branch (remote-branch)
  "Delete a remote branch"
  (let* ((parts (cl-ppcre:split "/" remote-branch :limit 2))
         (remote (first parts))
         (branch (second parts)))
    (when (and remote branch)
      (git-run "push" remote "--delete" branch))))

;;; Stash

(defun git-stash ()
  "Stash current changes"
  (git-run "stash"))

(defun git-stash-pop ()
  "Pop the last stash"
  (git-run "stash" "pop"))

(defun git-stash-list ()
  "List stashes"
  (git-run-lines "stash" "list"))

;;; Push/Pull/Fetch

(defun git-push-interactive ()
  "Push to remote interactively (allows credential prompts)"
  (uiop:run-program '("git" "push")
                    :input :interactive
                    :output :interactive
                    :error-output :interactive))

(defun git-pull-interactive ()
  "Pull from remote interactively (allows credential prompts)"
  (uiop:run-program '("git" "pull")
                    :input :interactive
                    :output :interactive
                    :error-output :interactive))

(defun git-push (&optional remote branch)
  "Push to remote"
  (if (and remote branch)
      (git-run "push" remote branch)
      (git-run "push")))

(defun git-pull (&optional remote branch)
  "Pull from remote"
  (if (and remote branch)
      (git-run "pull" remote branch)
      (git-run "pull")))

(defun git-remotes ()
  "List remote names"
  (git-run-lines "remote"))

(defun git-remote-url (remote)
  "Get URL for a remote"
  (string-trim '(#\Newline #\Space)
               (git-run "remote" "get-url" remote)))

(defun git-remote-add (name url)
  "Add a new remote"
  (git-run "remote" "add" name url))

(defun git-remote-remove (name)
  "Remove a remote"
  (git-run "remote" "remove" name))

(defun git-remote-rename (old-name new-name)
  "Rename a remote"
  (git-run "remote" "rename" old-name new-name))

(defun git-remote-set-url (name url)
  "Set URL for a remote"
  (git-run "remote" "set-url" name url))

(defun git-remotes-with-urls ()
  "Get list of (name . url) pairs for all remotes"
  (let ((remotes (git-remotes)))
    (loop for remote in remotes
          collect (cons remote (git-remote-url remote)))))

;;; Submodules

(defclass submodule-entry ()
  ((name :initarg :name :accessor submodule-name :initform nil)
   (path :initarg :path :accessor submodule-path :initform nil)
   (url :initarg :url :accessor submodule-url :initform nil)
   (status :initarg :status :accessor submodule-status :initform :clean)  ; :clean, :modified, :uninitialized
   (commit :initarg :commit :accessor submodule-commit :initform nil))
  (:documentation "Represents a git submodule"))

(defun make-submodule-entry (&key name path url status commit)
  (make-instance 'submodule-entry :name name :path path :url url :status status :commit commit))

(defun git-submodules ()
  "Get list of submodule-entry objects with status"
  (let ((lines (git-run-lines "submodule" "status")))
    (loop for line in lines
          when (> (length line) 0)
          collect (let* ((status-char (char line 0))
                         (status (case status-char
                                   (#\- :uninitialized)
                                   (#\+ :modified)
                                   (#\U :merge-conflict)
                                   (t :clean)))
                         (rest (string-trim " " (subseq line 1)))
                         (parts (cl-ppcre:split "\\s+" rest :limit 2))
                         (commit (first parts))
                         (path (second parts)))
                    (make-submodule-entry
                     :name (car (last (cl-ppcre:split "/" path)))
                     :path path
                     :status status
                     :commit (subseq commit 0 (min 7 (length commit))))))))

(defun git-submodule-init ()
  "Initialize submodules"
  (git-run "submodule" "init"))

(defun git-submodule-update (&optional path)
  "Update submodules (optionally just one by path)"
  (if path
      (git-run "submodule" "update" "--init" path)
      (git-run "submodule" "update" "--init" "--recursive")))

(defun git-submodule-sync ()
  "Sync submodule URLs from .gitmodules"
  (git-run "submodule" "sync"))

(defun git-submodule-add (url &optional path)
  "Add a new submodule"
  (if path
      (git-run "submodule" "add" url path)
      (git-run "submodule" "add" url)))

(defun git-submodule-deinit (path)
  "Deinitialize a submodule"
  (git-run "submodule" "deinit" "-f" path))

;;; Repo info

(defun git-repo-root ()
  "Get repository root directory"
  (string-trim '(#\Newline #\Space)
               (git-run "rev-parse" "--show-toplevel")))

(defun git-repo-name ()
  "Get repository name from root directory"
  (let ((root (git-repo-root)))
    (car (last (cl-ppcre:split "/" root)))))

;;; Ahead/behind tracking

(defun git-ahead-behind ()
  "Get ahead/behind counts for current branch vs upstream.
   Returns (ahead . behind) or nil if no upstream."
  (let ((output (git-run "rev-list" "--left-right" "--count" "@{upstream}...HEAD")))
    (when (and output (> (length output) 0))
      (let ((parts (cl-ppcre:split "\\s+" (string-trim '(#\Newline #\Space #\Tab) output))))
        (when (= (length parts) 2)
          (cons (parse-integer (second parts) :junk-allowed t)
                (parse-integer (first parts) :junk-allowed t)))))))

;;; Git Config

(defclass config-entry ()
  ((key :initarg :key :accessor config-key :initform nil)
   (value :initarg :value :accessor config-value :initform nil)
   (scope :initarg :scope :accessor config-scope :initform :local))  ; :local, :global, :system
  (:documentation "Represents a git config entry"))

(defun make-config-entry (&key key value scope)
  (make-instance 'config-entry :key key :value value :scope scope))

(defun git-config-list (&optional scope)
  "Get list of config-entry objects. SCOPE can be :local, :global, :system, or nil for all."
  (let* ((args (case scope
                 (:local '("config" "--local" "--list"))
                 (:global '("config" "--global" "--list"))
                 (:system '("config" "--system" "--list"))
                 (t '("config" "--list" "--show-scope"))))
         (lines (apply #'git-run-lines args)))
    (loop for line in lines
          when (> (length line) 0)
          collect (if scope
                      ;; Simple key=value format
                      (let ((pos (position #\= line)))
                        (when pos
                          (make-config-entry
                           :key (subseq line 0 pos)
                           :value (subseq line (1+ pos))
                           :scope scope)))
                      ;; scope<tab>key=value format
                      (let* ((tab-pos (position #\Tab line))
                             (scope-str (when tab-pos (subseq line 0 tab-pos)))
                             (rest (when tab-pos (subseq line (1+ tab-pos))))
                             (eq-pos (when rest (position #\= rest))))
                        (when (and scope-str rest eq-pos)
                          (make-config-entry
                           :key (subseq rest 0 eq-pos)
                           :value (subseq rest (1+ eq-pos))
                           :scope (cond
                                    ((string= scope-str "local") :local)
                                    ((string= scope-str "global") :global)
                                    ((string= scope-str "system") :system)
                                    (t :unknown)))))))))

(defun git-config-get (key &optional scope)
  "Get a config value by key. Returns string or nil."
  (let ((args (case scope
                (:local (list "config" "--local" "--get" key))
                (:global (list "config" "--global" "--get" key))
                (:system (list "config" "--system" "--get" key))
                (t (list "config" "--get" key)))))
    (let ((output (apply #'git-run args)))
      (when (and output (> (length output) 0))
        (string-trim '(#\Newline #\Space) output)))))

(defun git-config-set (key value &optional scope)
  "Set a config value. SCOPE defaults to local."
  (let ((args (case scope
                (:global (list "config" "--global" key value))
                (:system (list "config" "--system" key value))
                (t (list "config" "--local" key value)))))
    (apply #'git-run args)))

(defun git-config-unset (key &optional scope)
  "Unset a config value."
  (let ((args (case scope
                (:local (list "config" "--local" "--unset" key))
                (:global (list "config" "--global" "--unset" key))
                (:system (list "config" "--system" "--unset" key))
                (t (list "config" "--unset" key)))))
    (apply #'git-run args)))

;;; Git Worktrees

(defclass worktree-entry ()
  ((path :initarg :path :accessor worktree-path :initform nil)
   (head :initarg :head :accessor worktree-head :initform nil)
   (branch :initarg :branch :accessor worktree-branch :initform nil)
   (bare :initarg :bare :accessor worktree-bare :initform nil)
   (detached :initarg :detached :accessor worktree-detached :initform nil)
   (locked :initarg :locked :accessor worktree-locked :initform nil)
   (prunable :initarg :prunable :accessor worktree-prunable :initform nil))
  (:documentation "Represents a git worktree"))

(defun make-worktree-entry (&key path head branch bare detached locked prunable)
  (make-instance 'worktree-entry :path path :head head :branch branch
                 :bare bare :detached detached :locked locked :prunable prunable))

(defun git-worktree-list ()
  "Get list of worktree-entry objects."
  (let ((lines (git-run-lines "worktree" "list" "--porcelain")))
    (let ((worktrees nil)
          (current nil))
      (dolist (line lines)
        (cond
          ((string= line "")
           (when current
             (push current worktrees)
             (setf current nil)))
          ((cl-ppcre:scan "^worktree " line)
           (setf current (make-worktree-entry :path (subseq line 9))))
          ((cl-ppcre:scan "^HEAD " line)
           (when current
             (setf (worktree-head current) (subseq line 5))))
          ((cl-ppcre:scan "^branch " line)
           (when current
             (let ((branch (subseq line 7)))
               (setf (worktree-branch current)
                     (if (cl-ppcre:scan "^refs/heads/" branch)
                         (subseq branch 11)
                         branch)))))
          ((string= line "bare")
           (when current (setf (worktree-bare current) t)))
          ((string= line "detached")
           (when current (setf (worktree-detached current) t)))
          ((cl-ppcre:scan "^locked" line)
           (when current (setf (worktree-locked current) t)))
          ((cl-ppcre:scan "^prunable" line)
           (when current (setf (worktree-prunable current) t)))))
      (when current
        (push current worktrees))
      (nreverse worktrees))))

(defun git-worktree-add (path &optional branch)
  "Add a new worktree at PATH for BRANCH. If BRANCH is nil, creates detached HEAD."
  (if branch
      (git-run "worktree" "add" path branch)
      (git-run "worktree" "add" "--detach" path)))

(defun git-worktree-add-new-branch (path new-branch &optional start-point)
  "Add a new worktree at PATH with a new branch NEW-BRANCH."
  (if start-point
      (git-run "worktree" "add" "-b" new-branch path start-point)
      (git-run "worktree" "add" "-b" new-branch path)))

(defun git-worktree-remove (path &optional force)
  "Remove a worktree at PATH."
  (if force
      (git-run "worktree" "remove" "--force" path)
      (git-run "worktree" "remove" path)))

(defun git-worktree-lock (path &optional reason)
  "Lock a worktree to prevent pruning."
  (if reason
      (git-run "worktree" "lock" "--reason" reason path)
      (git-run "worktree" "lock" path)))

(defun git-worktree-unlock (path)
  "Unlock a worktree."
  (git-run "worktree" "unlock" path))

(defun git-worktree-prune ()
  "Prune stale worktree information."
  (git-run "worktree" "prune"))

;;; Stash management

(defclass stash-entry ()
  ((index :initarg :index :accessor stash-index :initform 0)
   (ref :initarg :ref :accessor stash-ref :initform nil)
   (branch :initarg :branch :accessor stash-branch :initform nil)
   (message :initarg :message :accessor stash-message :initform nil))
  (:documentation "Represents a git stash entry"))

(defun make-stash-entry (&key index ref branch message)
  (make-instance 'stash-entry :index index :ref ref :branch branch :message message))

(defun git-stash-list ()
  "Get list of stash-entry objects."
  (let ((lines (git-run-lines "stash" "list" "--format=%gd|%gs")))
    (loop for line in lines
          for i from 0
          when (> (length line) 0)
          collect (let* ((parts (cl-ppcre:split "\\|" line :limit 2))
                         (ref (first parts))
                         (desc (second parts)))
                    ;; desc is like "WIP on master: abc123 message" or "On master: message"
                    (let ((branch nil)
                          (message desc))
                      (when (cl-ppcre:scan "^(?:WIP )?[Oo]n ([^:]+):" desc)
                        (cl-ppcre:register-groups-bind (b) ("^(?:WIP )?[Oo]n ([^:]+):" desc)
                          (setf branch b))
                        (setf message (cl-ppcre:regex-replace "^(?:WIP )?[Oo]n [^:]+:\\s*" desc "")))
                      (make-stash-entry :index i :ref ref :branch branch :message message))))))

(defun git-stash-push (&optional message)
  "Stash current changes with optional message."
  (if message
      (git-run "stash" "push" "-m" message)
      (git-run "stash" "push")))

(defun git-stash-push-staged (&optional message)
  "Stash only staged changes."
  (if message
      (git-run "stash" "push" "--staged" "-m" message)
      (git-run "stash" "push" "--staged")))

(defun git-stash-push-include-untracked (&optional message)
  "Stash changes including untracked files."
  (if message
      (git-run "stash" "push" "--include-untracked" "-m" message)
      (git-run "stash" "push" "--include-untracked")))

(defun git-stash-pop (&optional index)
  "Pop stash at INDEX (default 0)."
  (if index
      (git-run "stash" "pop" (format nil "stash@{~D}" index))
      (git-run "stash" "pop")))

(defun git-stash-apply (&optional index)
  "Apply stash at INDEX without removing it."
  (if index
      (git-run "stash" "apply" (format nil "stash@{~D}" index))
      (git-run "stash" "apply")))

(defun git-stash-drop (&optional index)
  "Drop stash at INDEX."
  (if index
      (git-run "stash" "drop" (format nil "stash@{~D}" index))
      (git-run "stash" "drop")))

(defun git-stash-clear ()
  "Clear all stashes."
  (git-run "stash" "clear"))

(defun git-stash-show (index)
  "Get diff for stash at INDEX."
  (git-run "stash" "show" "-p" (format nil "stash@{~D}" index)))

(defun git-stash-branch (branch-name &optional index)
  "Create a branch from stash at INDEX."
  (if index
      (git-run "stash" "branch" branch-name (format nil "stash@{~D}" index))
      (git-run "stash" "branch" branch-name)))
