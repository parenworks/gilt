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

(defun git-log (&key (count 50))
  "Get recent commits"
  (let ((lines (git-run-lines "log" 
                              (format nil "-~D" count)
                              "--pretty=format:%H|%h|%an|%ar|%s")))
    (loop for line in lines
          for parts = (cl-ppcre:split "\\|" line :limit 5)
          when (= (length parts) 5)
            collect (make-log-entry
                     :hash (first parts)
                     :short-hash (second parts)
                     :author (third parts)
                     :date (fourth parts)
                     :message (fifth parts)))))

;;; Branches

(defun git-branches ()
  "Get list of branch names"
  (let ((lines (git-run-lines "branch" "--format=%(refname:short)")))
    lines))

(defun git-current-branch ()
  "Get current branch name"
  (string-trim '(#\Newline #\Space) 
               (git-run "rev-parse" "--abbrev-ref" "HEAD")))

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
                               (git-run "rev-parse" (format nil "HEAD~~D" count)))))
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

(defun git-fetch (&optional remote)
  "Fetch from remote"
  (if remote
      (git-run "fetch" remote)
      (git-run "fetch" "--all")))

(defun git-remotes ()
  "List remote names"
  (git-run-lines "remote"))

(defun git-remote-url (remote)
  "Get URL for a remote"
  (string-trim '(#\Newline #\Space)
               (git-run "remote" "get-url" remote)))

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
