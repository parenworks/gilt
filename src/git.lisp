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

(defgeneric repo-run (repo &rest args)
  (:documentation "Run a git command in the repository"))

(defgeneric repo-run-lines (repo &rest args)
  (:documentation "Run a git command and return output as lines"))

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

;;; Convenience functions using current directory

(defun git-run (&rest args)
  "Run a git command and return output as string"
  (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/git" args
                        :output s
                        :error nil
                        :search t)))

(defun git-run-lines (&rest args)
  "Run a git command and return output as list of lines"
  (let ((output (apply #'git-run args)))
    (cl-ppcre:split "\\n" (string-trim '(#\Newline) output))))

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
          (not (char= x #\?))))))

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

;;; Branches

(defun git-checkout (branch)
  "Checkout a branch"
  (git-run "checkout" branch))

(defun git-create-branch (name)
  "Create and checkout a new branch"
  (git-run "checkout" "-b" name))

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
