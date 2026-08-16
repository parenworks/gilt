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

(defun git-toplevel ()
  "Get the git repository root, or nil if not in a repo."
  (let ((root (string-trim '(#\Newline #\Space)
                            (with-output-to-string (s)
                              (sb-ext:run-program "/usr/bin/git" 
                                                  '("rev-parse" "--show-toplevel")
                                                  :output s :error nil :search t)))))
    (if (or (null root) (string= root "")) nil root)))

(defun git-init ()
  "Initialize a new git repository in the current directory."
  (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/git" '("init")
                        :output s :error nil :search t)))

(defun git-clone (url &optional destination)
  "Clone a repository from URL into DESTINATION (optional).
   Returns the destination path on success."
  (let* ((dest (or destination
                   (let ((last-slash (position #\/ url :from-end t)))
                     (if last-slash
                         (let ((name (subseq url (1+ last-slash))))
                           (if (and (> (length name) 4)
                                    (string= (subseq name (- (length name) 4)) ".git"))
                               (subseq name 0 (- (length name) 4))
                               name))
                         url))))
         (cmd (if destination
                  (list "git" "clone" url destination)
                  (list "git" "clone" url))))
    (let ((output (with-output-to-string (s)
                    (sb-ext:run-program "/usr/bin/git" (cdr cmd)
                                        :output s :error s :search t))))
      (declare (ignore output))
      dest)))

(defun ensure-repo ()
  "Ensure *current-repo* is initialized. If not in a git repo, offer
   options to init, clone, or browse recent repos."
  (unless *current-repo*
    (let ((root (git-toplevel)))
      (unless root
        (format t "Not a git repository.~%")
        (format t "  [1] Initialize a new repository here~%")
        (format t "  [2] Clone a repository~%")
        (format t "  [3] Browse recent repositories~%")
        (format t "  [q] Quit~%")
        (format t "Choose an option: ")
        (finish-output)
        (let ((response (string-trim '(#\Space #\Newline #\Return)
                                      (read-line *standard-input* nil ""))))
          (cond
            ((or (string= response "1") (string-equal response "i")
                 (string-equal response "init"))
             (git-init)
             (setf root (git-toplevel))
             (unless root
               (format t "Failed to initialize git repository~%")
               (sb-ext:exit :code 1)))
            ((or (string= response "2") (string-equal response "c")
                 (string-equal response "clone"))
             (format t "Enter repository URL: ")
             (finish-output)
             (let ((url (string-trim '(#\Space #\Newline #\Return)
                                      (read-line *standard-input* nil ""))))
               (when (and url (> (length url) 0))
                 (format t "Cloning ~A...~%" url)
                 (let ((dest (git-clone url)))
                   (when dest
                     (setf root (namestring (truename dest))))))))
            ((or (string= response "3") (string-equal response "r")
                 (string-equal response "recent"))
             (let ((recents (load-recent-repos)))
               (if recents
                   (progn
                     (format t "~&Recent repositories:~%")
                     (loop for path in recents for i from 1
                           do (format t "~&  [~D] ~A~%" i path))
                     (format t "~&Choose number: ")
                     (finish-output)
                     (let ((choice (parse-integer (string-trim '(#\Space #\Newline #\Return)
                                                                (read-line *standard-input* nil "0"))
                                                   :junk-allowed t)))
                       (when (and choice (> choice 0) (<= choice (length recents)))
                         (let ((path (nth (1- choice) recents)))
                           (when (probe-file path)
                             (setf root path))))))
                   (format t "~&No recent repositories found.~%")))
             (unless root (sb-ext:exit :code 0)))
            (t (sb-ext:exit :code 0)))))
      (when root
        (let ((name (car (last (cl-ppcre:split "/" root)))))
          (setf *current-repo* (make-instance 'git-repository :path root :name name))
          (save-recent-repo root)))))
  *current-repo*)

(defvar *parent-repo-stack* nil "Stack of parent repos for submodule navigation")

(defparameter *rename-threshold* nil
  "Git rename detection threshold (0-100). nil means use git default.")

(defun rename-threshold-arg ()
  "Return the -M<threshold> argument string if threshold is set, or nil."
  (when *rename-threshold*
    (format nil "-M~D%" *rename-threshold*)))

(defun enter-submodule (submodule-path)
  "Enter a submodule by changing *current-repo* to point to it. Pushes current repo onto stack."
  (let* ((repo (ensure-repo))
         (full-path (namestring (merge-pathnames submodule-path (repo-path-dir repo))))
         (name (car (last (cl-ppcre:split "/" (string-right-trim "/" full-path))))))
    (push repo *parent-repo-stack*)
    (setf *current-repo* (make-instance 'git-repository :path full-path :name name))))

(defun leave-submodule ()
  "Leave current submodule and return to parent repo. Returns t if successful."
  (when *parent-repo-stack*
    (setf *current-repo* (pop *parent-repo-stack*))
    t))

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

(defun git-diff-numstat (&key (staged nil))
  "Get numstat for changed files. Returns alist of (file . (added . removed)).
   If STAGED is true, shows staged changes only."
  (let ((lines (if staged
                   (git-run-lines "diff" "--cached" "--numstat")
                   (git-run-lines "diff" "--numstat"))))
    (loop for line in lines
          when (and line (> (length line) 0))
          collect (let ((parts (cl-ppcre:split "\\s+" line :limit 3)))
                    (when (>= (length parts) 3)
                      (let ((added (let ((a (first parts)))
                                     (if (string= a "-") nil (parse-integer a :junk-allowed t))))
                            (removed (let ((r (second parts)))
                                       (if (string= r "-") nil (parse-integer r :junk-allowed t))))
                            (file (third parts)))
                        (cons file (cons added removed))))))))

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

(defun git-diff-split (&key (file nil) (context-size 3) ignore-whitespace)
  "Get side-by-side diff. Returns raw text with left/right columns."
  (let ((args (list "diff" "--color=always" "--side-by-side"
                    (format nil "-U~D" context-size)
                    "--width=200")))
    (when ignore-whitespace (push "-w" (cdr (last args))))
    (when *rename-threshold* (push (rename-threshold-arg) (cdr (last args))))
    (if file
        (apply #'git-run (append args (list "--" file)))
        (apply #'git-run args))))

(defun git-diff-staged-split (&key (file nil) (context-size 3) ignore-whitespace)
  "Get side-by-side staged diff. Returns raw text with left/right columns."
  (let ((args (list "diff" "--cached" "--color=always" "--side-by-side"
                    (format nil "-U~D" context-size)
                    "--width=200")))
    (when ignore-whitespace (push "-w" (cdr (last args))))
    (when *rename-threshold* (push (rename-threshold-arg) (cdr (last args))))
    (if file
        (apply #'git-run (append args (list "--" file)))
        (apply #'git-run args))))

(defun git-diff (&key (file nil) (context-size 3) ignore-whitespace)
  "Get unstaged diff, optionally for specific file"
  (let ((args (list "diff" "--color=always"
                    (format nil "-U~D" context-size))))
    (when ignore-whitespace (push "-w" (cdr (last args))))
    (when *rename-threshold* (push (rename-threshold-arg) (cdr (last args))))
    (if file
        (apply #'git-run (append args (list "--" file)))
        (apply #'git-run args))))

(defun git-diff-staged (&key (file nil) (context-size 3) ignore-whitespace)
  "Get staged diff, optionally for specific file"
  (let ((args (list "diff" "--cached" "--color=always"
                    (format nil "-U~D" context-size))))
    (when ignore-whitespace (push "-w" (cdr (last args))))
    (when *rename-threshold* (push (rename-threshold-arg) (cdr (last args))))
    (if file
        (apply #'git-run (append args (list "--" file)))
        (apply #'git-run args))))

(defun git-diff-refs (ref-a ref-b &key (context-size 3) ignore-whitespace)
  "Get diff between two refs (branches, tags, commits)."
  (let ((args (list "diff" "--color=always"
                    (format nil "-U~D" context-size)
                    ref-a ref-b)))
    (when ignore-whitespace (push "-w" (cdr (last args))))
    (when *rename-threshold* (push (rename-threshold-arg) (cdr (last args))))
    (apply #'git-run args)))

(defun git-diff-refs-stat (ref-a ref-b)
  "Get diffstat summary between two refs."
  (git-run "diff" "--stat" ref-a ref-b))

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

(defun build-partial-patch (hunk selected-indices)
  "Build a valid patch from HUNK containing only lines at SELECTED-INDICES.
   SELECTED-INDICES are 0-based indices into the hunk content (excluding header).
   Context lines are always included. Unselected + lines become context.
   Unselected - lines are dropped (kept in working copy)."
  (let* ((content (hunk-content hunk))
         (header (first content))
         (body (rest content))
         (new-body nil)
         (old-count 0)
         (new-count 0))
    ;; Process each line in the hunk body
    (loop for line in body
          for idx from 0
          for first-char = (if (> (length line) 0) (char line 0) #\Space)
          for selected = (member idx selected-indices)
          do (cond
               ;; Context line - always include
               ((char= first-char #\Space)
                (push line new-body)
                (incf old-count)
                (incf new-count))
               ;; Added line (+) - include if selected, otherwise drop
               ((char= first-char #\+)
                (if selected
                    (progn (push line new-body) (incf new-count))
                    nil))  ; drop unselected additions
               ;; Removed line (-) - include if selected, otherwise convert to context
               ((char= first-char #\-)
                (if selected
                    (progn (push line new-body) (incf old-count))
                    (progn
                      ;; Convert to context line (keep in both old and new)
                      (push (concatenate 'string " " (subseq line 1)) new-body)
                      (incf old-count)
                      (incf new-count))))
               ;; Other lines (e.g., "\ No newline at end of file")
               (t (push line new-body))))
    ;; Build new header with corrected counts
    (let ((new-header (cl-ppcre:regex-replace
                       "@@ -(\\d+)(?:,\\d+)? \\+(\\d+)(?:,\\d+)? @@"
                       header
                       (format nil "@@ -\\1,~D +\\2,~D @@" old-count new-count))))
      (cons new-header (nreverse new-body)))))

(defun git-stage-lines (hunk selected-indices)
  "Stage specific lines from a hunk. SELECTED-INDICES are 0-based indices
   into the hunk body (excluding the @@ header line)."
  (let* ((patch-lines (build-partial-patch hunk selected-indices))
         (patch (format nil "~{~A~%~}" patch-lines))
         (full-patch (format nil "--- a/~A~%+++ b/~A~%~A"
                              (hunk-file hunk) (hunk-file hunk) patch)))
    (with-input-from-string (s full-patch)
      (let* ((proc (sb-ext:run-program "/usr/bin/git" '("apply" "--cached" "-")
                                       :input s :output nil :error :stream))
             (err-output (with-output-to-string (out)
                           (loop for line = (read-line (sb-ext:process-error proc) nil nil)
                                 while line do (write-line line out))))
             (exit-code (sb-ext:process-exit-code proc)))
        (if (= exit-code 0)
            t
            (values nil (if (> (length err-output) 0)
                            err-output
                            (format nil "exit code ~D" exit-code))))))))

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

(defun git-log-graph (&key (count 50) all)
  "Get commit graph as list of strings. If ALL is true, show all branches.
   Uses --topo-order for cleaner graph layout (no intermixed branch lines)."
  (if all
      (git-run-lines "log" (format nil "-~D" count)
                     "--graph" "--oneline" "--decorate"
                     "--color=always" "--all" "--topo-order")
      (git-run-lines "log" (format nil "-~D" count)
                     "--graph" "--oneline" "--decorate"
                     "--color=always" "--topo-order")))

(defun git-log-all (&key (count 50))
  "Get commits from all branches in topo order.
   Returns list of log-entry objects."
  (let ((lines (git-run-lines "log"
                              (format nil "-~D" count)
                              "--all" "--topo-order"
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

(defun git-commit-files (hash)
  "Get list of files changed in a commit. Returns list of (status . filename) pairs."
  (let ((lines (git-run-lines "diff-tree" "--no-commit-id" "-r" "--name-status" hash)))
    (loop for line in lines
          when (> (length line) 2)
          collect (cons (subseq line 0 1) (string-trim '(#\Tab #\Space) (subseq line 1))))))

(defun git-commit-diff (hash &optional file)
  "Get the diff for a specific commit, optionally for a single file."
  (if file
      (git-run "diff" (format nil "~A^" hash) hash "--color=always" "--" file)
      (git-run "diff" (format nil "~A^" hash) hash "--color=always")))

;;; Custom Patch Builder

(defun parse-commit-hunks (hash file)
  "Parse diff output from a commit into individual hunks for a file.
   Unlike parse-diff-hunks which works on the working tree, this parses
   the diff between hash^ and hash for the given file."
  (let* ((diff-output (git-run "diff" "-U3" (format nil "~A^" hash) hash "--" file))
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

(defun build-full-patch (file hunks)
  "Build a complete patch from selected HUNKS for FILE.
   Returns a string suitable for git apply."
  (when hunks
    (with-output-to-string (out)
      (format out "--- a/~A~%" file)
      (format out "+++ b/~A~%" file)
      (dolist (hunk hunks)
        (format out "~{~A~%~}" (hunk-content hunk))))))

(defun build-reverse-patch-from-hunk (hunk)
  "Reverse a hunk: swap + and - lines, swap old/new counts in header."
  (let* ((content (hunk-content hunk))
         (header (first content))
         (body (rest content))
         (reversed-body
           (loop for line in body
                 collect (cond
                           ((and (> (length line) 0) (char= (char line 0) #\+))
                            (concatenate 'string "-" (subseq line 1)))
                           ((and (> (length line) 0) (char= (char line 0) #\-))
                            (concatenate 'string "+" (subseq line 1)))
                           (t line))))
         ;; Swap the old and new in the header
         (reversed-header
           (cl-ppcre:regex-replace
             "@@ -(\\d+(?:,\\d+)?) \\+(\\d+(?:,\\d+)?) @@(.*)"
             header
             "@@ -\\2 +\\1 @@\\3")))
    (cons reversed-header reversed-body)))

(defun apply-patch-to-index (patch-text)
  "Apply a patch string to the staging area (index).
   Returns (values success-p message)."
  (let* ((repo (ensure-repo))
         (dir (repo-path-dir repo)))
    (let ((proc (sb-ext:run-program "git" (list "apply" "--cached" "-")
                                    :input :stream
                                    :output :stream
                                    :error :stream
                                    :directory dir
                                    :search t
                                    :wait nil)))
      (write-string patch-text (sb-ext:process-input proc))
      (close (sb-ext:process-input proc))
      (sb-ext:process-wait proc)
      (let ((exit (sb-ext:process-exit-code proc))
            (err (let ((s (make-string-output-stream)))
                   (loop for c = (read-char (sb-ext:process-error proc) nil nil)
                         while c do (write-char c s))
                   (get-output-stream-string s))))
        (if (zerop exit)
            (values t "Patch applied to index")
            (values nil (format nil "Patch failed: ~A" (string-trim '(#\Newline) err))))))))

(defun apply-reverse-patch-to-index (patch-text)
  "Apply a reversed patch to the staging area (undo changes).
   Returns (values success-p message)."
  (let* ((repo (ensure-repo))
         (dir (repo-path-dir repo)))
    (let ((proc (sb-ext:run-program "git" (list "apply" "--cached" "--reverse" "-")
                                    :input :stream
                                    :output :stream
                                    :error :stream
                                    :directory dir
                                    :search t
                                    :wait nil)))
      (write-string patch-text (sb-ext:process-input proc))
      (close (sb-ext:process-input proc))
      (sb-ext:process-wait proc)
      (let ((exit (sb-ext:process-exit-code proc))
            (err (let ((s (make-string-output-stream)))
                   (loop for c = (read-char (sb-ext:process-error proc) nil nil)
                         while c do (write-char c s))
                   (get-output-stream-string s))))
        (if (zerop exit)
            (values t "Reverse patch applied")
            (values nil (format nil "Reverse patch failed: ~A" (string-trim '(#\Newline) err))))))))

(defun create-commit-from-patch (patch-text message)
  "Apply a patch to the index and create a new commit with MESSAGE.
   Returns (values success-p result-message)."
  (multiple-value-bind (ok err) (apply-patch-to-index patch-text)
    (if ok
        (let* ((repo (ensure-repo))
               (dir (repo-path-dir repo)))
          (let ((proc (sb-ext:run-program "git" (list "commit" "-m" message)
                                          :output :stream
                                          :error :stream
                                          :directory dir
                                          :search t
                                          :wait t)))
            (let ((exit (sb-ext:process-exit-code proc)))
              (if (zerop exit)
                  (values t (format nil "Created commit: ~A" message))
                  (values nil "Commit failed after applying patch")))))
        (values nil err))))

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

(defun git-blame-at (file &key ref (detect-copies nil))
  "Get blame information for a file at a specific ref.
   DETECT-COPIES enables -C flag to detect code moved from other files.
   Returns list of blame-line objects."
  (let ((args (append (list "blame" "--porcelain")
                      (when detect-copies (list "-C"))
                      (when ref (list ref))
                      (list file))))
    (let ((lines (apply #'git-run-lines args)))
      (when lines
        (let ((result nil)
              (current-hash nil)
              (current-author nil)
              (current-date nil)
              (line-num 0))
          (dolist (line lines)
            (cond
              ((and (>= (length line) 40)
                    (every (lambda (c) (or (digit-char-p c 16))) (subseq line 0 40)))
               (setf current-hash (subseq line 0 40))
               (incf line-num))
              ((and (> (length line) 7)
                    (string= (subseq line 0 7) "author "))
               (setf current-author (subseq line 7)))
              ((and (> (length line) 12)
                    (string= (subseq line 0 12) "author-time "))
               (let ((timestamp (parse-integer (subseq line 12) :junk-allowed t)))
                 (when timestamp
                   (setf current-date (format-relative-time timestamp)))))
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
          (nreverse result))))))

(defun git-blame-parent (file hash)
  "Get blame for a file at the parent of the given commit.
   Useful for navigating blame history backwards."
  (let ((parent (git-run "rev-parse" (format nil "~A^" hash))))
    (when (and parent (> (length (string-trim '(#\Space #\Newline) parent)) 0))
      (git-blame-at file :ref (string-trim '(#\Space #\Newline) parent)))))

;;; Line range tracing (git log -L)

(defclass trace-entry ()
  ((commit-hash :initarg :commit-hash :accessor trace-commit-hash :initform nil)
   (short-hash :initarg :short-hash :accessor trace-short-hash :initform nil)
   (author :initarg :author :accessor trace-author :initform nil)
   (date :initarg :date :accessor trace-date :initform nil)
   (message :initarg :message :accessor trace-message :initform nil)
   (old-start :initarg :old-start :accessor trace-old-start :initform nil)
   (old-end :initarg :old-end :accessor trace-old-end :initform nil)
   (new-start :initarg :new-start :accessor trace-new-start :initform nil)
   (new-end :initarg :new-end :accessor trace-new-end :initform nil)
   (diff :initarg :diff :accessor trace-diff :initform nil))
  (:documentation "Represents a line-range trace entry from git log -L"))

(defun make-trace-entry (&key commit-hash short-hash author date message
                         old-start old-end new-start new-end diff)
  (make-instance 'trace-entry
                 :commit-hash commit-hash :short-hash short-hash
                 :author author :date date :message message
                 :old-start old-start :old-end old-end
                 :new-start new-start :new-end new-end :diff diff))

(defun git-log-line-range (file start-line end-line &key (limit 50))
  "Trace the history of a line range in a file using git log -L.
   Returns a list of trace-entry objects."
  (let* ((range-spec (format nil "~D,~D:~A" start-line end-line file))
         (output (git-run "log" "-L" range-spec
                          "--format=format:COMMIT %H %h %an %ad %s"
                          "--date=short"
                          (format nil "-~D" limit))))
    (when (and output (> (length output) 0))
      (let ((entries nil)
            (lines (cl-ppcre:split "\\n" output))
            (current-hash nil)
            (current-short nil)
            (current-author nil)
            (current-date nil)
            (current-message nil)
            (diff-lines nil))
        (dolist (line lines)
          (cond
            ;; Commit header line
            ((and (> (length line) 7) (string= (subseq line 0 7) "COMMIT "))
             ;; Save previous entry if any
             (when current-hash
               (push (make-trace-entry :commit-hash current-hash
                                       :short-hash current-short
                                       :author current-author
                                       :date current-date
                                       :message current-message
                                       :diff (nreverse diff-lines))
                     entries))
             ;; Parse new commit header
             (let ((parts (cl-ppcre:split " " line :limit 6)))
               (setf current-hash (second parts))
               (setf current-short (third parts))
               (setf current-author (fourth parts))
               (setf current-date (fifth parts))
               (setf current-message (if (> (length parts) 5) (sixth parts) ""))
               (setf diff-lines nil)))
            ;; Diff line (starts with +, -, @, or space)
            ((or (and (> (length line) 0)
                      (member (char line 0) '(#\+ #\- #\@ #\Space)))
                 (string= line ""))
             (when current-hash
               (push line diff-lines)))))
        ;; Save last entry
        (when current-hash
          (push (make-trace-entry :commit-hash current-hash
                                  :short-hash current-short
                                  :author current-author
                                  :date current-date
                                  :message current-message
                                  :diff (nreverse diff-lines))
                entries))
        (nreverse entries)))))

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

(defun git-branches (&optional (sort-mode :name))
  "Get list of branch names, sorted by SORT-MODE (:name, :date, :recent)"
  (let ((lines (case sort-mode
                 (:date
                  (git-run-lines "branch" "--sort=creatordate"
                                 "--format=%(refname:short)"))
                 (:recent
                  (git-run-lines "branch" "--sort=-committerdate"
                                 "--format=%(refname:short)"))
                 (t
                  (git-run-lines "branch" "--format=%(refname:short)")))))
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

(defun git-unstage-all ()
  "Unstage all staged changes"
  (git-run "reset" "HEAD"))

(defun git-discard-file (file)
  "Discard changes to a file"
  (git-run "checkout" "--" file))

;;; Commits

(defun git-commit (message)
  "Create a commit with message"
  (git-run "commit" "-m" message))

(defun git-amend ()
  "Amend the last commit without changing message"
  (git-run "commit" "--amend" "--no-edit"))

(defun git-amend-message (message)
  "Amend the last commit with a new message"
  (git-run "commit" "--amend" "-m" message))

(defun git-reword-commit (hash message)
  "Reword a commit message using interactive rebase.
   Only works for commits that haven't been pushed."
  ;; For HEAD, we can just amend
  (let ((head (string-trim '(#\Newline #\Space) (git-run "rev-parse" "HEAD"))))
    (if (string= hash head)
        (git-amend-message message)
        ;; For other commits, need interactive rebase - complex, skip for now
        (error "Rewording non-HEAD commits requires interactive rebase"))))

(defun git-reset-soft (ref)
  "Soft reset to REF - keeps changes staged"
  (git-run "reset" "--soft" ref))

(defun git-reset-mixed (ref)
  "Mixed reset to REF - keeps changes unstaged"
  (git-run "reset" "--mixed" ref))

(defun git-reset-hard (ref)
  "Hard reset to REF - discards all changes"
  (git-run "reset" "--hard" ref))

(defun git-fixup-commit (hash)
  "Create a fixup commit for the given commit hash"
  (git-run "commit" "--fixup" hash))

(defun git-commit-set-author (name email)
  "Amend HEAD to change author"
  (git-run "commit" "--amend" "--no-edit" 
           (format nil "--author=~A <~A>" name email)))

(defun git-commit-add-coauthor (name email)
  "Add co-author trailer to HEAD commit"
  (let* ((current-msg (string-trim '(#\Newline) (git-run "log" "-1" "--format=%B")))
         (trailer (format nil "~%~%Co-authored-by: ~A <~A>" name email))
         (new-msg (concatenate 'string current-msg trailer)))
    (git-amend-message new-msg)))

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

;;; Interactive Rebase

(defclass rebase-todo-entry ()
  ((action :initarg :action :accessor rebase-action :initform :pick
           :documentation "One of :pick, :reword, :squash, :fixup, :drop")
   (hash :initarg :hash :accessor rebase-hash :initform nil)
   (short-hash :initarg :short-hash :accessor rebase-short-hash :initform nil)
   (message :initarg :message :accessor rebase-message :initform nil)
   (new-message :initarg :new-message :accessor rebase-new-message :initform nil
                :documentation "New message for :reword action"))
  (:documentation "Represents a single entry in an interactive rebase todo list"))

(defun make-rebase-todo-entry (&key (action :pick) hash short-hash message)
  (make-instance 'rebase-todo-entry :action action :hash hash
                 :short-hash short-hash :message message))

(defmethod print-object ((entry rebase-todo-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~A ~A ~A" (rebase-action entry) 
            (rebase-short-hash entry) (rebase-message entry))))

(defun git-rebase-todo-list (base-commit)
  "Get list of rebase-todo-entry objects for commits from BASE-COMMIT to HEAD.
   BASE-COMMIT is typically a hash like HEAD~N or a branch name."
  (let ((lines (git-run-lines "log" "--reverse" "--pretty=format:%H|%h|%s"
                              (format nil "~A..HEAD" base-commit))))
    (loop for line in lines
          for parts = (cl-ppcre:split "\\|" line :limit 3)
          when (= (length parts) 3)
          collect (make-rebase-todo-entry
                   :action :pick
                   :hash (first parts)
                   :short-hash (second parts)
                   :message (third parts)))))

(defun rebase-action-string (action)
  "Convert rebase action keyword to git rebase todo string."
  (case action
    (:pick "pick")
    (:reword "reword")
    (:squash "squash")
    (:fixup "fixup")
    (:drop "drop")
    (t "pick")))

(defun write-rebase-todo-file (entries path)
  "Write rebase todo entries to a file in git rebase -i format."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (entry entries)
      (unless (eq (rebase-action entry) :drop)
        (format out "~A ~A ~A~%"
                (rebase-action-string (rebase-action entry))
                (rebase-short-hash entry)
                (if (and (eq (rebase-action entry) :reword)
                         (rebase-new-message entry))
                    (rebase-message entry)
                    (rebase-message entry)))))))

(defun repo-path-dir (repo)
  "Get repo path as a directory pathname (with trailing slash)."
  (let ((path (repo-path repo)))
    (if (char= (char path (1- (length path))) #\/)
        (pathname path)
        (pathname (concatenate 'string path "/")))))

(defun git-rebase-interactive (entries base-commit)
  "Execute an interactive rebase using the given todo ENTRIES.
   Uses GIT_SEQUENCE_EDITOR to inject our todo list.
   For :reword entries with new-message, creates a helper script."
  (let* ((repo (ensure-repo))
         (repo-dir (repo-path-dir repo))
         (todo-file (merge-pathnames ".gilt-rebase-todo" repo-dir))
         (reword-entries (remove-if-not (lambda (e) 
                                          (and (eq (rebase-action e) :reword)
                                               (rebase-new-message e)))
                                        entries))
         (editor-script (merge-pathnames ".gilt-rebase-editor" repo-dir)))
    ;; Write the todo file
    (write-rebase-todo-file entries todo-file)
    ;; Create the sequence editor script that copies our todo file
    (with-open-file (out editor-script :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%cp '~A' \"$1\"~%" (namestring todo-file)))
    ;; Make it executable
    (sb-ext:run-program "/bin/chmod" (list "+x" (namestring editor-script))
                        :output nil :error nil)
    ;; Build environment: inherit current env + add our vars
    (let* ((has-squash-or-fixup (some (lambda (e) (member (rebase-action e) '(:squash :fixup)))
                                      entries))
           (msg-script (merge-pathnames ".gilt-rebase-msg-editor" repo-dir))
           (env (append (sb-ext:posix-environ)
                        (list (format nil "GIT_SEQUENCE_EDITOR=~A" (namestring editor-script))))))
      ;; For squash/fixup: GIT_EDITOR must accept the combined message automatically
      ;; For reword: GIT_EDITOR writes the new message
      ;; If neither, use "true" as a no-op editor
      (cond
        (reword-entries
         (with-open-file (out msg-script :direction :output :if-exists :supersede)
           (format out "#!/bin/sh~%")
           (format out "echo '~A' > \"$1\"~%"
                   (rebase-new-message (first reword-entries))))
         (sb-ext:run-program "/bin/chmod" (list "+x" (namestring msg-script))
                             :output nil :error nil)
         (push (format nil "GIT_EDITOR=~A" (namestring msg-script)) env))
        (has-squash-or-fixup
         ;; Use "true" to auto-accept the combined commit message
         (push "GIT_EDITOR=true" env))
        (t
         ;; No editor needed, but set one just in case
         (push "GIT_EDITOR=true" env)))
      ;; Run the rebase
      (let ((result
              (with-output-to-string (s)
                (sb-ext:run-program "/usr/bin/git" 
                                    (list "rebase" "-i" base-commit)
                                    :output s :error s
                                    :environment env
                                    :directory (repo-path repo)))))
        ;; Clean up temp files
        (ignore-errors (delete-file todo-file))
        (ignore-errors (delete-file editor-script))
        (ignore-errors (delete-file msg-script))
        result))))

(defun git-rebase-abort ()
  "Abort an in-progress rebase."
  (git-run "rebase" "--abort"))

(defun git-rebase-continue ()
  "Continue a rebase after resolving conflicts."
  (git-run "rebase" "--continue"))

(defun git-rebase-skip ()
  "Skip the current commit during a rebase."
  (git-run "rebase" "--skip"))

(defun git-rebase-onto (branch)
  "Rebase current branch onto BRANCH."
  (git-run "rebase" branch))

;;; Branches

(defun git-checkout (branch)
  "Checkout a branch"
  (git-run "checkout" branch))

(defun git-create-branch (name)
  "Create and checkout a new branch"
  (git-run "checkout" "-b" name))

(defun git-move-commits-to-new-branch (new-branch-name count)
  "Move the last COUNT commits from the current branch to a new branch.
   Creates NEW-BRANCH-NAME at current HEAD, then resets current branch back."
  (let ((current (git-current-branch)))
    ;; Create the new branch at current HEAD (without switching to it)
    (git-run "branch" new-branch-name)
    ;; Reset current branch back by COUNT commits
    (git-run "reset" "--hard" (format nil "HEAD~~~D" count))
    ;; Switch to the new branch
    (git-run "checkout" new-branch-name)
    current))

(defun git-rename-branch (old-name new-name)
  "Rename a local branch"
  (git-run "branch" "-m" old-name new-name))

(defun git-fast-forward (branch)
  "Fast-forward BRANCH to match its upstream. Must not be the current branch."
  (git-run "fetch" "origin" (format nil "~A:~A" branch branch)))

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

(defun git-edit-file (file &optional line)
  "Open a file in the user's preferred editor ($EDITOR or vi).
   If LINE is given, pass +LINE to the editor (vim/nvim style)."
  (let ((editor (or (uiop:getenv "EDITOR") "vi")))
    (uiop:run-program (if line
                          (list editor (format nil "+~D" line) file)
                          (list editor file))
                      :input :interactive
                      :output :interactive
                      :error-output :interactive)))

(defun git-open-in-editor (path)
  "Open a directory or file in the user's preferred editor ($EDITOR or vi).
   Many editors (vim, code, etc.) accept a directory to open a file browser."
  (let ((editor (or (uiop:getenv "EDITOR") "vi")))
    (uiop:run-program (list editor path)
                      :input :interactive
                      :output :interactive
                      :error-output :interactive)))

(defun git-commit-with-editor ()
  "Run git commit using the user's $EDITOR for the commit message.
   Returns t if commit succeeded, nil if aborted."
  (let ((result (nth-value 2
                  (uiop:run-program (list "git" "commit")
                                    :input :interactive
                                    :output :interactive
                                    :error-output :interactive
                                    :ignore-error-status t))))
    (zerop result)))

(defun git-commit-no-verify (message)
  "Create a commit with message, bypassing pre-commit hooks."
  (git-run "commit" "--no-verify" "-m" message))

(defun git-undo ()
  "Undo the last git operation by resetting to the previous reflog entry.
   Returns the reflog message of what was undone, or nil if nothing to undo."
  (let ((reflog (git-run-lines "reflog" "--format=%H %gs" "-n" "2")))
    (when (>= (length reflog) 2)
      (let* ((prev-line (second reflog))
             (space-pos (position #\Space prev-line))
             (prev-hash (subseq prev-line 0 space-pos))
             (prev-msg (subseq prev-line (1+ space-pos))))
        (git-run "reset" "--hard" prev-hash)
        prev-msg))))

(defun git-redo ()
  "Redo the last undone git operation.
   Walks the reflog to find the entry after the current HEAD.
   Returns the reflog message of what was redone, or nil if nothing to redo."
  (let* ((current-hash (string-trim '(#\Newline #\Space)
                                     (git-run "rev-parse" "HEAD")))
         (reflog (git-run-lines "reflog" "--format=%H %gs" "-n" "100")))
    ;; Find current HEAD in reflog, then return the entry before it (newer)
    (loop for i from 1 below (length reflog)
          for line = (nth i reflog)
          for space-pos = (position #\Space line)
          for hash = (subseq line 0 space-pos)
          when (string= hash current-hash)
            do (let* ((newer-line (nth (1- i) reflog))
                      (newer-space (position #\Space newer-line))
                      (newer-hash (subseq newer-line 0 newer-space))
                      (newer-msg (subseq newer-line (1+ newer-space))))
                 (git-run "reset" "--hard" newer-hash)
                 (return newer-msg)))))

;;; Reflog

(defclass reflog-entry ()
  ((hash :initarg :hash :accessor reflog-hash :initform nil)
   (short-hash :initarg :short-hash :accessor reflog-short-hash :initform nil)
   (ref :initarg :ref :accessor reflog-ref :initform nil)
   (message :initarg :message :accessor reflog-message :initform nil)
   (selector :initarg :selector :accessor reflog-selector :initform nil))
  (:documentation "Represents a git reflog entry"))

(defun make-reflog-entry (&key hash short-hash ref message selector)
  (make-instance 'reflog-entry :hash hash :short-hash short-hash
                 :ref ref :message message :selector selector))

(defun git-reflog (&key (limit 100))
  "Return a list of reflog-entry objects.
   Uses --format to get hash, selector, and message in a parseable format."
  (let ((output (git-run "reflog" "--format=%H%x09%gd%x09%gs"
                         (format nil "-n ~D" limit))))
    (when (and output (> (length output) 0))
      (loop for line in (cl-ppcre:split "\\n" output)
            when (> (length (string-trim '(#\Space #\Tab) line)) 0)
              collect (let* ((parts (cl-ppcre:split "\\t" line))
                             (hash (first parts))
                             (selector (second parts))
                             (message (or (third parts) ""))
                             (short-hash (when hash (subseq hash 0 (min 8 (length hash))))))
                        (make-reflog-entry :hash hash :short-hash short-hash
                                           :ref selector :message message
                                           :selector selector))))))

(defun git-reflog-diff (selector)
  "Show the diff for a reflog entry identified by its selector (e.g. HEAD@{3})."
  (git-run "diff" selector))

(defun git-reflog-show (selector)
  "Show the commit details for a reflog entry."
  (git-run "show" "--stat" "--patch" selector))

;;; Grep

(defclass grep-result ()
  ((file :initarg :file :accessor grep-result-file :initform nil)
   (line :initarg :line :accessor grep-result-line :initform nil)
   (content :initarg :content :accessor grep-result-content :initform nil))
  (:documentation "Represents a single git grep match"))

(defun make-grep-result (&key file line content)
  (make-instance 'grep-result :file file :line line :content content))

(defun git-grep (pattern &key (ref nil) (ignore-case nil) (line-numbers t))
  "Run git grep and return a list of grep-result objects.
   PATTERN is the search string (regex).
   REF, if given, searches at that revision instead of the working tree.
   IGNORE-CASE adds -i flag.
   LINE-NUMBERS adds -n flag (default true)."
  (when (or (null pattern) (= (length (string-trim '(#\Space) pattern)) 0))
    (return-from git-grep nil))
  (let* ((args '("grep" "--no-color"))
         (args (if line-numbers (append args '("-n")) args))
         (args (if ignore-case (append args '("-i")) args))
         (args (if ref (append args (list ref "--")) (append args '("--"))))
         (args (append args (list pattern)))
         (output (apply #'git-run args)))
    (when (and output (> (length output) 0))
      (loop for line in (cl-ppcre:split "\\n" output)
            when (> (length (string-trim '(#\Space #\Tab) line)) 0)
              collect (let* ((parts (cl-ppcre:split ":" line :limit 3))
                             (file (first parts))
                             (line-num (when (second parts)
                                         (parse-integer (second parts) :junk-allowed t)))
                             (content (or (third parts) "")))
                        (make-grep-result :file file :line line-num :content content))))))

(defun git-show-file (ref path)
  "Show the content of a file at a given ref (commit/branch/tag).
   Returns the raw file content as a string."
  (git-run "show" (format nil "~A:~A" ref path)))

(defun bat-available-p ()
  "Check if bat or batcat is available on the system."
  (or (probe-file "/usr/bin/bat")
      (probe-file "/usr/bin/batcat")
      (ignore-errors (uiop:run-program '("which" "bat") :output nil :error nil))))

(defun bat-highlight (content &optional (language nil))
  "Pipe content through bat for syntax highlighting.
   Returns ANSI-highlighted string. Falls back to raw content if bat unavailable."
  (let ((bat-bin (cond ((probe-file "/usr/bin/bat") "/usr/bin/bat")
                       ((probe-file "/usr/bin/batcat") "/usr/bin/batcat")
                       (t nil))))
    (if bat-bin
        (let ((args (if language
                        (list bat-bin "--color=always" "--style=plain"
                              (format nil "--language=~A" language))
                        (list bat-bin "--color=always" "--style=plain"))))
          (with-output-to-string (s)
            (uiop:run-program args
                              :input (make-string-input-stream content)
                              :output s :error nil)))
        content)))

;;; Tree/Blob browsing

(defclass tree-entry ()
  ((mode :initarg :mode :accessor tree-entry-mode :initform nil)
   (type :initarg :type :accessor tree-entry-type :initform nil)  ; :blob, :tree, :commit
   (hash :initarg :hash :accessor tree-entry-hash :initform nil)
   (name :initarg :name :accessor tree-entry-name :initform nil)
   (path :initarg :path :accessor tree-entry-path :initform nil))  ; full path from tree root
  (:documentation "Represents a git tree entry (file or directory)"))

(defun make-tree-entry (&key mode type hash name path)
  (make-instance 'tree-entry :mode mode :type type :hash hash :name name :path path))

(defun git-ls-tree (ref &optional (path ""))
  "List the contents of a tree at REF, optionally in a subdirectory PATH.
   Returns a list of tree-entry objects."
  (let ((tree-spec (if (or (null path) (string= path ""))
                       ref
                       (format nil "~A:~A" ref path))))
    (let ((output (git-run "ls-tree" tree-spec)))
      (when (and output (> (length output) 0))
        (loop for line in (cl-ppcre:split "\\n" output)
              when (> (length (string-trim '(#\Space #\Tab) line)) 0)
                collect (let* ((parts (cl-ppcre:split "\\t" line :limit 2))
                               (meta (first parts))
                               (name (or (second parts) ""))
                               (meta-parts (cl-ppcre:split "\\s+" meta))
                               (mode (first meta-parts))
                               (type (second meta-parts))
                               (hash (third meta-parts))
                               (full-path (if (or (null path) (string= path ""))
                                              name
                                              (format nil "~A/~A" path name))))
                          (make-tree-entry :mode mode
                                           :type (cond
                                                   ((string= type "blob") :blob)
                                                   ((string= type "tree") :tree)
                                                   ((string= type "commit") :commit)
                                                   (t nil))
                                           :hash hash
                                           :name name
                                           :path full-path)))))))

(defun git-checkout-tag (tag-name)
  "Checkout a tag as detached HEAD."
  (git-run "checkout" tag-name))

(defun git-rename-stash (index new-message)
  "Rename a stash entry. Gets the SHA, drops, then re-stores with new message."
  (let* ((stash-ref (format nil "stash@{~D}" index))
         (sha (string-trim '(#\Newline #\Space)
                           (git-run "rev-parse" stash-ref))))
    (git-run "stash" "drop" stash-ref)
    (git-run "stash" "store" "-m" new-message sha)))

(defun git-set-upstream (branch remote-branch)
  "Set the upstream tracking branch for a local branch."
  (git-run "branch" (format nil "--set-upstream-to=~A" remote-branch) branch))

(defun git-unset-upstream (branch)
  "Remove the upstream tracking branch for a local branch."
  (git-run "branch" "--unset-upstream" branch))

(defun git-merge-squash (branch)
  "Squash merge a branch into current (stages changes, does not commit)."
  (git-run "merge" "--squash" branch))

(defun git-bisect-start ()
  "Start a bisect session."
  (git-run "bisect" "start"))

(defun git-bisect-bad (&optional commit)
  "Mark a commit as bad during bisect. Returns bisect output."
  (if commit
      (git-run "bisect" "bad" commit)
      (git-run "bisect" "bad")))

(defun git-bisect-good (&optional commit)
  "Mark a commit as good during bisect. Returns bisect output."
  (if commit
      (git-run "bisect" "good" commit)
      (git-run "bisect" "good")))

(defun git-bisect-skip ()
  "Skip the current commit during bisect."
  (git-run "bisect" "skip"))

(defun git-bisect-reset ()
  "End the bisect session and return to original HEAD."
  (git-run "bisect" "reset"))

(defun git-bisect-log ()
  "Get the bisect log showing steps taken so far."
  (git-run "bisect" "log"))

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

(defun git-push-force-with-lease (&optional remote branch)
  "Force push with lease (safer than force push).
   Only forces if remote ref hasn't changed since last fetch."
  (if (and remote branch)
      (git-run "push" "--force-with-lease" remote branch)
      (git-run "push" "--force-with-lease")))

(defun git-push-force (&optional remote branch)
  "Force push (overwrites remote history). Use with caution."
  (if (and remote branch)
      (git-run "push" "--force" remote branch)
      (git-run "push" "--force")))

(defun git-push-set-upstream (remote branch)
  "Push and set upstream tracking branch."
  (git-run "push" "--set-upstream" remote branch))

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
   (locked-reason :initarg :locked-reason :accessor worktree-locked-reason :initform nil)
   (prunable :initarg :prunable :accessor worktree-prunable :initform nil)
   ;; Derived fields (populated by git-worktree-list, not by porcelain output)
   (current :initarg :current :accessor worktree-current :initform nil
            :documentation "T if this is the worktree gilt is currently operating in")
   (main :initarg :main :accessor worktree-main :initform nil
         :documentation "T if this is the main worktree (first listed by git)")
   (missing :initarg :missing :accessor worktree-missing :initform nil
            :documentation "T if the worktree's directory no longer exists on disk")
   (name :initarg :name :accessor worktree-name :initform nil
         :documentation "Short uniquified name derived from the path"))
  (:documentation "Represents a git worktree"))

(defun make-worktree-entry (&key path head branch bare detached locked prunable
                             locked-reason current main missing name)
  (make-instance 'worktree-entry :path path :head head :branch branch
                 :bare bare :detached detached :locked locked
                 :locked-reason locked-reason :prunable prunable
                 :current current :main main :missing missing :name name))

(defun normalize-path (path)
  "Normalize a filesystem path for comparison: expand, strip trailing slashes."
  (let* ((trimmed (string-trim '(#\Newline #\Space #\Tab) path))
         (no-slash (string-right-trim '(#\/) trimmed)))
    (if (string= no-slash "") "/" no-slash)))

(defun worktree-dir-exists-p (path)
  "Return T if PATH is an existing directory."
  (let ((p (string-right-trim '(#\/) (string-trim '(#\Newline #\Space) path))))
    (when (and (> (length p) 0)
               (handler-case (probe-file (concatenate 'string p "/"))
                 (error () nil)))
      t)))

(defun worktree-basename (path)
  "Return the final path component of PATH."
  (let ((norm (normalize-path path)))
    (car (last (cl-ppcre:split "/" norm)))))

(defun assign-worktree-names (worktrees)
  "Assign short uniquified names to each worktree based on path basenames.
   If basenames collide, disambiguate by prepending successive parent dirs."
  (let ((basenames (mapcar #'(lambda (wt)
                               (worktree-basename (worktree-path wt)))
                           worktrees)))
    (loop for wt in worktrees
          for basename in basenames
          for i from 0
          ;; Count how many worktrees share this basename
          for dup-count = (count basename basenames :test #'string=)
          do (setf (worktree-name wt)
                   (if (or (worktree-bare wt) (<= dup-count 1))
                       basename
                       ;; Disambiguate: prepend parent dir basename
                       (let* ((norm (normalize-path (worktree-path wt)))
                              (parts (cl-ppcre:split "/" norm))
                              (parent (when (>= (length parts) 2)
                                        (nth (- (length parts) 2) parts))))
                         (if parent
                             (concatenate 'string parent "/" basename)
                             basename)))))
    worktrees))

(defun git-worktree-current-path ()
  "Return the normalized toplevel path of the current working tree, or nil
   (e.g. inside a bare repo)."
  (ignore-errors
    (let ((top (string-trim '(#\Newline #\Space)
                            (git-run "rev-parse" "--show-toplevel"))))
      (when (and top (> (length top) 0))
        (normalize-path top)))))

(defun git-worktree-list ()
  "Get list of worktree-entry objects with derived fields populated:
   current, main, missing, name, locked-reason."
  (let ((lines (git-run-lines "worktree" "list" "--porcelain"))
        (current-toplevel (git-worktree-current-path)))
    (let ((worktrees nil)
          (current nil)
          (idx 0))
      (dolist (line lines)
        (cond
          ((string= line "")
           (when current
             (push current worktrees)
             (setf current nil)))
          ((cl-ppcre:scan "^worktree " line)
           (setf current (make-worktree-entry :path (subseq line 9)))
           ;; First listed worktree is the main worktree
           (when (= idx 0)
             (setf (worktree-main current) t))
           (incf idx))
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
           (when current
             (setf (worktree-locked current) t)
             ;; "locked <reason>" or just "locked"
             (when (> (length line) 6)
               (setf (worktree-locked-reason current) (subseq line 7)))))
          ((cl-ppcre:scan "^prunable" line)
           (when current (setf (worktree-prunable current) t)))))
      (when current
        (push current worktrees))
      (setf worktrees (nreverse worktrees))
      ;; Populate derived fields
      (dolist (wt worktrees)
        ;; Current worktree detection
        (when (and current-toplevel
                   (string= (normalize-path (worktree-path wt)) current-toplevel))
          (setf (worktree-current wt) t))
        ;; Missing-path detection (only for non-bare worktrees)
        (unless (worktree-bare wt)
          (unless (worktree-dir-exists-p (worktree-path wt))
            (setf (worktree-missing wt) t))))
      ;; Detect branch during rebase/bisect when git doesn't report it
      (dolist (wt worktrees)
        (when (and (not (worktree-branch wt))
                   (not (worktree-detached wt))
                   (not (worktree-bare wt))
                   (not (worktree-missing wt)))
          (let ((detected (git-worktree-detect-branch wt)))
            (when detected
              (setf (worktree-branch wt) detected)))))
      (assign-worktree-names worktrees)
      worktrees)))

(defun git-worktree-detect-branch (wt)
  "Try to detect the branch name for a worktree that didn't report one
   (e.g. during rebase/bisect). Reads rebase-merge/head-name or BISECT_START
   from the worktree's private git dir. Returns branch name or nil."
  (ignore-errors
    (let* ((wt-path (worktree-path wt))
           ;; The worktree's private git dir: <common-dir>/worktrees/<basename>/
           (common-dir (string-trim '(#\Newline #\Space)
                                    (git-run "rev-parse" "--git-common-dir")))
           (wt-gitdir (concatenate 'string
                                   (string-right-trim '(#\/) common-dir)
                                   "/worktrees/"
                                   (worktree-basename wt-path)
                                   "/")))
      (flet ((read-file (p)
               (when (probe-file p)
                 (string-trim '(#\Newline #\Space)
                              (with-open-file (f p :direction :input)
                                (read-line f nil ""))))))
        (or
         ;; rebase-merge/head-name contains "refs/heads/<branch>"
         (let ((head-name (read-file (concatenate 'string wt-gitdir "rebase-merge/head-name"))))
           (when (and head-name (cl-ppcre:scan "^refs/heads/" head-name))
             (subseq head-name 11)))
         ;; BISECT_START contains the branch name directly
         (read-file (concatenate 'string wt-gitdir "BISECT_START")))))))

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

(defun git-worktree-add-detached (path ref)
  "Add a new worktree at PATH with detached HEAD at REF (commit/tag/branch)."
  (git-run "worktree" "add" "--detach" path ref))

(defun git-worktree-add-from-ref (path ref &key new-branch)
  "Add a new worktree at PATH from REF (branch/commit/tag).
   If NEW-BRANCH is given, create a new branch named NEW-BRANCH at REF.
   Otherwise check out REF in the worktree (REF must be a branch) or detach."
  (cond
    (new-branch
     (git-run "worktree" "add" "-b" new-branch path ref))
    ((git-branch-exists-p ref)
     (git-run "worktree" "add" path ref))
    (t
     ;; Not a local branch (commit/tag/remote) -> detached
     (git-run "worktree" "add" "--detach" path ref))))

(defun git-worktree-remove (path &optional force)
  "Remove a worktree at PATH."
  (if force
      (git-run "worktree" "remove" "--force" path)
      (git-run "worktree" "remove" path)))

(defun git-worktree-move (path new-path)
  "Move a worktree from PATH to NEW-PATH."
  (git-run "worktree" "move" path new-path))

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

(defun git-branch-exists-p (branch)
  "Return T if BRANCH exists as a local branch."
  (let ((output (ignore-errors
                  (git-run "rev-parse" "--verify"
                           (concatenate 'string "refs/heads/" branch)))))
    (and output (> (length (string-trim '(#\Newline #\Space) output)) 0))))

(defun git-branch-in-worktree-p (branch)
  "Return the path of the worktree that has BRANCH checked out, or nil.
   A branch can only be checked out in one worktree at a time."
  (dolist (wt (git-worktree-list))
    (when (and (worktree-branch wt)
               (string= (worktree-branch wt) branch)
               (not (worktree-missing wt)))
      (return (worktree-path wt)))))

(defun git-worktree-detach (path)
  "Detach the HEAD of the worktree at PATH so its branch can be deleted
   or checked out elsewhere. Runs git checkout --detach in that worktree."
  (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/git" '("checkout" "--detach")
                        :output s :error nil :search t
                        :directory (pathname (concatenate 'string
                                                          (string-right-trim '(#\/) path)
                                                          "/")))))

(defun git-worktree-remove-and-delete-branch (path branch
                                               &key delete-remote force)
  "Remove the worktree at PATH and delete its BRANCH.
   If DELETE-REMOTE is T, also delete the remote tracking branch.
   If FORCE is T, force-remove the worktree."
  ;; Remove the worktree first
  (git-worktree-remove path force)
  ;; Delete local branch
  (ignore-errors (git-run "branch" "-D" branch))
  ;; Optionally delete remote branch
  (when delete-remote
    (ignore-errors (git-run "push" "origin" "--delete" branch))))

(defun enter-worktree (worktree-path)
  "Switch gilt's current repository context to the worktree at WORKTREE-PATH.
   Pushes the current repo onto the parent-repo stack so it can be restored.
   Returns T on success."
  (let* ((repo (ensure-repo))
         (norm-path (normalize-path worktree-path))
         (name (worktree-basename norm-path)))
    (push repo *parent-repo-stack*)
    (setf *current-repo* (make-instance 'git-repository :path norm-path :name name))
    t))

(defun leave-worktree ()
  "Leave the current worktree and restore the previous repository context.
   Returns T if there was a parent repo to restore."
  (leave-submodule))

(defun git-worktree-repair (path)
  "Repair worktree administrative files for the worktree at PATH."
  (git-run "worktree" "repair" path))

(defun git-worktree-candidate-paths ()
  "Return a list of candidate parent directories for new worktrees.
   Includes the repo root's parent directory, the parent directories of
   existing worktrees, the configured default path (if any), and the home
   directory, deduplicated. Useful for a location picker."
  (let ((repo-root (git-repo-root))
        (worktrees (git-worktree-list))
        (default-path (gilt-worktree-default-path))
        (candidates nil))
    ;; Add parents of existing worktrees (in worktree order)
    (dolist (wt worktrees)
      (let* ((path (worktree-path wt))
             (norm (normalize-path path))
             (parts (cl-ppcre:split "/" norm))
             (parent (when (>= (length parts) 2)
                       (format nil "~{~A~^/~}" (subseq parts 0 (1- (length parts)))))))
        (when (and parent (not (member parent candidates :test #'string=)))
          (push parent candidates))))
    ;; Add configured default path
    (when (and default-path (> (length default-path) 0))
      (let ((expanded (expand-tilde-path default-path)))
        ;; Resolve relative paths against repo root
        (let ((resolved (if (or (char= (char expanded 0) #\/)
                                (string= expanded "~"))
                            expanded
                            (when repo-root
                              (concatenate 'string
                                           (string-right-trim '(#\/) repo-root)
                                           "/" expanded)))))
          (when (and resolved (not (member resolved candidates :test #'string=)))
            (push resolved candidates)))))
    ;; Add repo root's parent as fallback
    (when repo-root
      (let* ((norm (normalize-path repo-root))
             (parts (cl-ppcre:split "/" norm))
             (parent (when (>= (length parts) 2)
                       (format nil "~{~A~^/~}" (subseq parts 0 (1- (length parts)))))))
        (when (and parent (not (member parent candidates :test #'string=)))
          (push parent candidates))))
    ;; Add home directory as a common choice
    (let ((home (namestring (user-homedir-pathname))))
      (unless (member home candidates :test #'string=)
        (push home candidates)))
    (nreverse candidates)))

(defun expand-tilde-path (path)
  "Expand a leading ~ in PATH to the user's home directory."
  (if (and (> (length path) 0) (char= (char path 0) #\~))
      (concatenate 'string (namestring (user-homedir-pathname))
                   (subseq path 1))
      path))

(defun gilt-worktree-config-file ()
  "Path to the gilt worktree config file."
  (merge-pathnames "worktree.conf" (gilt-config-dir)))

(defun gilt-worktree-default-path ()
  "Read the configured default parent directory for new worktrees from
   ~/.config/gilt/worktree.conf. Returns the path string or nil.
   Format: one line 'defaultPath=<path>', comments start with #."
  (let ((file (gilt-worktree-config-file)))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (loop for line = (read-line s nil nil)
              while line
              for trimmed = (string-trim '(#\Space #\Tab) line)
              when (and (> (length trimmed) 0)
                        (char/= (char trimmed 0) #\#)
                        (search "defaultPath=" trimmed))
                do (return (string-trim '(#\Space #\Tab)
                                        (subseq trimmed (+ 12 (search "defaultPath=" trimmed))))))))))

(defun git-run-in-dir (dir &rest args)
  "Run a git command in directory DIR (not the current repo) and return output.
   Uses --git-dir and --work-tree to target a different worktree."
  ;; For linked worktrees, .git is a file pointing to the real gitdir.
  ;; For the main worktree, .git is a directory. Either way, git accepts
  ;; --git-dir pointing at it. But for linked worktrees we need the actual
  ;; gitdir. Fall back to just running in that directory.
  (with-output-to-string (s)
    (sb-ext:run-program "/usr/bin/git" args
                        :output s
                        :error nil
                        :search t
                        :directory (pathname (concatenate 'string
                                                          (string-right-trim '(#\/) dir)
                                                          "/")))))

(defun git-fast-forward-in-worktree (branch worktree-path)
  "Fast-forward BRANCH to match its upstream, running the fetch in the
   worktree at WORKTREE-PATH so the branch ref updates without needing to
   switch into that worktree. Returns the git output."
  (let ((remote (or (git-upstream-remote branch) "origin"))
        (upstream-branch (or (git-upstream-branch branch) branch)))
    (apply #'git-run-in-dir worktree-path
           (list "fetch" remote
                 (format nil "~A:~A" upstream-branch branch)))))

(defun git-upstream-remote (branch)
  "Get the upstream remote name for BRANCH (e.g. 'origin'), or nil."
  (ignore-errors
    (let ((ref (git-run "for-each-ref"
                        "--format=%(upstream:short)"
                        (concatenate 'string "refs/heads/" branch))))
      (let ((trimmed (string-trim '(#\Newline #\Space) ref)))
        (when (and trimmed (> (length trimmed) 0))
          (let ((slash-pos (position #\/ trimmed)))
            (when slash-pos
              (subseq trimmed 0 slash-pos))))))))

(defun git-upstream-branch (branch)
  "Get the upstream branch name for BRANCH (without remote prefix), or nil."
  (ignore-errors
    (let ((ref (git-run "for-each-ref"
                        "--format=%(upstream:short)"
                        (concatenate 'string "refs/heads/" branch))))
      (let ((trimmed (string-trim '(#\Newline #\Space) ref)))
        (when (and trimmed (> (length trimmed) 0))
          (let ((slash-pos (position #\/ trimmed)))
            (when slash-pos
              (subseq trimmed (1+ slash-pos)))))))))

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

(defun git-stash-push-files (files &optional message)
  "Stash specific FILES (list of file paths) with optional message.
   Includes untracked files so new files can also be stashed."
  (let ((args (list "stash" "push" "--include-untracked")))
    (when message
      (setf args (append args (list "-m" message))))
    (setf args (append args (list "--") files))
    (apply #'git-run args)))

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

;;; Git notes support

(defun git-notes-show (object)
  "Show the note for a given object (commit hash, ref, etc.)."
  (git-run "notes" "show" object))

(defun git-notes-add (object message)
  "Add a note to OBJECT with MESSAGE. Overwrites existing note."
  (git-run "notes" "add" "-f" "-m" message object))

(defun git-notes-remove (object)
  "Remove the note for OBJECT."
  (git-run "notes" "remove" object))

(defun git-notes-list (&optional object)
  "List notes. If OBJECT is given, lists notes for that object.
   Returns list of (note-ref . object-hash) pairs."
  (let ((lines (if object
                   (git-run-lines "notes" "list" object)
                   (git-run-lines "notes" "list"))))
    (loop for line in lines
          when (and line (> (length line) 0))
          collect (let ((parts (cl-ppcre:split "\\s+" line :limit 2)))
                    (when (>= (length parts) 2)
                      (cons (first parts) (second parts)))))))

;;; git clean support

(defun git-clean-dry-run (&key (directories nil) (force nil) (ignored nil))
  "Preview what git clean would remove. Returns list of paths.
   :DIRECTORIES - include untracked directories
   :FORCE - force clean (overrides safety check)
   :IGNORED - include ignored files"
  (let ((args (list "clean" "--dry-run")))
    (when directories (push "-d" (cdr (last args))))
    (when force (push "-f" (cdr (last args))))
    (when ignored (push "-x" (cdr (last args))))
    (let ((output (apply #'git-run args)))
      (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Space) output)))))

(defun git-clean (&key (directories nil) (force nil) (ignored nil))
  "Remove untracked files. Use git-clean-dry-run first to preview.
   :DIRECTORIES - remove untracked directories too
   :FORCE - force clean
   :IGNORED - remove ignored files too"
  (let ((args (list "clean")))
    (when directories (push "-d" (cdr (last args))))
    (when force (push "-f" (cdr (last args))))
    (when ignored (push "-x" (cdr (last args))))
    (apply #'git-run args)))

;;; Format-patch / apply-patch support

(defun git-format-patch (commit &key (output-dir "."))
  "Generate patch files for commits from COMMIT to HEAD.
   Returns list of generated patch filenames."
  (let ((output (git-run "format-patch" "-o" output-dir commit)))
    (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Space) output))))

(defun git-format-patch-single (commit &key (output-dir "."))
  "Generate a single patch file for one commit.
   Returns the patch filename."
  (let ((output (git-run "format-patch" "-1" "-o" output-dir commit)))
    (string-trim '(#\Newline #\Space) output)))

(defun git-apply-patch-file (patch-file &key (check nil) (signoff nil))
  "Apply a patch file.
   :CHECK - check only, don't apply
   :SIGNOFF - add Signed-off-by line"
  (let ((args (list "apply")))
    (when check (push "--check" (cdr (last args))))
    (when signoff (push "--signoff" (cdr (last args))))
    (apply #'git-run (append args (list patch-file)))))

(defun git-am-patch (patch-file &key (signoff nil))
  "Apply a patch using git am (mailbox format).
   :SIGNOFF - add Signed-off-by line"
  (let ((args (list "am")))
    (when signoff (push "--signoff" (cdr (last args))))
    (apply #'git-run (append args (list patch-file)))))

;;; Clipboard support

(defun copy-to-clipboard (text)
  "Copy TEXT to system clipboard. Tries xclip, xsel, wl-copy, pbcopy."
  (let ((programs '(("xclip" "-selection" "clipboard")
                    ("xsel" "--clipboard" "--input")
                    ("wl-copy")
                    ("pbcopy"))))
    (loop for prog-args in programs
          for prog = (first prog-args)
          for args = (rest prog-args)
          when (ignore-errors
                 (let ((proc (sb-ext:run-program prog args
                                                 :input :stream
                                                 :output nil
                                                 :error nil
                                                 :search t
                                                 :wait nil)))
                   (write-string text (sb-ext:process-input proc))
                   (close (sb-ext:process-input proc))
                   (sb-ext:process-wait proc)
                   (zerop (sb-ext:process-exit-code proc))))
            do (return t)
          finally (return nil))))

;;; Ignore file

(defun git-ignore-file (file)
  "Add FILE to .gitignore in the repository root."
  (let* ((repo (ensure-repo))
         (gitignore-path (merge-pathnames ".gitignore" (repo-path-dir repo))))
    (with-open-file (s gitignore-path
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (format s "~&~A~%" file))
    (format nil "Added '~A' to .gitignore" file)))

;;; External diff tool

(defun git-difftool (file)
  "Launch external diff tool for FILE. Suspends TUI."
  (let* ((repo (ensure-repo))
         (dir (repo-path-dir repo)))
    (uiop:run-program (list "git" "difftool" "--no-prompt" "--" file)
                       :directory dir
                       :input :interactive
                       :output :interactive
                       :error-output :interactive)))

;;; Shell command execution

(defun git-shell-command (command)
  "Run an arbitrary shell command in the repo directory. Returns output string."
  (let* ((repo (ensure-repo))
         (dir (repo-path-dir repo)))
    (with-output-to-string (s)
      (sb-ext:run-program "/bin/sh" (list "-c" command)
                          :output s
                          :error s
                          :directory dir
                          :search t))))

;;; Open in browser

(defun resolve-ssh-hostname (alias)
  "Resolve an SSH host alias to its real Hostname from ~/.ssh/config.
   Returns the alias unchanged if no config entry is found."
  (let ((config-path (merge-pathnames ".ssh/config" (user-homedir-pathname))))
    (or (when (probe-file config-path)
          (with-open-file (s config-path :direction :input)
            (let ((in-host-block nil))
              (loop for line = (read-line s nil nil)
                    while line
                    for trimmed = (string-trim '(#\Space #\Tab) line)
                    do (cond
                         ((cl-ppcre:scan "(?i)^Host\\s+" trimmed)
                          (let ((host-value (string-trim '(#\Space #\Tab)
                                                          (subseq trimmed (length "Host ")))))
                            (setf in-host-block (string= host-value alias))))
                         ((and in-host-block (cl-ppcre:scan "(?i)^Hostname\\s+" trimmed))
                          (return (string-trim '(#\Space #\Tab)
                                                (subseq trimmed (length "Hostname "))))))))))
        alias)))

(defun git-remote-url-for-browser ()
  "Get the remote URL converted to a browser-friendly HTTPS URL."
  (let* ((url (string-trim '(#\Newline #\Space)
                            (ignore-errors (git-run "remote" "get-url" "origin")))))
    (when (and url (> (length url) 0))
      ;; Convert git@github.com:user/repo.git to https://github.com/user/repo
      (cond
        ((cl-ppcre:scan "^git@" url)
         ;; Extract SSH host, resolve alias from ~/.ssh/config
         (let* ((host (cl-ppcre:regex-replace "^git@([^:]+):.*" url "\\1"))
                (real-host (resolve-ssh-hostname host))
                (https-url (cl-ppcre:regex-replace "^git@[^:]+:" url
                                                    (format nil "https://~A/" real-host))))
           (cl-ppcre:regex-replace "\\.git$" https-url "")))
        ((cl-ppcre:scan "^https?://" url)
         (cl-ppcre:regex-replace "\\.git$" url ""))
        (t url)))))

(defun open-in-browser (url)
  "Open URL in the default browser. Tries xdg-open, open (macOS), wslview."
  (loop for prog in '("xdg-open" "open" "wslview")
        when (ignore-errors
               (sb-ext:run-program prog (list url)
                                   :output nil :error nil :search t :wait nil)
               t)
          do (return t)
        finally (return nil)))

;;; Recent repos

(defun gilt-config-dir ()
  "Get the gilt config directory (~/.config/gilt/), creating it if needed."
  (let ((dir (merge-pathnames ".config/gilt/" (user-homedir-pathname))))
    (ensure-directories-exist dir)
    dir))

(defun recent-repos-file ()
  "Path to the recent repos file."
  (merge-pathnames "recent-repos.txt" (gilt-config-dir)))

(defun load-recent-repos ()
  "Load list of recent repo paths from config file."
  (let ((file (recent-repos-file)))
    (if (probe-file file)
        (with-open-file (s file :direction :input)
          (loop for line = (read-line s nil nil)
                while line
                when (> (length (string-trim '(#\Space #\Tab) line)) 0)
                  collect (string-trim '(#\Space #\Tab) line)))
        nil)))

(defun save-recent-repo (path)
  "Add PATH to the recent repos list (max 10, most recent first)."
  (let* ((clean-path (string-right-trim "/" (namestring path)))
         (existing (load-recent-repos))
         (filtered (remove clean-path existing :test #'string=))
         (new-list (subseq (cons clean-path filtered) 0 (min 10 (1+ (length filtered))))))
    (with-open-file (s (recent-repos-file) :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (dolist (r new-list)
        (format s "~A~%" r)))
    new-list))

;;; Create pull request

(defun create-pull-request (branch)
  "Open the create PR page in browser for BRANCH."
  (let ((base-url (git-remote-url-for-browser)))
    (when base-url
      (let ((url (format nil "~A/compare/~A?expand=1" base-url branch)))
        (open-in-browser url)
        url))))

;;; Git-flow

(defun git-flow-init ()
  "Initialize git-flow in the repository."
  (git-run "flow" "init" "-d"))

(defun git-flow-feature-start (name)
  "Start a new git-flow feature branch."
  (git-run "flow" "feature" "start" name))

(defun git-flow-feature-finish (name)
  "Finish a git-flow feature branch."
  (git-run "flow" "feature" "finish" name))

(defun git-flow-release-start (version)
  "Start a new git-flow release."
  (git-run "flow" "release" "start" version))

(defun git-flow-release-finish (version)
  "Finish a git-flow release."
  (git-run "flow" "release" "finish" version))

(defun git-flow-hotfix-start (name)
  "Start a new git-flow hotfix."
  (git-run "flow" "hotfix" "start" name))

(defun git-flow-hotfix-finish (name)
  "Finish a git-flow hotfix."
  (git-run "flow" "hotfix" "finish" name))

;;; Custom command keybindings

(defun custom-commands-file ()
  "Path to the custom commands config file."
  (merge-pathnames "commands.conf" (gilt-config-dir)))

(defun load-custom-commands ()
  "Load custom commands from config file. Format: key=command per line.
Returns alist of (char . command-string)."
  (let ((file (custom-commands-file)))
    (if (probe-file file)
        (with-open-file (s file :direction :input)
          (loop for line = (read-line s nil nil)
                while line
                for trimmed = (string-trim '(#\Space #\Tab) line)
                when (and (> (length trimmed) 0)
                          (char/= (char trimmed 0) #\#)
                          (position #\= trimmed))
                  collect (let ((pos (position #\= trimmed)))
                            (cons (char trimmed 0)
                                  (string-trim '(#\Space #\Tab)
                                               (subseq trimmed (1+ pos)))))))
        nil)))

;;; Configurable keybindings

(defun keybindings-file ()
  "Path to the keybindings config file."
  (merge-pathnames "keybindings.conf" (gilt-config-dir)))

(defun load-keybindings ()
  "Load keybindings from config file.
Format: [context] key=action per line, or key=action for global.
Contexts: global, files, branches, commits, tags, stashes.
Returns nested alist: ((context . ((key . action) ...)) ...)."
  (let ((file (keybindings-file))
        (result nil)
        (current-context :global))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (loop for line = (read-line s nil nil)
              while line
              for trimmed = (string-trim '(#\Space #\Tab) line) do
                (cond
                  ;; Skip empty/comment lines
                  ((or (string= trimmed "")
                       (char= (char trimmed 0) #\#)) nil)
                  ;; Context header: [context]
                  ((and (> (length trimmed) 2)
                        (char= (char trimmed 0) #\[)
                        (char= (char trimmed (1- (length trimmed))) #\]))
                   (setf current-context
                         (intern (string-upcase
                                  (string-trim '(#\[ #\])
                                               trimmed))
                                 :keyword)))
                  ;; Key=action line
                  ((position #\= trimmed)
                   (let* ((pos (position #\= trimmed))
                          (key-str (string-trim '(#\Space #\Tab)
                                                (subseq trimmed 0 pos)))
                          (action-str (string-trim '(#\Space #\Tab)
                                                   (subseq trimmed (1+ pos))))
                          (key-char (if (> (length key-str) 0)
                                        (char key-str 0)
                                        nil)))
                     (when key-char
                       (let* ((ctx-entry (assoc current-context result))
                              (ctx-bindings (if ctx-entry (cdr ctx-entry) nil)))
                         (setf ctx-bindings
                               (acons key-char action-str
                                      (remove key-char ctx-bindings :key #'car)))
                         (setf result
                               (if ctx-entry
                                   (acons current-context ctx-bindings
                                          (remove current-context result :key #'car))
                                   (acons current-context ctx-bindings result)))))))))))
    result))

(defun lookup-keybinding (context key-char)
  "Look up a keybinding for a given context and key character.
   Checks context-specific bindings first, then global.
   Returns the action string or nil."
  (let ((bindings (load-keybindings)))
    (or (let ((ctx-entry (assoc context bindings)))
          (when ctx-entry
            (cdr (assoc key-char (cdr ctx-entry)))))
        (let ((global-entry (assoc :global bindings)))
          (when global-entry
            (cdr (assoc key-char (cdr global-entry))))))))

;;; Commit message templates

(defun commit-templates-file ()
  "Path to the commit templates config file."
  (merge-pathnames "commit-templates.conf" (gilt-config-dir)))

(defun load-commit-templates ()
  "Load commit message templates from config file.
   Format: pattern=prefix per line.
   Pattern is matched against branch name (substring match).
   Returns alist of (pattern . prefix)."
  (let ((file (commit-templates-file)))
    (if (probe-file file)
        (with-open-file (s file :direction :input)
          (loop for line = (read-line s nil nil)
                while line
                for trimmed = (string-trim '(#\Space #\Tab) line)
                when (and (> (length trimmed) 0)
                          (char/= (char trimmed 0) #\#)
                          (position #\= trimmed))
                  collect (let ((pos (position #\= trimmed)))
                            (cons (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos))
                                  (string-trim '(#\Space #\Tab) (subseq trimmed (1+ pos)))))))
        nil)))

(defun get-commit-template (branch)
  "Get the commit message prefix for a branch, based on templates config.
   Returns the prefix string or nil if no template matches."
  (let ((templates (load-commit-templates)))
    (loop for (pattern . prefix) in templates
          when (search pattern branch)
            do (return prefix))))

;;; Custom patch building

(defun git-diff-lines (file)
  "Get the raw diff output for FILE as a list of lines."
  (let ((output (if file
                    (git-run "diff" "--" file)
                    (git-run "diff"))))
    (when (and output (> (length output) 0))
      (cl-ppcre:split "\\n" output))))

(defun git-apply-patch (patch-text)
  "Apply a patch from PATCH-TEXT string."
  (let* ((repo (ensure-repo))
         (dir (repo-path-dir repo)))
    (let ((proc (sb-ext:run-program "git" (list "apply" "--cached" "-")
                                    :input :stream
                                    :output :stream
                                    :error :stream
                                    :directory dir
                                    :search t
                                    :wait nil)))
      (write-string patch-text (sb-ext:process-input proc))
      (close (sb-ext:process-input proc))
      (sb-ext:process-wait proc)
      (let ((exit (sb-ext:process-exit-code proc))
            (err (let ((s (make-string-output-stream)))
                   (loop for c = (read-char (sb-ext:process-error proc) nil nil)
                         while c do (write-char c s))
                   (get-output-stream-string s))))
        (if (zerop exit)
            "Patch applied successfully"
            (format nil "Patch failed: ~A" (string-trim '(#\Newline) err)))))))

;;; Update checker
;;; Check GitHub releases for newer versions of Gilt.

(defparameter *github-repo* "parenworks/gilt"
  "GitHub repository for update checking.")

(defun check-for-updates (current-version)
  "Check GitHub for newer releases. Returns (values newer-version-p latest-version)
   or (values nil nil) on error. Uses curl to avoid extra dependencies."
  (handler-case
      (let* ((url (format nil "https://api.github.com/repos/~A/releases/latest" *github-repo*))
             (proc (sb-ext:run-program "curl"
                                       (list "-s" "-m" "5"
                                             "-H" "Accept: application/vnd.github.v3+json"
                                             url)
                                       :output :stream
                                       :error nil
                                       :search t
                                       :wait t))
             (output (with-output-to-string (s)
                       (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                             while line do (write-line line s))))
             (exit (sb-ext:process-exit-code proc)))
        (when (zerop exit)
          ;; Extract tag_name from JSON (simple pattern match, no JSON library needed)
          (let ((tag-pos (search "\"tag_name\"" output)))
            (when tag-pos
              (let* ((colon-pos (position #\: output :start tag-pos))
                     (quote1 (position #\" output :start (1+ colon-pos)))
                     (quote2 (position #\" output :start (1+ quote1))))
                (when (and quote1 quote2)
                  (let ((tag (subseq output (1+ quote1) quote2)))
                    ;; Strip leading 'v' if present
                    (let ((latest (if (and (> (length tag) 0) (char= (char tag 0) #\v))
                                      (subseq tag 1)
                                      tag)))
                      (values (string/= latest current-version)
                              latest)))))))))
    (error () (values nil nil))))

;;; Rename similarity threshold
;;; (definition moved to top of file with other parameters)
