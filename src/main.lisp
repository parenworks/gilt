(in-package #:gilt)

;;; Main Application Loop
;;; LazyGit-style Git TUI

(defparameter *current-view* nil)
(defparameter *views* nil)

(defun init-views ()
  "Initialize all views"
  (setf *views* (make-hash-table :test 'eq))
  (setf (gethash :status *views*) (make-instance 'status-view))
  (setf (gethash :log *views*) (make-instance 'log-view))
  (setf (gethash :branches *views*) (make-instance 'branches-view))
  (setf *current-view* (gethash :status *views*)))

(defun switch-view (view-key)
  "Switch to a different view"
  (let ((view (gethash view-key *views*)))
    (when view
      (setf *current-view* view)
      (refresh-data view))))

(defun draw-header (width)
  "Draw the header bar with repo info"
  (cursor-to 1 1)
  (bg (color-code :bright-blue))
  (fg (color-code :bright-white))
  (bold)
  (let* ((repo-name (git-repo-name))
         (branch (git-current-branch))
         (header (format nil " Gilt │ ~A │ ~A " repo-name branch)))
    (write-string header *terminal-io*)
    (loop repeat (- width (length header)) do (write-char #\Space *terminal-io*)))
  (reset)
  (finish-output *terminal-io*))

(defun render (width height &optional dialog-only)
  "Render the current view"
  (unless dialog-only
    (clear-screen)
    (draw-header width))
  ;; Adjust for header
  (draw-view *current-view* width (1- height)))

(defun main-loop ()
  "Main event loop"
  (let ((size (terminal-size)))
    (when (null size)
      (setf size '(80 24)))
    (let ((width (first size))
          (height (second size)))
      (render width height)
      (finish-output *terminal-io*)
      (loop
        (let* ((key (read-key))
               (result (handle-key *current-view* key)))
          (cond
            ((eq result :quit)
             (return))
            ((eq result :status)
             (switch-view :status))
            ((eq result :log)
             (switch-view :log))
            ((eq result :branches)
             (switch-view :branches)))
          ;; Check for terminal resize
          (let ((new-size (terminal-size)))
            (when new-size
              (setf width (first new-size)
                    height (second new-size))))
          ;; Re-render only if not just dialog update
          (unless (eq result :dialog)
            (render width height)
            (finish-output *terminal-io*)))))))

(defun run ()
  "Entry point - run Gilt"
  (with-raw-terminal
    (init-views)
    (main-loop)))

(defun main ()
  "Main entry point for executable"
  (handler-case
      (run)
    (error (e)
      ;; Restore terminal
      (format t "~C[?25h~C[0m" (code-char 27) (code-char 27))
      (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon") :input t :output nil :error nil)
      (format t "~%Error: ~A~%" e)))
  (sb-ext:exit :code 0))
