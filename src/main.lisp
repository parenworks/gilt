(in-package #:gilt)

;;; Main Application Loop
;;; LazyGit-style Git TUI

;;; Application class - encapsulates all application state

(defclass application ()
  ((views :initarg :views :accessor app-views :initform (make-hash-table :test 'eq))
   (current-view :initarg :current-view :accessor app-current-view :initform nil)
   (width :initarg :width :accessor app-width :initform 80)
   (height :initarg :height :accessor app-height :initform 24)
   (running-p :initarg :running-p :accessor app-running-p :initform nil))
  (:documentation "Main Gilt application state"))

(defmethod print-object ((app application) stream)
  (print-unreadable-object (app stream :type t)
    (format stream "~Dx~D ~:[stopped~;running~]"
            (app-width app) (app-height app) (app-running-p app))))

(defgeneric app-init (app)
  (:documentation "Initialize the application"))

(defgeneric app-switch-view (app view-key)
  (:documentation "Switch to a different view"))

(defgeneric app-render (app &optional dialog-only)
  (:documentation "Render the current view"))

(defgeneric app-handle-key (app key)
  (:documentation "Handle a key event, return t to continue, nil to quit"))

(defgeneric app-run (app)
  (:documentation "Run the main event loop"))

(defmethod app-init ((app application))
  (setf (gethash :status (app-views app)) (make-instance 'status-view))
  (setf (gethash :log (app-views app)) (make-instance 'log-view))
  (setf (gethash :branches (app-views app)) (make-instance 'branches-view))
  (setf (app-current-view app) (gethash :status (app-views app)))
  (let ((size (terminal-size)))
    (when size
      (setf (app-width app) (first size)
            (app-height app) (second size))))
  app)

(defmethod app-switch-view ((app application) view-key)
  (let ((view (gethash view-key (app-views app))))
    (when view
      (setf (app-current-view app) view)
      (refresh-data view))))

(defmethod app-render ((app application) &optional dialog-only)
  (unless dialog-only
    (clear-screen)
    (draw-header (app-width app)))
  (draw-view (app-current-view app) (app-width app) (1- (app-height app)))
  (finish-output *terminal-io*))

(defmethod app-handle-key ((app application) key)
  (let ((result (handle-key (app-current-view app) key)))
    (cond
      ((eq result :quit) nil)
      ((eq result :status) (app-switch-view app :status) t)
      ((eq result :log) (app-switch-view app :log) t)
      ((eq result :branches) (app-switch-view app :branches) t)
      (t t))))

(defmethod app-run ((app application))
  (setf (app-running-p app) t)
  (app-render app)
  (loop while (app-running-p app) do
    (let* ((key (read-key))
           (continue-p (app-handle-key app key)))
      (unless continue-p
        (setf (app-running-p app) nil)
        (return))
      ;; Check for terminal resize
      (let ((new-size (terminal-size)))
        (when new-size
          (setf (app-width app) (first new-size)
                (app-height app) (second new-size))))
      ;; Re-render
      (app-render app))))

;;; Global application instance
(defparameter *app* nil "The current Gilt application instance")

;;; Legacy compatibility - delegate to *app*
(defun current-view ()
  (when *app* (app-current-view *app*)))

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

(defun run ()
  "Entry point - run Gilt"
  (with-raw-terminal
    (setf *app* (make-instance 'application))
    (app-init *app*)
    (app-run *app*)))

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
