(in-package #:gilt)

;;; Main Application Loop
;;; LazyGit-style Git TUI

;;; Version - follows Semantic Versioning (https://semver.org/)
;;; MAJOR.MINOR.PATCH
;;; - MAJOR: incompatible API changes
;;; - MINOR: new functionality (backwards compatible)
;;; - PATCH: bug fixes (backwards compatible)
(defparameter *version* "0.11.0"
  "Gilt version number")

;;; Application class - encapsulates all application state

(defclass application ()
  ((repo :initarg :repo :accessor app-repo :initform nil
         :documentation "The git repository this application is managing")
   (views :initarg :views :accessor app-views :initform (make-hash-table :test 'eq))
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
  ;; Initialize the git repository and set global for convenience functions
  (setf (app-repo app) (gilt.git:ensure-repo))
  ;; All views use main-view - the legacy subclasses were empty
  (setf (gethash :status (app-views app)) (make-instance 'main-view))
  (setf (gethash :log (app-views app)) (make-instance 'main-view))
  (setf (gethash :branches (app-views app)) (make-instance 'main-view))
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
  (begin-sync-update)
  (unless dialog-only
    (clear-screen)
    (draw-header (app-width app)))
  (draw-view (app-current-view app) (app-width app) (1- (app-height app)))
  (end-sync-update)
  (finish-output *terminal-io*))

(defmethod app-handle-key ((app application) key)
  (let ((result (handle-key (app-current-view app) key)))
    (values
     (cond
       ((eq result :quit) nil)
       ((eq result :status) (app-switch-view app :status) t)
       ((eq result :log) (app-switch-view app :log) t)
       ((eq result :branches) (app-switch-view app :branches) t)
       (t t))
     (eq result :dialog))))  ; Return second value indicating dialog-only redraw

(defmethod app-run ((app application))
  (setf (app-running-p app) t)
  (app-render app)
  (loop while (app-running-p app) do
    ;; Check if current view has an active runner - use timeout if so
    (let* ((view (app-current-view app))
           (has-runner (and view 
                            (slot-boundp view 'gilt.views::active-runner)
                            (slot-value view 'gilt.views::active-runner)))
           (key (if has-runner
                    (read-key-with-timeout 100)  ; 100ms timeout for polling
                    (read-key))))
      ;; If timeout (nil key) and runner active, just re-render to show updates
      (when (or key (not has-runner))
        (when key
          (multiple-value-bind (continue-p dialog-only)
              (app-handle-key app key)
            (unless continue-p
              (setf (app-running-p app) nil)
              (return))
            ;; Check for terminal resize
            (let ((new-size (terminal-size)))
              (when new-size
                (setf (app-width app) (first new-size)
                      (app-height app) (second new-size))))
            ;; Re-render - skip full redraw if dialog is handling input
            (unless dialog-only
              (app-render app)))))
      ;; If runner active and no key, still poll and render
      ;; Re-check runner in case it was dismissed by key handler
      (let ((current-runner (and view 
                                  (slot-boundp view 'gilt.views::active-runner)
                                  (slot-value view 'gilt.views::active-runner))))
        (when (and current-runner (null key))
          ;; Poll the runner for updates
          (gilt.pty:runner-poll current-runner)
          ;; Update main panel with output
          (let ((output (gilt.pty:runner-get-output current-runner)))
            (setf (gilt.ui:panel-items (gilt.views::main-panel view))
                  (append output
                          (if (gilt.pty:runner-finished-p current-runner)
                              (list "" 
                                    (format nil "--- Finished (exit code: ~D) ---" 
                                            (gilt.pty:runner-exit-code current-runner))
                                    "Press any key to continue...")
                              (list "" (slot-value view 'gilt.views::runner-title))))))
          (app-render app))))))

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
  (let ((args (uiop:command-line-arguments)))
    (cond
      ;; Version flag
      ((or (member "--version" args :test #'string=)
           (member "-v" args :test #'string=))
       (format t "gilt version ~A~%" *version*)
       (finish-output)
       (sb-ext:exit :code 0))
      ;; Debug flag - run diagnostics
      ((member "--debug" args :test #'string=)
       (format t "~%=== Gilt Debug Mode ===~%~%")
       (format t "SBCL: ~A~%" (lisp-implementation-version))
       (format t "TERM: ~A~%" (sb-ext:posix-getenv "TERM"))
       (format t "~%Checking /dev/tty...~%")
       (handler-case
           (let ((tty (open "/dev/tty" :direction :input :element-type '(unsigned-byte 8))))
             (format t "  /dev/tty: OK~%")
             (close tty))
         (error (e)
           (format t "  /dev/tty: FAILED - ~A~%" e)))
       (format t "~%Checking stty...~%")
       (handler-case
           (let ((output (with-output-to-string (s)
                           (sb-ext:run-program "/bin/stty" '("size")
                                               :input t :output s :error nil))))
             (format t "  stty size: ~A" output))
         (error (e)
           (format t "  stty: FAILED - ~A~%" e)))
       (format t "~%Testing raw mode...~%")
       (handler-case
           (progn
             (sb-ext:run-program "/bin/stty" '("-echo" "raw" "-icanon")
                                 :input t :output nil :error nil)
             (format t "  raw mode: OK~%")
             (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                                 :input t :output nil :error nil))
         (error (e)
           (format t "  raw mode: FAILED - ~A~%" e)))
       (format t "~%Press any key within 3 seconds to test input...~%")
       (finish-output)
       (handler-case
           (progn
             (sb-ext:run-program "/bin/stty" '("-echo" "raw" "-icanon")
                                 :input t :output nil :error nil)
             (let* ((tty (open "/dev/tty" :direction :input :element-type '(unsigned-byte 8)))
                    (fd (sb-sys:fd-stream-fd tty))
                    (start (get-internal-real-time))
                    (timeout (* 3 internal-time-units-per-second))
                    (got-input nil))
               ;; Set non-blocking mode
               (sb-posix:fcntl fd sb-posix:f-setfl 
                               (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                                       sb-posix:o-nonblock))
               (loop while (< (- (get-internal-real-time) start) timeout)
                     do (handler-case
                            (let ((byte (read-byte tty nil nil)))
                              (when byte
                                (setf got-input t)
                                (return)))
                          (sb-int:simple-stream-error () nil))
                        (sleep 0.01))
               (close tty)
               (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                                   :input t :output nil :error nil)
               (if got-input
                   (format t "  input: OK~%")
                   (format t "  input: TIMEOUT (no key pressed or input not working)~%"))))
         (error (e)
           (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                               :input t :output nil :error nil)
           (format t "  input: FAILED - ~A~%" e)))
       (format t "~%=== Debug Complete ===~%")
       (sb-ext:exit :code 0))
      ;; Normal run
      (t
       (handler-case
           (run)
         (error (e)
           ;; Restore terminal
           (format t "~C[?25h~C[0m" (code-char 27) (code-char 27))
           (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon") :input t :output nil :error nil)
           (format t "~%Error: ~A~%" e)))
       (sb-ext:exit :code 0)))))
