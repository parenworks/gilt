(in-package #:gilt.terminal)

;;; Raw Terminal Input/Output - CLOS-based
;;; Handles putting terminal in raw mode and reading key events
;;; Cross-platform Unix support including NixOS

;;; Key event class

(defclass key-event ()
  ((char :initarg :char :accessor key-event-char :initform nil
         :documentation "Character if printable key")
   (code :initarg :code :accessor key-event-code :initform nil
         :documentation "Keyword for special keys")
   (ctrl-p :initarg :ctrl-p :accessor key-event-ctrl-p :initform nil
           :documentation "Control modifier pressed")
   (alt-p :initarg :alt-p :accessor key-event-alt-p :initform nil
          :documentation "Alt modifier pressed"))
  (:documentation "Represents a keyboard input event"))

(defmethod print-object ((key key-event) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~@[char=~S~]~@[ code=~S~]~@[ ctrl~]~@[ alt~]"
            (key-event-char key) (key-event-code key)
            (key-event-ctrl-p key) (key-event-alt-p key))))

(defun make-key-event (&key char code ctrl-p alt-p)
  (make-instance 'key-event :char char :code code :ctrl-p ctrl-p :alt-p alt-p))

;;; Special key codes

(defconstant +key-up+ :up)
(defconstant +key-down+ :down)
(defconstant +key-left+ :left)
(defconstant +key-right+ :right)
(defconstant +key-enter+ :enter)
(defconstant +key-escape+ :escape)
(defconstant +key-tab+ :tab)
(defconstant +key-backspace+ :backspace)
(defconstant +key-delete+ :delete)
(defconstant +key-home+ :home)
(defconstant +key-end+ :end)
(defconstant +key-page-up+ :page-up)
(defconstant +key-page-down+ :page-down)

;;; Environment configuration (must be defined before methods that use them)

(defparameter *stty-path* 
  (or (sb-ext:posix-getenv "GILT_STTY_PATH") 
      (ignore-errors (probe-file "/run/current-system/sw/bin/stty"))
      (ignore-errors (probe-file "/bin/stty"))
      (ignore-errors (probe-file "/usr/bin/stty"))
      (ignore-errors (probe-file "/usr/local/bin/stty"))
      "/bin/stty"))

(defparameter *tty-path*
  (or (sb-ext:posix-getenv "GILT_TTY_PATH")
      (let ((candidates '("/dev/tty" "/dev/pts/0" "/dev/console" "/dev/tty0")))
        (loop for path in candidates
              when (ignore-errors (open path :direction :input :if-does-not-exist nil))
              return path
              finally (return "/dev/tty")))))

(defparameter *escape-timeout*
  (or (ignore-errors 
        (let ((timeout-str (sb-ext:posix-getenv "GILT_ESCAPE_TIMEOUT")))
          (when timeout-str 
            (let ((parsed (read-from-string timeout-str)))
              (if (numberp parsed) parsed nil)))))
      (let ((term (sb-ext:posix-getenv "TERM"))
            (alacritty-socket (sb-ext:posix-getenv "ALACRITTY_SOCKET")))
        (cond
          (alacritty-socket 0.01)
          ((and term (search "alacritty" term)) 0.01)
          (t 0.02)))))

;;; Terminal mode controller class

(defclass terminal-mode ()
  ((raw-p :initarg :raw-p :accessor terminal-raw-p :initform nil)
   (original-settings :accessor terminal-original-settings :initform nil))
  (:documentation "Manages terminal mode state"))

(defgeneric enable-raw-mode (mode)
  (:documentation "Put terminal in raw mode"))

(defgeneric disable-raw-mode (mode)
  (:documentation "Restore terminal to normal mode"))

(defgeneric query-size (mode)
  (:documentation "Query terminal dimensions"))

(defmethod enable-raw-mode ((mode terminal-mode))
  (unless (terminal-raw-p mode)
    (handler-case
        (sb-ext:run-program *stty-path* '("-echo" "raw" "-icanon")
                            :input t
                            :output nil
                            :error nil)
      (error (e)
        ;; Fallback: try to use shell built-in stty
        (handler-case
            (sb-ext:run-program "sh" (list "-c" "stty -echo raw -icanon")
                                :input t :output nil :error nil)
          (error (e2)
            (warn "Failed to enable raw mode with ~A: ~A" *stty-path* e)
            (warn "Fallback with shell stty also failed: ~A" e2)))))
    (setf (terminal-raw-p mode) t)))

(defmethod disable-raw-mode ((mode terminal-mode))
  (when (terminal-raw-p mode)
    (handler-case
        (sb-ext:run-program *stty-path* '("echo" "-raw" "icanon")
                            :input t
                            :output nil
                            :error nil)
      (error (e)
        ;; Fallback: try to use shell built-in stty
        (handler-case
            (sb-ext:run-program "sh" (list "-c" "stty echo -raw icanon")
                                :input t :output nil :error nil)
          (error (e2)
            (declare (ignore e2))
            (warn "Failed to disable raw mode with ~A: ~A" *stty-path* e)))))
    (setf (terminal-raw-p mode) nil)))

(defmethod query-size ((mode terminal-mode))
  (declare (ignore mode))
  (handler-case
      (let* ((size-str (with-output-to-string (s)
                         (sb-ext:run-program *stty-path* '("size")
                                             :input t
                                             :output s
                                             :error nil)))
             (parts (cl-ppcre:split "\\s+" (string-trim '(#\Newline #\Space) size-str))))
        (when (= (length parts) 2)
          (list (parse-integer (second parts))   ; width (cols)
                (parse-integer (first parts))))); height (rows)
    (error (e)
      (warn "Failed to query terminal size with ~A: ~A" *stty-path* e)
      ;; Return default size
      '(80 24))))

;;; Cross-platform utility functions

(defun find-stty ()
  "Find stty command across different Unix systems"
  (or
   ;; Check common NixOS locations first
   (ignore-errors (probe-file "/run/current-system/sw/bin/stty"))
   ;; Check standard locations
   (ignore-errors (probe-file "/bin/stty"))
   (ignore-errors (probe-file "/usr/bin/stty"))
   (ignore-errors (probe-file "/usr/local/bin/stty"))
   ;; Fallback to PATH search
   (handler-case
       (let ((result (sb-ext:run-program "which" '("stty") :output :string)))
         (when (and result (> (length result) 0))
           (string-trim '(#\Newline #\Space) result)))
     (error nil))
   ;; Final fallback
   "/bin/stty"))

(defun find-tty ()
  "Find available TTY device across different systems"
  (let ((candidates '("/dev/tty" "/dev/pts/0" "/dev/console" "/dev/tty0")))
    (loop for path in candidates
          when (ignore-errors (open path :direction :input :if-does-not-exist nil))
          return path
          finally (return "/dev/tty"))))

(defun detect-terminal-type ()
  "Detect terminal emulator and return optimization settings"
  (let ((term (sb-ext:posix-getenv "TERM"))
        (alacritty-socket (sb-ext:posix-getenv "ALACRITTY_SOCKET")))
    (cond
      (alacritty-socket '(:escape-timeout 0.01 :fast-input t))
      ((and term (search "alacritty" term)) '(:escape-timeout 0.01 :fast-input t))
      ((and term (search "xterm" term)) '(:escape-timeout 0.02 :fast-input nil))
      (t '(:escape-timeout 0.02 :fast-input nil)))))

;;; Global terminal mode instance

(defparameter *terminal-mode* (make-instance 'terminal-mode))

;;; Convenience functions

(defun terminal-size ()
  "Return (width height) of terminal"
  (query-size *terminal-mode*))

(defun enter-alternate-screen ()
  "Switch to alternate screen buffer"
  (format t "~C[?1049h" *escape*)
  (force-output))

(defun leave-alternate-screen ()
  "Switch back to main screen buffer"
  (format t "~C[?1049l" *escape*)
  (force-output))

(defmacro with-raw-terminal (&body body)
  "Execute body with terminal in raw mode, ensuring cleanup"
  `(progn
     (enter-alternate-screen)
     (enable-raw-mode *terminal-mode*)
     (cursor-hide)
     (unwind-protect
          (progn ,@body)
       (close-tty-stream)
       (cursor-show)
       (disable-raw-mode *terminal-mode*)
       (leave-alternate-screen)
       (reset))))

(defun setup-terminal ()
  "Enter raw mode and prepare terminal for TUI"
  (enable-raw-mode *terminal-mode*)
  (cursor-hide))

(defun restore-terminal ()
  "Temporarily restore terminal to normal mode (for spawning external programs)"
  (cursor-show)
  (disable-raw-mode *terminal-mode*))

;;; Input reader class - reads from /dev/tty

(defclass input-reader ()
  ((stream :initarg :stream :accessor reader-stream :initform nil)
   (tty-path :initarg :tty-path :accessor reader-tty-path :initform *tty-path*))
  (:documentation "Reads and parses keyboard input from TTY"))

(defmethod print-object ((reader input-reader) stream)
  (print-unreadable-object (reader stream :type t)
    (format stream "~A ~:[closed~;open~]"
            (reader-tty-path reader)
            (and (reader-stream reader) (open-stream-p (reader-stream reader))))))

(defgeneric reader-open (reader)
  (:documentation "Open the TTY stream for reading"))

(defgeneric reader-close (reader)
  (:documentation "Close the TTY stream"))

(defgeneric read-key-event (reader)
  (:documentation "Read a key event from the input stream"))

(defmethod reader-open ((reader input-reader))
  (unless (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (setf (reader-stream reader)
          (open (reader-tty-path reader)
                :direction :input
                :element-type '(unsigned-byte 8)
                :if-does-not-exist :error))
    ;; Set non-blocking mode on the file descriptor
    (let ((fd (sb-sys:fd-stream-fd (reader-stream reader))))
      (sb-posix:fcntl fd sb-posix:f-setfl 
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                              sb-posix:o-nonblock))))
  reader)

(defmethod reader-close ((reader input-reader))
  (when (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (close (reader-stream reader))
    (setf (reader-stream reader) nil))
  reader)

(defun read-byte-nonblocking (stream)
  "Try to read a byte without blocking. Returns byte or nil.
   With O_NONBLOCK set, read-byte returns nil if no data available."
  (handler-case
      (read-byte stream nil nil)
    ;; Handle EAGAIN/EWOULDBLOCK which can happen with non-blocking I/O
    (sb-int:simple-stream-error () nil)))

(defun wait-for-escape-sequence (stream timeout)
  "Wait for escape sequence bytes with dynamic timeout"
  (let ((start (get-internal-real-time))
        (timeout-ticks (* timeout internal-time-units-per-second)))
    (loop while (< (- (get-internal-real-time) start) timeout-ticks)
          do (when (listen stream) (return))
          finally (return (read-byte-nonblocking stream)))))

(defmethod read-key-event ((reader input-reader))
  "Read a key event from the TTY"
  (let* ((stream (reader-stream reader))
         (byte (read-byte-nonblocking stream)))
    (unless byte
      (return-from read-key-event nil))
    (cond
      ;; Escape sequence or bare escape
      ((= byte 27)
       ;; Use dynamic timeout based on terminal type
       (let ((next (wait-for-escape-sequence stream *escape-timeout*)))
         (cond
           ((null next)
            ;; No more bytes - bare Escape key
            (make-key-event :code +key-escape+))
           ((= next 91)
            ;; CSI sequence: ESC [
            (let ((params nil)
                  (final-byte nil))
              (loop
                (setf final-byte (read-byte stream nil nil))
                (unless final-byte (return))
                (cond
                  ((and (>= final-byte 48) (<= final-byte 57))
                   (push (code-char final-byte) params))
                  ((= final-byte 59)
                   (push #\; params))
                  (t (return))))
              (let ((param-str (coerce (nreverse params) 'string)))
                (case final-byte
                  (65 (make-key-event :code +key-up+))
                  (66 (make-key-event :code +key-down+))
                  (67 (make-key-event :code +key-right+))
                  (68 (make-key-event :code +key-left+))
                  (72 (make-key-event :code +key-home+))
                  (70 (make-key-event :code +key-end+))
                  (126
                   (cond
                     ((string= param-str "3") (make-key-event :code +key-delete+))
                     ((string= param-str "5") (make-key-event :code +key-page-up+))
                     ((string= param-str "6") (make-key-event :code +key-page-down+))
                     (t (make-key-event :code :unknown))))
                  (t (make-key-event :code :unknown))))))
           (t
            ;; Alt + key
            (make-key-event :char (code-char next) :alt-p t)))))
      ;; Control characters
      ((< byte 32)
       (cond
         ((= byte 13) (make-key-event :code +key-enter+))
         ((= byte 10) (make-key-event :char #\Newline))  ; Line feed (from paste)
         ((= byte 9) (make-key-event :code +key-tab+))
         ((= byte 8) (make-key-event :code +key-backspace+))  ; Ctrl+H / backspace on some terminals
         (t (make-key-event :char (code-char (+ byte 96)) :ctrl-p t))))
      ;; DEL character (127) - backspace on most terminals
      ((= byte 127)
       (make-key-event :code +key-backspace+))
       ;; Regular character
       (t
        (make-key-event :char (code-char byte))))))

;;; Global input reader instance
(defparameter *input-reader* (make-instance 'input-reader)
  "Global input reader for keyboard events")

(defun close-tty-stream ()
  "Close the TTY stream"
  (reader-close *input-reader*))

(defun read-key ()
  "Read a key event from terminal. Polls until key pressed."
  (reader-open *input-reader*)
  ;; Poll with small sleep since we use non-blocking I/O
  (loop
    (let ((key (read-key-event *input-reader*)))
      (when key (return key)))
    (sleep 0.01)))

(defun read-key-with-timeout (timeout-ms)
  "Try to read a key event with timeout. Returns key-event or nil if timeout."
  (reader-open *input-reader*)
  (let ((start-time (get-internal-real-time))
        (timeout-ticks (* timeout-ms (/ internal-time-units-per-second 1000))))
    ;; Poll for input with timeout
    (loop
      (let ((key (read-key-event *input-reader*)))
        (when key (return key)))
      (when (> (- (get-internal-real-time) start-time) timeout-ticks)
        (return nil))
      (sleep 0.01))))
