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

(require :sb-posix)

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
        (let* ((fd (sb-sys:fd-stream-fd sb-sys:*stdin*))
               (orig (sb-posix:tcgetattr fd))
               (raw (sb-posix:tcgetattr fd)))
          ;; Save original for restore
          (setf (terminal-original-settings mode) orig)
          ;; Modify flags for raw mode
          (setf (sb-posix:termios-iflag raw)
                (logand (sb-posix:termios-iflag raw)
                        (lognot (logior sb-posix:brkint sb-posix:icrnl
                                       sb-posix:inpck sb-posix:istrip sb-posix:ixon))))
          (setf (sb-posix:termios-oflag raw)
                (logand (sb-posix:termios-oflag raw)
                        (lognot sb-posix:opost)))
          (setf (sb-posix:termios-cflag raw)
                (logior (sb-posix:termios-cflag raw) sb-posix:cs8))
          (setf (sb-posix:termios-lflag raw)
                (logand (sb-posix:termios-lflag raw)
                        (lognot (logior sb-posix:echo sb-posix:icanon
                                       sb-posix:iexten sb-posix:isig))))
          ;; VMIN=1 VTIME=0
          (let ((cc (sb-posix:termios-cc raw)))
            (setf (aref cc sb-posix:vmin) 1)
            (setf (aref cc sb-posix:vtime) 0))
          (sb-posix:tcsetattr fd sb-posix:tcsaflush raw))
      (error (e)
        (warn "Failed to enable raw mode: ~A" e)))
    (setf (terminal-raw-p mode) t)))

(defmethod disable-raw-mode ((mode terminal-mode))
  (when (terminal-raw-p mode)
    (handler-case
        (let ((saved (terminal-original-settings mode)))
          (when saved
            (sb-posix:tcsetattr (sb-sys:fd-stream-fd sb-sys:*stdin*)
                                sb-posix:tcsaflush saved)))
      (error (e)
        (warn "Failed to disable raw mode: ~A" e)))
    (setf (terminal-raw-p mode) nil)))

(defmethod query-size ((mode terminal-mode))
  (declare (ignore mode))
  (handler-case
      (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 8) 8)))
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "ioctl"
                                (function sb-alien:int sb-alien:int
                                          sb-alien:unsigned-long (* t)))
         (sb-sys:fd-stream-fd sb-sys:*stdin*)
         #x5413
         (sb-alien:addr (sb-alien:deref buf 0)))
        (let ((rows (logior (sb-alien:deref buf 0) (ash (sb-alien:deref buf 1) 8)))
              (cols (logior (sb-alien:deref buf 2) (ash (sb-alien:deref buf 3) 8))))
          (when (and (> rows 0) (> cols 0))
            (list cols rows))))
    (error (e)
      (warn "Failed to query terminal size: ~A" e)
      '(80 24))))

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

(defun read-byte-from-fd (fd)
  "Read a single byte directly from file descriptor, bypassing SBCL stream layer.
   Returns the byte or nil if no data available (EAGAIN)."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (let ((n (sb-unix:unix-read fd (sb-sys:vector-sap buf) 1)))
      (cond
        ((and n (= n 1)) (aref buf 0))
        (t nil)))))

(defun wait-for-escape-sequence (stream timeout)
  "Wait for escape sequence bytes with dynamic timeout.
   Returns the next byte if available within timeout, or nil for bare Escape.
   Uses raw fd read to avoid SBCL stream buffering issues."
  (let ((fd (sb-sys:fd-stream-fd stream))
        (start (get-internal-real-time))
        (timeout-ticks (* timeout internal-time-units-per-second)))
    (loop
      (let ((byte (read-byte-from-fd fd)))
        (when byte (return byte)))
      (when (>= (- (get-internal-real-time) start) timeout-ticks)
        (return nil))
      (sleep 0.001))))

(defmethod read-key-event ((reader input-reader))
  "Read a key event from the TTY.
   Uses raw fd reads throughout to avoid SBCL stream buffering issues."
  (let* ((stream (reader-stream reader))
         (fd (sb-sys:fd-stream-fd stream))
         (byte (read-byte-from-fd fd)))
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
                (setf final-byte (read-byte-from-fd fd))
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
