(in-package #:gilt.terminal)

;;; Raw Terminal Input/Output - CLOS-based
;;; Handles putting terminal in raw mode and reading key events

;;; Key event class

(defclass key-event ()
  ((char :initarg :char :accessor key-char :initform nil
         :documentation "Character if printable key")
   (code :initarg :code :accessor key-code :initform nil
         :documentation "Keyword for special keys")
   (ctrl-p :initarg :ctrl-p :accessor key-ctrl-p :initform nil
           :documentation "Control modifier pressed")
   (alt-p :initarg :alt-p :accessor key-alt-p :initform nil
          :documentation "Alt modifier pressed"))
  (:documentation "Represents a keyboard input event"))

(defmethod print-object ((key key-event) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~@[char=~S~]~@[ code=~S~]~@[ ctrl~]~@[ alt~]"
            (key-char key) (key-code key)
            (key-ctrl-p key) (key-alt-p key))))

(defun make-key-event (&key char code ctrl-p alt-p)
  (make-instance 'key-event :char char :code code :ctrl-p ctrl-p :alt-p alt-p))

;; Aliases for compatibility
(defun key-event-char (key) (key-char key))
(defun key-event-code (key) (key-code key))
(defun key-event-ctrl-p (key) (key-ctrl-p key))
(defun key-event-alt-p (key) (key-alt-p key))

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
    (sb-ext:run-program "/bin/stty" '("-echo" "raw" "-icanon")
                        :input t
                        :output nil
                        :error nil)
    (setf (terminal-raw-p mode) t)))

(defmethod disable-raw-mode ((mode terminal-mode))
  (when (terminal-raw-p mode)
    (sb-ext:run-program "/bin/stty" '("echo" "-raw" "icanon")
                        :input t
                        :output nil
                        :error nil)
    (setf (terminal-raw-p mode) nil)))

(defmethod query-size ((mode terminal-mode))
  (declare (ignore mode))
  (let* ((size-str (with-output-to-string (s)
                     (sb-ext:run-program "/bin/stty" '("size")
                                         :input t
                                         :output s
                                         :error nil)))
         (parts (cl-ppcre:split "\\s+" (string-trim '(#\Newline #\Space) size-str))))
    (when (= (length parts) 2)
      (list (parse-integer (second parts))   ; width (cols)
            (parse-integer (first parts)))))) ; height (rows)

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

;;; Input reader class - reads from /dev/tty

(defclass input-reader ()
  ((stream :initarg :stream :accessor reader-stream :initform nil)
   (tty-path :initarg :tty-path :accessor reader-tty-path :initform "/dev/tty"))
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
                :if-does-not-exist :error)))
  reader)

(defmethod reader-close ((reader input-reader))
  (when (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (close (reader-stream reader))
    (setf (reader-stream reader) nil))
  reader)

(defun read-byte-nonblocking (stream)
  "Try to read a byte without blocking. Returns byte or nil."
  (when (listen stream)
    (read-byte stream nil nil)))

(defmethod read-key-event ((reader input-reader))
  "Read a key event from the TTY"
  (let* ((stream (reader-stream reader))
         (byte (read-byte stream nil nil)))
    (unless byte
      (return-from read-key-event (make-key-event :char #\q)))
    (cond
      ;; Escape sequence or bare escape
      ((= byte 27)
       ;; Small delay then check for more bytes (escape sequences)
       (sleep 0.02)
       (let ((next (read-byte-nonblocking stream)))
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
         ((= byte 9) (make-key-event :code +key-tab+))
         ((= byte 127) (make-key-event :code +key-backspace+))
         ((= byte 8) (make-key-event :code +key-backspace+))  ; Some terminals send 8 for backspace
         (t (make-key-event :char (code-char (+ byte 96)) :ctrl-p t))))
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
  "Read a key event from terminal. Blocks until key pressed."
  (reader-open *input-reader*)
  (read-key-event *input-reader*))
