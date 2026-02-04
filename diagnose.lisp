;;; Gilt Diagnostic Script
;;; Run with: sbcl --load diagnose.lisp
;;;
;;; This helps diagnose why gilt might freeze on startup
;;; Cross-platform Unix support including NixOS

(format t "~%=== Gilt Diagnostic Tool ===~%~%")

;; Basic cross-platform functions for fallback mode
(defun basic-find-stty ()
  "Basic stty finder"
  (or (ignore-errors (probe-file "/run/current-system/sw/bin/stty"))
      (ignore-errors (probe-file "/bin/stty"))
      (ignore-errors (probe-file "/usr/bin/stty"))
      "/bin/stty"))

(defun basic-find-tty ()
  "Basic tty finder"
  (or (ignore-errors (open "/dev/tty" :direction :input :if-does-not-exist nil))
      (ignore-errors (open "/dev/pts/0" :direction :input :if-does-not-exist nil))
      "/dev/tty"))

;; Load gilt system and dependencies
(handler-case
    (progn
      ;; Try to load the full system first
      (load "gilt.asd")
      (asdf:load-system :gilt))
  (error (e)
    (format t "Warning: Could not load full gilt system: ~A~%" e)
    (format t "Falling back to basic diagnostic mode...~%~%")))

;; Check 1: SBCL version
(format t "1. SBCL Version: ~A~%" (lisp-implementation-version))

;; Check 2: System detection
(format t "~%2. System Detection:~%")
(handler-case
    (let ((stty-path (gilt.terminal:find-stty))
          (tty-path (gilt.terminal:find-tty))
          (terminal-props (gilt.terminal:detect-terminal-type)))
      (format t "   stty path: ~A~%" stty-path)
      (format t "   tty path: ~A~%" tty-path)
      (format t "   terminal properties: ~A~%" terminal-props))
  (error (e)
    (format t "   Could not detect system (gilt not loaded): ~A~%" e)
    ;; Fallback to basic detection
    (let ((stty-path (basic-find-stty))
          (tty-path (basic-find-tty)))
      (format t "   Basic stty path: ~A~%" stty-path)
      (format t "   Basic tty path: ~A~%" tty-path))))

;; Check 3: TTY access
(format t "~%3. Checking TTY access...~%")
(handler-case
    (let ((tty-path (basic-find-tty)))
      (let ((tty (open tty-path :direction :input :element-type '(unsigned-byte 8))))
        (format t "   OK: ~A opened successfully~%" tty-path)
        (close tty)))
  (error (e)
    (format t "   ERROR: Cannot open TTY: ~A~%" e)
    (format t "   This is likely the cause of the freeze!~%")))

;; Check 4: stty command
(format t "~%4. Checking stty command...~%")
(handler-case
    (let ((stty-path (basic-find-stty)))
      (let ((output (with-output-to-string (s)
                      (sb-ext:run-program stty-path '("size")
                                          :input t
                                          :output s
                                          :error nil))))
        (format t "   Terminal size: ~A" output)))
  (error (e)
    (format t "   ERROR: stty failed: ~A~%" e)))

;; Check 5: Raw mode test
(format t "~%5. Testing raw mode (will restore after)...~%")
(handler-case
    (let ((stty-path (basic-find-stty)))
      (sb-ext:run-program stty-path '("-echo" "raw" "-icanon")
                          :input t :output nil :error nil)
      (format t "   Raw mode enabled OK~%")
      (sb-ext:run-program stty-path '("echo" "-raw" "icanon")
                          :input t :output nil :error nil)
      (format t "   Raw mode disabled OK~%"))
  (error (e)
    (format t "   ERROR: Raw mode test failed: ~A~%" e)))

;; Check 6: Environment
(format t "~%6. Environment:~%")
(format t "   TERM=~A~%" (sb-ext:posix-getenv "TERM"))
(format t "   SHELL=~A~%" (sb-ext:posix-getenv "SHELL"))
(format t "   DISPLAY=~A~%" (or (sb-ext:posix-getenv "DISPLAY") "(not set)"))
(format t "   ALACRITTY_SOCKET=~A~%" (or (sb-ext:posix-getenv "ALACRITTY_SOCKET") "(not set)"))

;; Check 7: Quick input test
(format t "~%7. Input test (press any key within 3 seconds)...~%")
(finish-output)
(handler-case
    (let ((stty-path (basic-find-stty))
          (tty-path (basic-find-tty)))
      (sb-ext:run-program stty-path '("-echo" "raw" "-icanon")
                          :input t :output nil :error nil)
      (let* ((tty (open tty-path :direction :input :element-type '(unsigned-byte 8)))
             (start (get-internal-real-time))
             (timeout (* 3 internal-time-units-per-second))
             (got-input nil))
        (loop while (< (- (get-internal-real-time) start) timeout)
               do (when (listen tty)
                    (read-byte tty)
                    (setf got-input t)
                    (return)))
        (close tty)
        (sb-ext:run-program stty-path '("echo" "-raw" "icanon")
                            :input t :output nil :error nil)
        (if got-input
            (format t "   OK: Received keyboard input~%")
            (format t "   WARNING: No input received (timeout). This may indicate input issues.~%"))))
  (error (e)
    (let ((stty-path (basic-find-stty)))
      (sb-ext:run-program stty-path '("echo" "-raw" "icanon")
                          :input t :output nil :error nil))
    (format t "   ERROR: Input test failed: ~A~%" e)))

(format t "~%=== Diagnostic Complete ===~%")
(format t "~%If you see errors above, please report them.~%")
(format t "If all tests pass but gilt still freezes, the issue may be elsewhere.~%~%")

(sb-ext:exit)
