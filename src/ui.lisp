(in-package #:gilt.ui)

;;; UI Components - Panels, Boxes, Text Rendering - CLOS-based
;;; LazyGit-style layout with bordered panels

;;; Text utilities

(defun word-wrap-lines (lines width)
  "Wrap a list of logical lines to fit within width, returning visual lines.
   Each logical line becomes one or more visual lines."
  (let ((result nil))
    (dolist (line lines)
      (if (or (null line) (string= line "") (<= (length line) width))
          (push (or line "") result)
          ;; Need to wrap this line
          (let ((pos 0)
                (len (length line)))
            (loop while (< pos len) do
              (let* ((remaining (- len pos))
                     (chunk-end (min (+ pos width) len)))
                (if (<= remaining width)
                    ;; Last chunk
                    (progn
                      (push (subseq line pos) result)
                      (setf pos len))
                    ;; Find word break
                    (let ((break-pos (position #\Space line :start pos :end chunk-end :from-end t)))
                      (if (and break-pos (> break-pos pos))
                          ;; Break at space
                          (progn
                            (push (subseq line pos break-pos) result)
                            (setf pos (1+ break-pos)))  ; Skip the space
                          ;; No space found, hard break
                          (progn
                            (push (subseq line pos chunk-end) result)
                            (setf pos chunk-end))))))))))
    (nreverse result)))

;;; Panel class

(defclass panel ()
  ((x :initarg :x :accessor panel-x :initform 1)
   (y :initarg :y :accessor panel-y :initform 1)
   (width :initarg :width :accessor panel-width :initform 40)
   (height :initarg :height :accessor panel-height :initform 10)
   (title :initarg :title :accessor panel-title :initform nil)
   (items :initarg :items :accessor panel-items :initform nil
          :documentation "List of displayable items")
   (selected :initarg :selected :accessor panel-selected :initform 0
             :documentation "Selected index")
   (scroll-offset :initarg :scroll-offset :accessor panel-scroll-offset :initform 0
                  :documentation "For scrolling long lists")
   (focused :initarg :focused :accessor panel-focused :initform nil
            :documentation "Whether panel has focus"))
  (:documentation "A bordered panel that can display a list of items"))

(defun make-panel (&key (x 1) (y 1) (width 40) (height 10) title items (selected 0) focused)
  (make-instance 'panel :x x :y y :width width :height height
                        :title title :items items :selected selected :focused focused))

(defmethod print-object ((panel panel) stream)
  (print-unreadable-object (panel stream :type t)
    (format stream "~A ~Dx~D at (~D,~D)~@[ focused~]"
            (or (panel-title panel) "untitled")
            (panel-width panel) (panel-height panel)
            (panel-x panel) (panel-y panel)
            (panel-focused panel))))

;;; Box drawing characters (Unicode)

(defparameter *box-chars*
  '(:top-left #\╭
    :top-right #\╮
    :bottom-left #\╰
    :bottom-right #\╯
    :horizontal #\─
    :vertical #\│
    :t-down #\┬
    :t-up #\┴
    :t-right #\├
    :t-left #\┤
    :cross #\┼))

(defun box-char (name)
  (getf *box-chars* name))

;;; Drawing primitives

(defun draw-text (row col text &key max-width)
  "Draw text at position, optionally truncating"
  (cursor-to row col)
  (when (and max-width (<= max-width 0)) (return-from draw-text nil))
  (let ((display-text (if (and max-width (> (length text) max-width))
                          (concatenate 'string (subseq text 0 (max 0 (- max-width 1))) "…")
                          text)))
    (write-string display-text *terminal-io*)))

(defun draw-horizontal-line (row col width &optional (char (box-char :horizontal)))
  "Draw a horizontal line"
  (cursor-to row col)
  (loop repeat width do (princ char)))

(defun draw-vertical-line (col start-row end-row &optional (char (box-char :vertical)))
  "Draw a vertical line"
  (loop for row from start-row to end-row do
    (cursor-to row col)
    (princ char)))

(defun draw-box (x y width height &key title (focused nil) item-info)
  "Draw a box with optional title and item count info (e.g., '2 of 3')"
  (when (or (<= width 2) (<= height 2))
    (return-from draw-box nil))
  (let ((x2 (+ x width -1))
        (y2 (+ y height -1))
        (tl (box-char :top-left))
        (tr (box-char :top-right))
        (bl (box-char :bottom-left))
        (br (box-char :bottom-right))
        (hz (box-char :horizontal))
        (vt (box-char :vertical)))
    ;; Set color based on focus - use visible colors
    (if focused
        (fg (color-code :bright-cyan))
        (fg (color-code :white)))
    (finish-output *terminal-io*)
    ;; Top line with corners
    (cursor-to y x)
    (finish-output *terminal-io*)
    (write-char tl *terminal-io*)
    (loop repeat (max 0 (- width 2)) do (write-char hz *terminal-io*))
    (write-char tr *terminal-io*)
    (finish-output *terminal-io*)
    ;; Vertical lines and bottom corners
    (loop for row from (1+ y) below y2 do
      (cursor-to row x)
      (write-char vt *terminal-io*)
      (cursor-to row x2)
      (write-char vt *terminal-io*))
    (finish-output *terminal-io*)
    ;; Bottom line with item info (positioned on right side with padding)
    (cursor-to y2 x)
    (if focused
        (fg (color-code :bright-cyan))
        (fg (color-code :white)))
    (write-char bl *terminal-io*)
    (if item-info
        ;; Draw with item info on right side, with 1 char border on each side
        (let* ((info-str (format nil "~A" item-info))
               (info-len (length info-str))
               (right-padding 1)  ; 1 char border before corner
               (left-border (- width 2 info-len right-padding)))
          (loop repeat (max 0 left-border) do (write-char hz *terminal-io*))
          (fg (color-code :bright-cyan))
          (write-string info-str *terminal-io*)
          (if focused
              (fg (color-code :bright-cyan))
              (fg (color-code :white)))
          (loop repeat right-padding do (write-char hz *terminal-io*)))
        ;; No item info - just draw border
        (loop repeat (max 0 (- width 2)) do (write-char hz *terminal-io*)))
    ;; Position explicitly for right corner to ensure alignment
    (cursor-to y2 x2)
    (write-char br *terminal-io*)
    (finish-output *terminal-io*)
    ;; Title - colorize only the title word, not the [N] prefix
    (when title
      (cursor-to y (+ x 2))
      ;; Parse title to separate [N] prefix from actual title
      (let* ((title-str (format nil " ~A " title))
             ;; Find where the actual title starts (after [N] )
             (bracket-end (position #\] title-str))
             (prefix (if bracket-end (subseq title-str 0 (+ bracket-end 2)) ""))
             (rest (if bracket-end (subseq title-str (+ bracket-end 2)) title-str)))
        ;; Draw prefix in border color
        (if focused
            (fg (color-code :bright-cyan))
            (fg (color-code :white)))
        (write-string prefix *terminal-io*)
        ;; Draw actual title in magenta (bold if focused)
        (if focused
            (progn (bold) (fg (color-code :bright-magenta)))
            (fg (color-code :magenta)))
        (write-string rest *terminal-io*)))
    (reset)
    (finish-output *terminal-io*)))

;;; Panel rendering

(defun panel-visible-items (panel)
  "Return the items visible in the panel's viewport"
  (let* ((content-height (max 0 (- (panel-height panel) 2)))
         (items (panel-items panel))
         (offset (panel-scroll-offset panel)))
    (subseq items 
            offset 
            (min (length items) (+ offset content-height)))))

(defun draw-panel (panel)
  "Draw a complete panel with border and contents"
  (let* ((x (panel-x panel))
         (y (panel-y panel))
         (w (panel-width panel))
         (h (panel-height panel))
         (focused (panel-focused panel))
         (selected (panel-selected panel))
         (offset (panel-scroll-offset panel))
         (content-width (max 0 (- w 2)))
         (total-items (length (panel-items panel)))
         (item-info (when (> total-items 0)
                      (format nil "~D of ~D" (1+ selected) total-items))))
    ;; Draw border with item count
    (draw-box x y w h :title (panel-title panel) :focused focused :item-info item-info)
    ;; Draw items
    (loop for item in (panel-visible-items panel)
          for i from 0
          for row = (+ y 1 i)
          for actual-index = (+ offset i)
          do
             (cursor-to row (+ x 1))
             ;; Clear the line content area
             (loop repeat content-width do (write-char #\Space *terminal-io*))
             (cursor-to row (+ x 1))
             ;; Highlight selected item
             (when (and focused (= actual-index selected))
               (inverse))
             ;; Render item - check for colored-item structure
             (cond
               ;; Multi-colored item: (:multi-colored (color1 text1) (color2 text2) ...)
               ((and (consp item) (eq (car item) :multi-colored))
                (let ((col (+ x 1))
                      (remaining content-width))
                  (dolist (segment (cdr item))
                    (when (> remaining 0)
                      (let* ((seg-color (first segment))
                             (seg-text (second segment))
                             (text-len (length seg-text))
                             (display-text (if (> text-len remaining)
                                               (subseq seg-text 0 remaining)
                                               seg-text)))
                        (fg (color-code seg-color))
                        (write-string display-text *terminal-io*)
                        (decf remaining (length display-text)))))))
               ;; Single colored item: (:colored color text)
               ((and (consp item) (eq (car item) :colored))
                (let ((item-color (second item))
                      (text (third item)))
                  (fg (color-code item-color))
                  (draw-text row (+ x 1) text :max-width content-width)))
               ;; Plain text item
               (t
                (let ((text (if (stringp item) item (format nil "~A" item))))
                  (draw-text row (+ x 1) text :max-width content-width))))
             (reset))
    (finish-output *terminal-io*)))

;;; Panel navigation

(defun panel-select-next (panel)
  "Move selection down"
  (let ((max-idx (1- (length (panel-items panel)))))
    (when (< (panel-selected panel) max-idx)
      (incf (panel-selected panel))
      (panel-scroll-to-selection panel))))

(defun panel-select-prev (panel)
  "Move selection up"
  (when (> (panel-selected panel) 0)
    (decf (panel-selected panel))
    (panel-scroll-to-selection panel)))

(defun panel-select-page-down (panel &optional (page-size 10))
  "Move selection down by PAGE-SIZE items"
  (let ((max-idx (1- (length (panel-items panel)))))
    (setf (panel-selected panel) (min max-idx (+ (panel-selected panel) page-size)))
    (panel-scroll-to-selection panel)))

(defun panel-select-page-up (panel &optional (page-size 10))
  "Move selection up by PAGE-SIZE items"
  (setf (panel-selected panel) (max 0 (- (panel-selected panel) page-size)))
  (panel-scroll-to-selection panel))

(defun panel-scroll-to-selection (panel)
  "Adjust scroll offset to keep selection visible"
  (let* ((selected (panel-selected panel))
         (offset (panel-scroll-offset panel))
         (visible-height (- (panel-height panel) 2)))
    (cond
      ;; Selection above viewport
      ((< selected offset)
       (setf (panel-scroll-offset panel) selected))
      ;; Selection below viewport
      ((>= selected (+ offset visible-height))
       (setf (panel-scroll-offset panel) (- selected visible-height -1))))))

;;; Status bar

(defun draw-status-bar (row width text &key (bg-color :bright-black))
  "Draw a status bar at the bottom"
  (cursor-to row 1)
  (bg (color-code bg-color))
  (fg (color-code :white))
  (write-string text *terminal-io*)
  ;; Fill rest of line
  (loop repeat (max 0 (- width (length text))) do (write-char #\Space *terminal-io*))
  (reset)
  (finish-output *terminal-io*))

;;; Help bar

(defun draw-help-bar (row width bindings &optional version)
  "Draw a help bar showing key bindings. bindings is alist of (key . description)"
  (cursor-to row 1)
  (bg (color-code 236))
  ;; Draw keybindings
  (loop for (key . desc) in bindings
        for first = t then nil
        do
           (unless first (write-string "  " *terminal-io*))
           (fg (color-code :bright-cyan))
           (write-string key *terminal-io*)
           (fg (color-code :white))
           (write-char #\Space *terminal-io*)
           (write-string desc *terminal-io*))
  ;; Fill rest of line and draw version on right
  (if version
      (let* ((version-str (format nil "v~A" version))
             (version-len (length version-str)))
        ;; Clear to near end, leaving room for version
        (clear-to-end)
        ;; Position cursor for version on right
        (cursor-to row (- width version-len))
        (bg (color-code 236))
        (fg (color-code :bright-black))
        (write-string version-str *terminal-io*))
      (clear-to-end))
  (reset)
  (finish-output *terminal-io*))

;;; Dialog class - Modal popup for user input

(defclass dialog ()
  ((title :initarg :title :accessor dialog-title :initform "Dialog")
   (message :initarg :message :accessor dialog-message :initform nil)
   (input-lines :initarg :input-lines :accessor dialog-input-lines :initform (list ""))
   (cursor-line :initarg :cursor-line :accessor dialog-cursor-line :initform 0)
   (buttons :initarg :buttons :accessor dialog-buttons :initform '("OK" "Cancel"))
   (selected-button :initarg :selected-button :accessor dialog-selected-button :initform 0)
   (input-mode :initarg :input-mode :accessor dialog-input-mode :initform nil)
   (multiline :initarg :multiline :accessor dialog-multiline :initform nil)
   (height :initarg :height :accessor dialog-height :initform 3)
   (data :initarg :data :accessor dialog-data :initform nil
         :documentation "Arbitrary data storage for dialog context"))
  (:documentation "A modal dialog box with optional multi-line input"))

(defun make-dialog (&key title message (buttons '("OK" "Cancel")) input-mode multiline data)
  "Create a dialog"
  (make-instance 'dialog
                 :title title
                 :message message
                 :buttons buttons
                 :input-mode input-mode
                 :multiline multiline
                 :data data
                 :height (if multiline 12 3)))

(defmethod print-object ((dlg dialog) stream)
  (print-unreadable-object (dlg stream :type t)
    (format stream "~A~@[ input-mode~]~@[ multiline~]"
            (dialog-title dlg)
            (dialog-input-mode dlg)
            (dialog-multiline dlg))))

(defgeneric draw-dialog (dialog width height)
  (:documentation "Draw the dialog centered on screen"))

(defun wrap-text (text width)
  "Wrap text to fit within WIDTH characters, returning list of lines"
  (if (<= (length text) width)
      (list text)
      (let ((lines nil)
            (current-line "")
            (words (cl-ppcre:split "\\s+" text)))
        (dolist (word words)
          (cond
            ;; First word on line
            ((zerop (length current-line))
             (setf current-line word))
            ;; Word fits on current line
            ((<= (+ (length current-line) 1 (length word)) width)
             (setf current-line (concatenate 'string current-line " " word)))
            ;; Start new line
            (t
             (push current-line lines)
             (setf current-line word))))
        ;; Don't forget last line
        (when (> (length current-line) 0)
          (push current-line lines))
        (nreverse lines))))

(defmethod draw-dialog ((dlg dialog) screen-width screen-height)
  "Draw a dialog box - single or multi-line"
  (let* ((msg (or (dialog-message dlg) ""))
         (msg-lines (if (dialog-input-mode dlg)
                        nil
                        (wrap-text msg (min 60 (- screen-width 10)))))
         (buttons-width (+ 4 (reduce #'+ (mapcar (lambda (b) (+ 3 (length b)))
                                                  (dialog-buttons dlg))
                                        :initial-value 0)))
         (base-w (if (dialog-input-mode dlg) 
                     70 
                     (max 45 buttons-width
                          (+ 6 (reduce #'max (mapcar #'length msg-lines) :initial-value 0)))))
         (w (min base-w (- screen-width 4)))
         (h (if (dialog-input-mode dlg)
                (dialog-height dlg)
                (+ 3 (length msg-lines))))  ; top + message lines + buttons + bottom
         (x (floor (- screen-width w) 2))
         (y (floor (- screen-height h) 2))
         (content-width (- w 2)))
    ;; Draw box with rounded corners
    (fg (color-code :bright-cyan))
    (bg (color-code 236))
    ;; Top border with title
    (cursor-to y x)
    (write-char (box-char :top-left) *terminal-io*)
    (when (dialog-title dlg)
      (fg (color-code :bright-white))
      (bold)
      (write-string (format nil " ~A " (dialog-title dlg)) *terminal-io*)
      (reset)
      (fg (color-code :bright-cyan))
      (bg (color-code 236)))
    (let ((title-len (if (dialog-title dlg) (+ 2 (length (dialog-title dlg))) 0)))
      (loop repeat (max 0 (- w 2 title-len)) do (write-char (box-char :horizontal) *terminal-io*)))
    (write-char (box-char :top-right) *terminal-io*)
    
    (cond
      ;; Multi-line input mode
      ((and (dialog-input-mode dlg) (dialog-multiline dlg))
       (let* ((num-lines (- h 3))  ; content rows (minus top, bottom, button row)
              (text-width (max 1 (- content-width 3)))  ; minus " ❯ " prompt, ensure positive
              (logical-lines (or (dialog-input-lines dlg) (list "")))
              (cursor-logical (min (dialog-cursor-line dlg) (1- (length logical-lines)))))
         ;; Draw each logical line (simple approach - no word wrap for now, just truncate)
         (loop for row from 0 below num-lines
               for screen-row = (+ y 1 row)
               do
                  (cursor-to screen-row x)
                  (write-char (box-char :vertical) *terminal-io*)
                  (bg (color-code 236))
                  ;; Prompt on first line only
                  (if (= row 0)
                      (progn (fg (color-code :bright-green)) (write-string " ❯ " *terminal-io*))
                      (write-string "   " *terminal-io*))
                  ;; Line content
                  (fg (color-code :bright-white))
                  (let* ((line-text (if (< row (length logical-lines)) 
                                        (or (nth row logical-lines) "")
                                        ""))
                         (display-len (min (length line-text) text-width))
                         (display-text (subseq line-text 0 display-len))
                         (padding (max 0 (- text-width display-len))))
                    (write-string display-text *terminal-io*)
                    ;; Cursor on current line
                    (if (= row cursor-logical)
                        (progn
                          (fg (color-code :bright-cyan))
                          (write-char #\▌ *terminal-io*)
                          (bg (color-code 236))
                          (loop repeat (max 0 (1- padding)) do (write-char #\Space *terminal-io*)))
                        (loop repeat padding do (write-char #\Space *terminal-io*))))
                  ;; Position right border explicitly
                  (cursor-to screen-row (+ x w -1))
                  (fg (color-code :bright-cyan))
                  (write-char (box-char :vertical) *terminal-io*))
         ;; Button row
         (cursor-to (+ y h -2) x)
         (write-char (box-char :vertical) *terminal-io*)
         (bg (color-code 236))
         (write-char #\Space *terminal-io*)
         (loop for btn in (dialog-buttons dlg)
               for i from 0
               do
                  (if (= i (dialog-selected-button dlg))
                      (progn (bg (color-code :bright-cyan)) (fg (color-code :black)))
                      (progn (bg (color-code 240)) (fg (color-code :bright-white))))
                  (write-string (format nil " ~A " btn) *terminal-io*)
                  (bg (color-code 236))
                  (write-char #\Space *terminal-io*))
         ;; Position right border explicitly for button row
         (cursor-to (+ y h -2) (+ x w -1))
         (fg (color-code :bright-cyan))
         (write-char (box-char :vertical) *terminal-io*)))
      
      ;; Single-line input mode
      ((dialog-input-mode dlg)
       (cursor-to (1+ y) x)
       (write-char (box-char :vertical) *terminal-io*)
       (bg (color-code 236))
       (let* ((input-width (- content-width 4))
              (lines (dialog-input-lines dlg))
              (buf (if lines (first lines) ""))
              (display-buf (if (> (length buf) input-width)
                               (subseq buf (- (length buf) input-width))
                               buf)))
         (fg (color-code :bright-green))
         (write-string " ❯ " *terminal-io*)
         (fg (color-code :bright-white))
         (write-string display-buf *terminal-io*)
         (fg (color-code :bright-cyan))
         (write-char #\▌ *terminal-io*))
       ;; Position right border explicitly
       (cursor-to (1+ y) (+ x w -1))
       (fg (color-code :bright-cyan))
       (write-char (box-char :vertical) *terminal-io*))
      
      ;; Confirmation mode - with wrapped message lines
      (t
       (let ((buttons (dialog-buttons dlg)))
         ;; Draw message lines
         (loop for line in msg-lines
               for row from 0
               for screen-row = (+ y 1 row)
               do
                  (cursor-to screen-row x)
                  (write-char (box-char :vertical) *terminal-io*)
                  (bg (color-code 236))
                  (if (= row 0)
                      (progn (fg (color-code :bright-green)) (write-string " ? " *terminal-io*))
                      (write-string "   " *terminal-io*))
                  (fg (color-code :bright-white))
                  (write-string line *terminal-io*)
                  ;; Pad to content width
                  (let ((padding (- content-width 3 (length line))))
                    (when (> padding 0)
                      (loop repeat padding do (write-char #\Space *terminal-io*))))
                  (fg (color-code :bright-cyan))
                  (write-char (box-char :vertical) *terminal-io*))
         ;; Draw button row
         (let ((button-row (+ y 1 (length msg-lines))))
           (cursor-to button-row x)
           (write-char (box-char :vertical) *terminal-io*)
           (bg (color-code 236))
           (write-string "   " *terminal-io*)  ; align with message
           (loop for btn in buttons
                 for i from 0
                 do
                    (if (= i (dialog-selected-button dlg))
                        (progn (bg (color-code :bright-cyan)) (fg (color-code :black)))
                        (progn (bg (color-code 240)) (fg (color-code :bright-white))))
                    (write-string (format nil " ~A " btn) *terminal-io*)
                    (bg (color-code 236))
                    (write-char #\Space *terminal-io*))
           ;; Pad to right border
           (cursor-to button-row (+ x w -1))
           (fg (color-code :bright-cyan))
           (write-char (box-char :vertical) *terminal-io*)))))
    
    ;; Bottom border with hint - position explicitly to ensure alignment
    (cursor-to (+ y h -1) x)
    (write-char (box-char :bottom-left) *terminal-io*)
    (fg (color-code :white))
    (let* ((hint (cond
                   ((dialog-multiline dlg) " Enter:newline Ctrl+D:submit Esc:cancel ")
                   ((dialog-input-mode dlg) " Enter:confirm Esc:cancel ")
                   (t " Tab:switch Enter:confirm Esc:cancel ")))
           (hint-len (length hint))
           (border-len (- w 2 hint-len)))
      (write-string hint *terminal-io*)
      (fg (color-code :bright-cyan))
      (loop repeat (max 0 border-len) do (write-char (box-char :horizontal) *terminal-io*)))
    ;; Position explicitly for right corner
    (cursor-to (+ y h -1) (+ x w -1))
    (write-char (box-char :bottom-right) *terminal-io*)
    (reset)
    (finish-output *terminal-io*)))

(defgeneric handle-dialog-key (dialog key)
  (:documentation "Handle key input for dialog, returns :ok, :cancel, or nil"))

(defun dialog-get-text (dlg)
  "Get all text from dialog as single string (lines joined with newlines)"
  (format nil "~{~A~^~%~}" (dialog-input-lines dlg)))

(defmethod handle-dialog-key ((dlg dialog) key)
  "Handle key events for dialog"
  (let ((result
         (cond
    ;; Escape - cancel
    ((eq (key-event-code key) +key-escape+)
     :cancel)
    ;; Tab - switch buttons (multiline mode)
    ((and (dialog-multiline dlg)
          (eq (key-event-code key) +key-tab+))
     (setf (dialog-selected-button dlg)
           (mod (1+ (dialog-selected-button dlg)) (length (dialog-buttons dlg))))
     nil)
    ;; Enter in multiline input mode - add new line (Tab to switch to button, then Enter confirms)
    ((and (dialog-multiline dlg)
          (dialog-input-mode dlg)
          (eq (key-event-code key) +key-enter+))
     ;; Add new line
     (let* ((lines (or (dialog-input-lines dlg) (list "")))
            (cur-line (min (dialog-cursor-line dlg) (1- (length lines))))
            (max-lines (max 1 (- (dialog-height dlg) 3))))
       (when (< (length lines) max-lines)
         (setf (dialog-input-lines dlg)
               (append (subseq lines 0 (min (1+ cur-line) (length lines)))
                       (list "")
                       (if (< (1+ cur-line) (length lines))
                           (subseq lines (1+ cur-line))
                           nil)))
         (setf (dialog-cursor-line dlg) (min (1+ cur-line) (1- (length (dialog-input-lines dlg)))))))
     nil)
    ;; Ctrl+D in multiline input mode - confirm/submit
    ;; Note: Ctrl+D sends byte 4, which we convert to #\d with ctrl-p flag
    ((and (dialog-multiline dlg)
          (dialog-input-mode dlg)
          (key-event-ctrl-p key)
          (key-event-char key)
          (char-equal (key-event-char key) #\d))
     ;; DEBUG: Log to command log via side effect
     :ok)
    ;; Also catch raw Ctrl+D (byte 4) in case it comes through differently
    ((and (dialog-multiline dlg)
          (dialog-input-mode dlg)
          (key-event-char key)
          (= (char-code (key-event-char key)) 4))
     :ok)
    ;; Enter - confirm (single-line or confirmation)
    ((eq (key-event-code key) +key-enter+)
     (if (dialog-input-mode dlg)
         :ok  ; Single-line input always confirms on Enter
         (let ((btn (nth (dialog-selected-button dlg) (dialog-buttons dlg))))
           (if (string= btn "Cancel") :cancel :ok))))
    ;; Up arrow in multiline
    ((and (dialog-multiline dlg)
          (eq (key-event-code key) +key-up+))
     (when (> (dialog-cursor-line dlg) 0)
       (decf (dialog-cursor-line dlg)))
     nil)
    ;; Down arrow in multiline
    ((and (dialog-multiline dlg)
          (eq (key-event-code key) +key-down+))
     (when (< (dialog-cursor-line dlg) (1- (length (dialog-input-lines dlg))))
       (incf (dialog-cursor-line dlg)))
     nil)
    ;; Tab - switch buttons (confirmation only)
    ((eq (key-event-code key) +key-tab+)
     (setf (dialog-selected-button dlg)
           (mod (1+ (dialog-selected-button dlg)) (length (dialog-buttons dlg))))
     nil)
    ;; Left/Right - switch buttons (confirmation only)
    ((and (not (dialog-input-mode dlg))
          (or (eq (key-event-code key) +key-left+)
              (eq (key-event-code key) +key-right+)))
     (setf (dialog-selected-button dlg)
           (mod (1+ (dialog-selected-button dlg)) (length (dialog-buttons dlg))))
     nil)
    ;; Backspace in input mode (check both code and Ctrl+H)
    ((and (dialog-input-mode dlg)
          (or (eq (key-event-code key) +key-backspace+)
              (and (key-event-char key) 
                   (char= (key-event-char key) #\h)
                   (key-event-ctrl-p key))))
     (let* ((lines (dialog-input-lines dlg))
            (cur-line (dialog-cursor-line dlg))
            (cur-text (nth cur-line lines)))
       (if (> (length cur-text) 0)
           ;; Delete char from current line
           (setf (nth cur-line (dialog-input-lines dlg))
                 (subseq cur-text 0 (1- (length cur-text))))
           ;; At start of line - merge with previous if multiline
           (when (and (dialog-multiline dlg) (> cur-line 0))
             (setf (dialog-input-lines dlg)
                   (append (subseq lines 0 cur-line)
                           (subseq lines (1+ cur-line))))
             (decf (dialog-cursor-line dlg)))))
     nil)
    ;; Newline character (from paste) in multiline mode
    ((and (dialog-input-mode dlg)
          (dialog-multiline dlg)
          (key-event-char key)
          (char= (key-event-char key) #\Newline))
     (let* ((lines (or (dialog-input-lines dlg) (list "")))
            (cur-line (min (dialog-cursor-line dlg) (1- (length lines))))
            (max-lines (max 1 (- (dialog-height dlg) 3))))
       (when (< (length lines) max-lines)
         (setf (dialog-input-lines dlg)
               (append (subseq lines 0 (min (1+ cur-line) (length lines)))
                       (list "")
                       (if (< (1+ cur-line) (length lines))
                           (subseq lines (1+ cur-line))
                           nil)))
         (setf (dialog-cursor-line dlg) (min (1+ cur-line) (1- (length (dialog-input-lines dlg)))))))
     nil)
    ;; Character input
    ((and (dialog-input-mode dlg)
          (key-event-char key)
          (graphic-char-p (key-event-char key)))
     (let* ((lines (or (dialog-input-lines dlg) (list "")))
            (cur-line (min (dialog-cursor-line dlg) (1- (length lines))))
            (cur-text (or (nth cur-line lines) "")))
       (when (null (dialog-input-lines dlg))
         (setf (dialog-input-lines dlg) (list "")))
       (setf (nth cur-line (dialog-input-lines dlg))
             (concatenate 'string cur-text (string (key-event-char key)))))
     nil)
    (t nil))))
    result))
