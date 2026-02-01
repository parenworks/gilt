(in-package #:gilt.ui)

;;; UI Components - Panels, Boxes, Text Rendering - CLOS-based
;;; LazyGit-style layout with bordered panels

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
  (let ((display-text (if (and max-width (> (length text) max-width))
                          (concatenate 'string (subseq text 0 (- max-width 1)) "…")
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

(defun draw-box (x y width height &key title (focused nil))
  "Draw a box with optional title"
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
    (loop repeat (- width 2) do (write-char hz *terminal-io*))
    (write-char tr *terminal-io*)
    (finish-output *terminal-io*)
    ;; Vertical lines and bottom corners
    (loop for row from (1+ y) below y2 do
      (cursor-to row x)
      (write-char vt *terminal-io*)
      (cursor-to row x2)
      (write-char vt *terminal-io*))
    (finish-output *terminal-io*)
    ;; Bottom line
    (cursor-to y2 x)
    (write-char bl *terminal-io*)
    (loop repeat (- width 2) do (write-char hz *terminal-io*))
    (write-char br *terminal-io*)
    (finish-output *terminal-io*)
    ;; Title
    (when title
      (cursor-to y (+ x 2))
      (if focused
          (progn (bold) (fg (color-code :bright-white)))
          (fg (color-code :white)))
      (format *terminal-io* " ~A " title))
    (reset)
    (finish-output *terminal-io*)))

;;; Panel rendering

(defun panel-visible-items (panel)
  "Return the items visible in the panel's viewport"
  (let* ((content-height (- (panel-height panel) 2))
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
         (content-width (- w 2)))
    ;; Draw border
    (draw-box x y w h :title (panel-title panel) :focused focused)
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
             ;; Render item (can be string or structured)
             (let ((text (if (stringp item) item (format nil "~A" item))))
               (draw-text row (+ x 1) text :max-width content-width))
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
  (loop repeat (- width (length text)) do (write-char #\Space *terminal-io*))
  (reset)
  (finish-output *terminal-io*))

;;; Help bar

(defun draw-help-bar (row width bindings)
  "Draw a help bar showing key bindings. bindings is alist of (key . description)"
  (cursor-to row 1)
  (bg (color-code 236))
  (loop for (key . desc) in bindings
        for first = t then nil
        do
           (unless first (write-string "  " *terminal-io*))
           (fg (color-code :bright-cyan))
           (write-string key *terminal-io*)
           (fg (color-code :white))
           (write-char #\Space *terminal-io*)
           (write-string desc *terminal-io*))
  ;; Fill rest of line
  (clear-to-end)
  (reset)
  (finish-output *terminal-io*))

;;; Dialog class - Modal popup for user input

(defclass dialog ()
  ((title :initarg :title :accessor dialog-title :initform "Dialog")
   (message :initarg :message :accessor dialog-message :initform nil)
   (input-buffer :initarg :input-buffer :accessor dialog-input-buffer :initform "")
   (buttons :initarg :buttons :accessor dialog-buttons :initform '("OK" "Cancel"))
   (selected-button :initarg :selected-button :accessor dialog-selected-button :initform 0)
   (input-mode :initarg :input-mode :accessor dialog-input-mode :initform nil))
  (:documentation "A simple modal dialog box"))

(defun make-dialog (&key title message (buttons '("OK" "Cancel")) input-mode)
  "Create a dialog"
  (make-instance 'dialog
                 :title title
                 :message message
                 :buttons buttons
                 :input-mode input-mode))

(defgeneric draw-dialog (dialog width height)
  (:documentation "Draw the dialog centered on screen"))

(defmethod draw-dialog ((dlg dialog) screen-width screen-height)
  "Draw a simple dialog box"
  (let* ((w (if (dialog-input-mode dlg) 70 50))
         (h 4)  ; 4 rows: top border, content, bottom border, +1 for 0-indexing
         (x (floor (- screen-width w) 2))
         (y (floor (- screen-height h) 2)))
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
    (let ((title-len (if (dialog-title dlg) (+ 3 (length (dialog-title dlg))) 0)))
      (loop repeat (max 0 (- w 2 title-len)) do (write-char (box-char :horizontal) *terminal-io*)))
    (write-char (box-char :top-right) *terminal-io*)
    ;; Content row
    (cursor-to (1+ y) x)
    (write-char (box-char :vertical) *terminal-io*)
    (bg (color-code 236))
    (if (dialog-input-mode dlg)
        ;; Input mode: prompt and text field
        (progn
          (fg (color-code :bright-green))
          (write-string " ❯ " *terminal-io*)
          (fg (color-code :bright-white))
          (let* ((input-width (- w 6))
                 (buf (dialog-input-buffer dlg))
                 (display-buf (if (> (length buf) input-width)
                                  (subseq buf (- (length buf) input-width))
                                  buf)))
            (write-string display-buf *terminal-io*)
            (fg (color-code :bright-cyan))
            (write-char #\▌ *terminal-io*)
            (fg (color-code :white))
            (loop repeat (max 0 (- input-width (length display-buf) 1))
                  do (write-char #\Space *terminal-io*))))
        ;; Confirmation mode: message and buttons
        (progn
          (fg (color-code :bright-green))
          (write-string " ? " *terminal-io*)
          (fg (color-code :white))
          (let ((msg (or (dialog-message dlg) "")))
            (write-string msg *terminal-io*)
            (write-string "  " *terminal-io*))
          ;; Buttons
          (let ((buttons (dialog-buttons dlg)))
            (loop for btn in buttons
                  for i from 0
                  do
                     (if (= i (dialog-selected-button dlg))
                         (progn (bg (color-code :bright-cyan)) (fg (color-code :black)))
                         (progn (bg (color-code :bright-black)) (fg (color-code :white))))
                     (write-string (format nil " ~A " btn) *terminal-io*)
                     (bg (color-code 236))
                     (write-char #\Space *terminal-io*))
            (let* ((msg (or (dialog-message dlg) ""))
                   (btn-width (+ (reduce #'+ buttons :key #'length) (* 4 (length buttons))))
                   (used (+ 6 (length msg) btn-width)))
              (loop repeat (max 0 (- w 1 used)) do (write-char #\Space *terminal-io*))))))
    (fg (color-code :bright-cyan))
    (write-char (box-char :vertical) *terminal-io*)
    ;; Bottom border with hint
    (cursor-to (+ y 2) x)
    (write-char (box-char :bottom-left) *terminal-io*)
    (fg (color-code :bright-black))
    (if (dialog-input-mode dlg)
        (write-string " Enter:confirm Esc:cancel " *terminal-io*)
        (write-string " Tab:switch Enter:confirm Esc:cancel " *terminal-io*))
    (fg (color-code :bright-cyan))
    (let ((hint-len (if (dialog-input-mode dlg) 26 38)))
      (loop repeat (max 0 (- w 2 hint-len)) do (write-char (box-char :horizontal) *terminal-io*)))
    (write-char (box-char :bottom-right) *terminal-io*)
    (reset)
    (finish-output *terminal-io*)))

(defgeneric handle-dialog-key (dialog key)
  (:documentation "Handle key input for dialog, returns :ok, :cancel, or nil"))

(defmethod handle-dialog-key ((dlg dialog) key)
  "Handle key events for dialog"
  (cond
    ;; Escape - cancel
    ((eq (key-event-code key) +key-escape+)
     :cancel)
    ;; Enter - confirm
    ((eq (key-event-code key) +key-enter+)
     (if (dialog-input-mode dlg)
         :ok  ; Input mode: Enter always confirms
         (let ((btn (nth (dialog-selected-button dlg) (dialog-buttons dlg))))
           (if (string= btn "Cancel") :cancel :ok))))
    ;; Tab - switch buttons
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
    ;; Backspace in input mode
    ((and (dialog-input-mode dlg)
          (eq (key-event-code key) +key-backspace+))
     (let ((buf (dialog-input-buffer dlg)))
       (when (> (length buf) 0)
         (setf (dialog-input-buffer dlg) (subseq buf 0 (1- (length buf))))))
     nil)
    ;; Character input
    ((and (dialog-input-mode dlg)
          (key-event-char key)
          (graphic-char-p (key-event-char key)))
     (setf (dialog-input-buffer dlg)
           (concatenate 'string (dialog-input-buffer dlg) (string (key-event-char key))))
     nil)
    (t nil)))
