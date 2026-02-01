(defpackage #:gilt.ansi
  (:use #:cl)
  (:export ;; Color classes
           #:color
           #:indexed-color
           #:rgb-color
           #:named-color
           #:color-index
           #:color-red
           #:color-green
           #:color-blue
           #:color-name
           #:make-indexed-color
           #:make-rgb-color
           #:make-named-color
           #:lookup-color
           #:register-color
           #:emit-fg
           #:emit-bg
           ;; Style class
           #:text-style
           #:make-style
           #:style-fg
           #:style-bg
           #:style-bold-p
           #:style-dim-p
           #:style-italic-p
           #:style-underline-p
           #:style-inverse-p
           #:emit-style
           ;; Terminal class
           #:terminal
           #:terminal-stream
           #:terminal-width
           #:terminal-height
           #:term-cursor-to
           #:term-cursor-home
           #:term-cursor-hide
           #:term-cursor-show
           #:term-clear-screen
           #:term-clear-line
           #:term-clear-to-end
           #:term-reset
           #:term-write
           #:*terminal*
           ;; Convenience functions
           #:*escape*
           #:cursor-to
           #:cursor-home
           #:cursor-up
           #:cursor-down
           #:cursor-forward
           #:cursor-back
           #:cursor-hide
           #:cursor-show
           #:clear-screen
           #:clear-line
           #:clear-to-end
           #:fg
           #:bg
           #:fg-rgb
           #:bg-rgb
           #:reset
           #:bold
           #:dim
           #:italic
           #:underline
           #:inverse
           #:color-code
           #:with-style))

(defpackage #:gilt.terminal
  (:use #:cl #:gilt.ansi)
  (:export ;; Key event class
           #:key-event
           #:make-key-event
           #:key-char
           #:key-code
           #:key-ctrl-p
           #:key-alt-p
           #:key-event-char
           #:key-event-code
           #:key-event-ctrl-p
           #:key-event-alt-p
           ;; Terminal mode class
           #:terminal-mode
           #:terminal-raw-p
           #:enable-raw-mode
           #:disable-raw-mode
           #:query-size
           #:*terminal-mode*
           ;; Input reader class
           #:input-reader
           #:reader-stream
           #:read-key-event
           #:read-byte-with-timeout
           #:*input-reader*
           ;; Convenience functions
           #:with-raw-terminal
           #:terminal-size
           #:read-key
           ;; Key constants
           #:+key-up+
           #:+key-down+
           #:+key-left+
           #:+key-right+
           #:+key-enter+
           #:+key-escape+
           #:+key-tab+
           #:+key-backspace+
           #:+key-delete+
           #:+key-home+
           #:+key-end+
           #:+key-page-up+
           #:+key-page-down+))

(defpackage #:gilt.ui
  (:use #:cl #:gilt.ansi #:gilt.terminal)
  (:export #:panel
           #:make-panel
           #:panel-x
           #:panel-y
           #:panel-width
           #:panel-height
           #:panel-title
           #:panel-items
           #:panel-selected
           #:panel-scroll-offset
           #:panel-focused
           #:draw-panel
           #:draw-text
           #:draw-box
           #:draw-help-bar
           #:draw-status-bar
           #:panel-select-next
           #:panel-select-prev
           #:panel-scroll-to-selection
           ;; Dialog class
           #:dialog
           #:make-dialog
           #:dialog-x
           #:dialog-y
           #:dialog-width
           #:dialog-height
           #:dialog-title
           #:dialog-message
           #:dialog-input-buffer
           #:dialog-input-label
           #:dialog-buttons
           #:dialog-selected-button
           #:dialog-input-mode
           #:dialog-input-lines
           #:dialog-cursor-line
           #:dialog-multiline
           #:dialog-data
           #:dialog-get-text
           #:draw-dialog
           #:handle-dialog-key))

(defpackage #:gilt.git
  (:use #:cl)
  (:export ;; Repository class
           #:git-repository
           #:repo-path
           #:repo-name
           #:repo-run
           #:repo-run-lines
           ;; Status entry class
           #:status-entry
           #:make-status-entry
           #:status-entry-status
           #:status-entry-file
           #:status-entry-staged-p
           ;; Log entry class
           #:log-entry
           #:make-log-entry
           #:log-entry-hash
           #:log-entry-short-hash
           #:log-entry-author
           #:log-entry-date
           #:log-entry-message
           ;; Git operations
           #:git-run
           #:git-run-lines
           #:git-status
           #:git-diff
           #:git-diff-staged
           #:git-log
           #:git-branches
           #:git-current-branch
           #:git-stage-file
           #:git-unstage-file
           #:git-commit
           #:git-checkout
           #:git-merge
           #:git-merge-abort
           #:git-merge-in-progress-p
           #:git-mark-resolved
           #:git-edit-file
           #:git-resolve-with-ours
           #:git-resolve-with-theirs
           #:git-delete-branch
           #:git-fetch
           #:git-remote-branches
           #:git-track-remote-branch
           #:git-delete-remote-branch
           #:git-squash-commits
           #:git-cherry-pick
           #:git-revert
           #:log-entry
           #:diff-hunk
           #:hunk-file
           #:hunk-start-line
           #:hunk-line-count
           #:hunk-header
           #:hunk-content
           #:hunk-selected-p
           #:parse-diff-hunks
           #:git-stage-hunk
           #:git-stash
           #:git-stash-pop
           #:git-stash-list
           #:git-stage-all
           #:git-discard-file
           #:git-amend
           #:git-create-branch
           #:git-push
           #:git-pull
           #:git-push-interactive
           #:git-pull-interactive
           #:git-fetch
           #:git-remotes
           #:git-remote-url
           #:git-ahead-behind
           #:git-repo-root
           #:git-repo-name))

(defpackage #:gilt.views
  (:use #:cl #:gilt.ansi #:gilt.terminal #:gilt.ui #:gilt.git)
  (:export #:view
           #:main-view
           #:status-view
           #:log-view
           #:branches-view
           #:draw-view
           #:handle-key
           #:refresh-data
           #:init-views
           #:*current-view*))

(defpackage #:gilt
  (:use #:cl #:gilt.ansi #:gilt.terminal #:gilt.ui #:gilt.git #:gilt.views)
  (:export #:main
           #:run))
