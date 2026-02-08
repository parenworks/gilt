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
           #:begin-sync-update
           #:end-sync-update
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
   (:export ;; Key event class and accessors
            #:key-event
            #:make-key-event
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
            ;; Configuration
            #:wait-for-escape-sequence
            #:*tty-path*
            #:*escape-timeout*
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
            #:read-key-with-timeout
            #:setup-terminal
            #:restore-terminal
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
           #:git-branch-tracking-info
           #:git-repo-state
           #:git-diff
           #:git-diff-staged
           #:git-log
           #:git-log-search
           #:git-log-branch-only
           #:git-branches
           #:git-tags
           #:tag-entry
           #:tag-name
           #:tag-type
           #:tag-date
           #:tag-message
           #:make-tag-entry
           #:git-create-tag
           #:git-delete-tag
           #:git-push-tag
           #:git-push-all-tags
           #:git-current-branch
           #:git-branch-has-upstream-p
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
           #:git-amend-message
           #:git-reword-commit
           #:git-reset-soft
           #:git-reset-mixed
           #:git-reset-hard
           #:git-fixup-commit
           #:git-commit-set-author
           #:git-commit-add-coauthor
           #:git-create-branch
           #:git-rename-branch
           #:git-fast-forward
           #:git-unstage-all
           #:git-push
           #:git-pull
           #:git-push-interactive
           #:git-pull-interactive
           #:git-fetch
           #:git-remotes
           #:git-remote-url
           #:git-remote-add
           #:git-remote-remove
           #:git-remote-rename
           #:git-remote-set-url
           #:git-remotes-with-urls
           #:git-submodules
           #:submodule-entry
           #:submodule-name
           #:submodule-path
           #:submodule-url
           #:submodule-status
           #:submodule-commit
           #:make-submodule-entry
           #:git-submodule-init
           #:git-submodule-update
           #:git-submodule-sync
           #:git-submodule-add
           #:git-submodule-deinit
           #:git-ahead-behind
           #:git-config-list
           #:git-config-get
           #:git-config-set
           #:git-config-unset
           #:config-entry
           #:config-key
           #:config-value
           #:config-scope
           #:make-config-entry
           #:git-worktree-list
           #:git-worktree-add
           #:git-worktree-add-new-branch
           #:git-worktree-remove
           #:git-worktree-lock
           #:git-worktree-unlock
           #:git-worktree-prune
           #:worktree-entry
           #:worktree-path
           #:worktree-head
           #:worktree-branch
           #:worktree-bare
           #:worktree-detached
           #:worktree-locked
           #:worktree-prunable
           #:make-worktree-entry
           #:git-repo-root
           #:git-repo-name
           #:git-commit-message
           #:git-blame
           #:blame-line
           #:blame-line-hash
           #:blame-line-short-hash
           #:blame-line-author
           #:blame-line-date
           #:blame-line-num
           #:blame-line-content
           #:ensure-repo
           #:*current-repo*
           ;; Stash management
           #:stash-entry
           #:stash-index
           #:stash-ref
           #:stash-branch
           #:stash-message
           #:make-stash-entry
           #:git-stash-list
           #:git-stash-push
           #:git-stash-push-staged
           #:git-stash-push-include-untracked
           #:git-stash-pop
           #:git-stash-apply
           #:git-stash-drop
           #:git-stash-clear
           #:git-stash-show
           #:git-stash-branch
           ;; Interactive rebase
           #:rebase-todo-entry
           #:make-rebase-todo-entry
           #:rebase-action
           #:rebase-hash
           #:rebase-short-hash
           #:rebase-message
           #:rebase-new-message
           #:rebase-action-string
           #:git-rebase-todo-list
           #:git-rebase-interactive
           #:git-rebase-abort
           #:git-rebase-continue
           #:git-rebase-skip
           #:git-rebase-onto))

(defpackage #:gilt.pty
  (:use #:cl)
  (:export ;; Process runner class
           #:process-runner
           #:make-process-runner
           #:runner-master-fd
           #:runner-process
           #:runner-output-lines
           #:runner-current-line
           #:runner-finished-p
           #:runner-exit-code
           ;; Generic functions
           #:runner-start
           #:runner-poll
           #:runner-send
           #:runner-stop
           #:runner-get-output))

(defpackage #:gilt.views
  (:use #:cl #:gilt.ansi #:gilt.terminal #:gilt.ui #:gilt.git #:gilt.pty)
  (:export #:view
           #:main-view
           #:draw-view
           #:handle-key
           #:refresh-data
           #:init-views
           #:*current-view*))

(defpackage #:gilt
  (:use #:cl #:gilt.ansi #:gilt.terminal #:gilt.ui #:gilt.git #:gilt.views)
  (:export #:main
           #:run
           #:*version*))
