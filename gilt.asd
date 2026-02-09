(asdf:defsystem #:gilt
  :description "A LazyGit-style Git TUI in Common Lisp"
  :author "Glenn"
  :license "MIT"
  :version "0.15.0"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:bordeaux-threads)
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "ansi")
                             (:file "terminal")
                             (:file "ui")
                             (:file "git")
                             (:file "pty")
                             (:file "views")
                             (:file "main")))))
