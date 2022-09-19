(defsystem "emacs-update"
  :version "0.1.0"
  :author "Tim Hawes"
  :license "MIT"
  :depends-on (#:cl-ppcre
               #:unix-opts
	       #:drakma)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "main"))))
  :description "Looks for git projects your Doom Emacs uses and notifies if the package has updates."
  :in-order-to ((test-op (test-op "emacs-update/tests")))
  :build-operation "asdf:program-op"
  :build-pathname "target/emacs-update"
  :entry-point "emacs-update:-main")

(defsystem "emacs-update/tests"
  :author "Tim Hawes"
  :license "MIT"
  :depends-on ("emacs-update"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for emacs-update"
  :perform (test-op (op c) (symbol-call :rove :run c)))
