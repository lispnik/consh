;;;; consh.asd — Common Lisp Unix shell (non-POSIX)

(defsystem "consh"
  :description "A Unix shell implemented as a Common Lisp image. Pipelines carry CLOS objects, not bytes."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :depends-on ("cffi")
  :serial t
  ;; `asdf:make :consh` dumps a standalone executable via program-op.
  :build-operation "program-op"
  :build-pathname "../consh"          ; drop the binary at the project root
  :entry-point "consh:main"
  :pathname "src"
  :components ((:file "packages")
               (:file "ffi")
               (:file "reaper")
               (:file "channel")
               (:file "invocation")
               (:file "parse")
               (:module "wrappers"
                :components ((:file "ls")
                             (:file "find")
                             (:file "cat")
                             (:file "grep")))
               (:file "pipeline")
               (:file "exec")
               (:file "jobs")
               (:file "surface"))
  :in-order-to ((test-op (test-op "consh/test"))))

(defsystem "consh/test"
  :description "FiveAM test suites for consh."
  :depends-on ("consh" "fiveam")
  :serial t
  :pathname "t"
  :components ((:file "packages")
               (:file "ffi")
               (:file "reaper")
               (:file "channel")
               (:file "parse")
               (:file "pipeline")
               (:file "jobs")
               (:file "surface"))
  :perform (test-op (op c)
             (symbol-call :fiveam :run! (find-symbol "CONSH" :consh/test))))
