;;;; t/packages.lisp — test package + root suite

(defpackage #:consh/test
  (:use #:cl #:fiveam #:consh #:consh.ffi)
  ;; CONSH shadows CL:PARSE-ERROR; take CONSH's symbol here too.
  (:shadowing-import-from #:consh #:parse-error)
  (:export #:consh #:run-tests))

(in-package #:consh/test)

(def-suite consh
  :description "All consh test suites.")

(defun run-tests ()
  (run! 'consh))
