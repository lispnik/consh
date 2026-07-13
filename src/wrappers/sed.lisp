;;;; wrappers/sed.lisp — sed: translate GNU-form flags to BSD (SPEC §2 dialects).
;;;;
;;;; sed is a pure DIALECT-TRANSLATION wrapper: it enriches nothing (its output
;;;; is transformed text), but its flag syntax diverges between GNU and BSD.  We
;;;; treat GNU as the canonical surface the user writes, probe which sed is
;;;; installed, and rewrite to the BSD spelling only when the host sed is BSD:
;;;;
;;;;   -i            (in-place, no backup)   ->  -i ''      (BSD needs the arg)
;;;;   -i.bak                                ->  -i .bak
;;;;   --in-place[=SUF]                      ->  -i [SUF]
;;;;   -r / --regexp-extended                ->  -E
;;;;
;;;; On GNU (or an unclassified sed) the invocation is passed through untouched.

(in-package #:consh)

(defclass sed-invocation (command-invocation) ()
  (:documentation "A `sed` call.  GNU flag syntax, translated to BSD on BSD."))

(defmethod command-dialect ((c sed-invocation))
  (ensure-dialect c "sed"))

(defun %sed-arg-to-bsd (arg)
  "Translate one GNU-form sed ARG to BSD form; returns a LIST of replacements
(usually one element, two when a bare -i must grow an explicit suffix arg)."
  (cond
    ;; in-place with no backup: GNU `-i`, BSD needs an (empty) suffix argument
    ((string= arg "-i") (list "-i" ""))
    ((string= arg "--in-place") (list "-i" ""))
    ((and (> (length arg) (length "--in-place="))
          (string= arg "--in-place=" :end1 (length "--in-place=")))
     (list "-i" (subseq arg (length "--in-place="))))
    ;; GNU treats everything after -i in the same token as the suffix (`-i.bak`)
    ((and (> (length arg) 2) (char= (char arg 0) #\-) (char= (char arg 1) #\i))
     (list "-i" (subseq arg 2)))
    ;; extended regexp: GNU -r / --regexp-extended -> BSD -E
    ((or (string= arg "-r") (string= arg "--regexp-extended")) (list "-E"))
    ;; a short-flag bundle carrying r (e.g. -nr): swap r for E in place
    ((and (> (length arg) 1) (char= (char arg 0) #\-) (char/= (char arg 1) #\-)
          (find #\r arg :start 1))
     (list (substitute #\E #\r arg :start 1)))
    (t (list arg))))

(defun %sed-translate-to-bsd (args)
  (loop for a in args append (%sed-arg-to-bsd a)))

(defmethod rewrite-invocation ((c sed-invocation))
  ;; Only rewrite when we are certain the host sed is BSD; GNU and :unknown pass
  ;; through (the user writes GNU, which a GNU sed already understands).
  (if (eq (command-dialect c) :bsd)
      (let ((new (%sed-translate-to-bsd (invocation-arguments c))))
        (if (equal new (invocation-arguments c))
            c
            (make-instance 'sed-invocation :program (invocation-program c)
                           :arguments new)))
      c))

(register-wrapper "sed" 'sed-invocation)
