;;;; wrappers/date.lisp — date: translate GNU epoch flag to BSD (SPEC §2 dialects).
;;;;
;;;; Another dialect-translation wrapper.  The canonical (GNU) way to format a
;;;; Unix timestamp is `date -d @SECONDS +FMT`; BSD spells it `date -r SECONDS
;;;; +FMT`.  We probe which date is installed and, on BSD, rewrite the epoch
;;;; form:
;;;;
;;;;   -d @N   /   -d@N   /   --date=@N     ->   -r N
;;;;
;;;; +FORMAT is identical on both, so it passes through.  Non-epoch `-d STRING`
;;;; (freeform relative dates) has no portable BSD equivalent and is left as-is.

(in-package #:consh)

(defclass date-invocation (command-invocation) ()
  (:documentation "A `date` call.  GNU epoch syntax, translated to BSD on BSD."))

(defmethod command-dialect ((c date-invocation))
  (ensure-dialect c "date"))

(defun %epoch-at-p (s)
  "T if S looks like an @-prefixed Unix timestamp (\"@1700000000\")."
  (and (stringp s) (> (length s) 1) (char= (char s 0) #\@)
       (every #'digit-char-p (subseq s 1))))

(defun %date-attached-epoch (arg)
  "If ARG is an attached epoch flag (\"-d@N\" or \"--date=@N\"), return N (the
digits); otherwise NIL."
  (let ((val (cond ((and (> (length arg) 2) (string= arg "-d" :end1 2)) (subseq arg 2))
                   ((and (> (length arg) (length "--date="))
                         (string= arg "--date=" :end1 (length "--date=")))
                    (subseq arg (length "--date=")))
                   (t nil))))
    (and (%epoch-at-p val) (subseq val 1))))

(defmethod rewrite-invocation ((c date-invocation))
  (if (not (eq (command-dialect c) :bsd))
      c
      (let ((args (invocation-arguments c))
            (out '()) (changed nil) (i 0))
        (loop while (< i (length args))
              for a = (nth i args)
              do (cond
                   ;; -d @N  (two tokens)
                   ((and (string= a "-d") (< (1+ i) (length args))
                         (%epoch-at-p (nth (1+ i) args)))
                    (push "-r" out) (push (subseq (nth (1+ i) args) 1) out)
                    (setf changed t) (incf i 2))
                   ;; -d@N or --date=@N  (one token)
                   ((%date-attached-epoch a)
                    (push "-r" out) (push (%date-attached-epoch a) out)
                    (setf changed t) (incf i))
                   (t (push a out) (incf i))))
        (if changed
            (make-instance 'date-invocation :program (invocation-program c)
                           :arguments (nreverse out))
            c))))

(register-wrapper "date" 'date-invocation)
