;;;; t/present.lisp — the presentation layer: table-columns + table renderer.

(in-package #:consh/test)

(def-suite present :in consh :description "Aligned-table rendering of object streams.")
(in-suite present)

(defun %fi (name size owner)
  (make-instance 'file-info :name name :path (pathname name)
                 :size size :mtime 0 :mode nil :uid nil :gid nil :owner owner))

(test table-renders-aligned-columns
  (let* ((objs (list (%fi "kernel.img" 4096 "mkennedy")
                     (%fi "notes.txt" 14 "mkennedy")))
         (out (with-output-to-string (s) (table objs :stream s)))
         (lines (with-input-from-string (s out)
                  (loop for l = (read-line s nil nil) while l collect l))))
    ;; header, rule, two data rows
    (is (= 4 (length lines)))
    (is (string= "NAME        SIZE  OWNER" (first lines)))
    (is (string= "----------  ----  --------" (second lines)))
    ;; SIZE right-aligns under its column; a left-aligned final column is trimmed
    (is (string= "kernel.img  4096  mkennedy" (third lines)))
    (is (string= "notes.txt     14  mkennedy" (fourth lines)))))

(test table-of-empty-list-prints-nothing
  (is (string= "" (with-output-to-string (s) (table '() :stream s)))))

(test table-accepts-a-single-object
  (let ((out (with-output-to-string (s) (table (%fi "a" 1 "u") :stream s))))
    (is (search "NAME" out))
    (is (search "a" out))))

(test table-columns-cover-the-wrapped-types
  (flet ((headers (o) (mapcar #'car (table-columns o))))
    (is (equal '("NAME" "SIZE" "OWNER") (headers (%fi "a" 1 "u"))))
    (is (equal '("FILE" "LINE" "TEXT")
               (headers (make-instance 'grep-match :file "f" :line-number 1 :text "t"))))
    (is (equal '("MOUNT" "BLOCKS" "USED" "AVAIL" "USE%" "DEVICE")
               (headers (make-instance 'filesystem :device "d" :blocks 1 :used 1
                                       :available 0 :capacity 50 :mount-point "/"))))
    (is (equal '("FILE" "LINES" "WORDS" "BYTES")
               (headers (make-instance 'wc-count :lines 1 :words 2 :bytes 3 :file "f"))))
    (is (equal '("BLOCKS" "PATH")
               (headers (make-instance 'du-entry :blocks 4 :path "."))))))

(test table-custom-columns-override
  (let ((out (with-output-to-string (s)
               (table (list (%fi "a" 10 "u"))
                      :columns (list (cons "SZ" #'file-size)) :stream s))))
    (is (search "SZ" out))
    (is (search "10" out))
    (is (not (search "NAME" out)))))

(test table-color-bolds-header-and-rule-only
  (let ((out (with-output-to-string (s)
               (table (list (%fi "a" 1 "u")) :stream s :color t))))
    ;; header + rule wrapped in bold SGR; the data row is not
    (is (search (format nil "~C[1mNAME" #\Escape) out))
    (is (search (format nil "~C[1m----" #\Escape) out))
    (is (not (search (format nil "~C[1ma " #\Escape) out)))))

;;; ---------------------------------------------------------------------------
;;; Presentation policy: PRESENT / %TABULAR-RESULT-P
;;; ---------------------------------------------------------------------------

(test tabular-result-p-recognizes-uniform-wrapped-streams
  ;; a wrapped type, singly or as a uniform list
  (is-true  (consh::%tabular-result-p (%fi "a" 1 "u")))
  (is-true  (consh::%tabular-result-p (list (%fi "a" 1 "u") (%fi "b" 2 "u"))))
  ;; bare values are not tabular — they keep the plain rendering
  (is-false (consh::%tabular-result-p (list "a" "b")))
  (is-false (consh::%tabular-result-p "hi"))
  (is-false (consh::%tabular-result-p 42))
  ;; a mixed stream falls back (first is wrapped, rest is not the same class)
  (is-false (consh::%tabular-result-p (list (%fi "a" 1 "u") "plain")))
  ;; empty and degenerate inputs are safe (never a table, never an error)
  (is-false (consh::%tabular-result-p '()))
  (is-false (consh::%tabular-result-p (cons 1 2))))

(test present-auto-tables-a-wrapped-stream
  (let ((out (with-output-to-string (s)
               (present (list (%fi "kernel.img" 4096 "mk") (%fi "notes.txt" 14 "mk")) s))))
    (is (search "NAME" out))
    (is (search "kernel.img  4096" out))
    ;; no bold escapes when *present-color* is off (the test default)
    (is (not (find #\Escape out)))))

(test present-prints-strings-one-per-line-unquoted
  (let ((out (with-output-to-string (s) (present (list "hello" "world") s))))
    (is (string= (format nil "hello~%world~%") out))))

(test present-mixed-stream-stays-per-line
  (let ((out (with-output-to-string (s) (present (list (%fi "a" 1 "u") "plain") s))))
    (is (not (search "NAME" out)))            ; not tabulated
    (is (search "FILE-INFO" out))             ; printed via print-object
    (is (search "plain" out))))

(test present-empty-list-prints-nothing
  (is (string= "" (with-output-to-string (s) (present '() s)))))

(test present-scalar-prints-readably
  (is (string= (format nil "~S~%" "hi") (with-output-to-string (s) (present "hi" s))))
  (is (string= (format nil "42~%")      (with-output-to-string (s) (present 42 s)))))

(test present-color-flag-bolds-the-header
  (let ((consh::*present-color* t))
    (let ((out (with-output-to-string (s) (present (list (%fi "a" 1 "u")) s))))
      (is (search (format nil "~C[1mNAME" #\Escape) out)))))
