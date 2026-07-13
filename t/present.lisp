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
