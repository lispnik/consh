;;;; wrappers/lsblk.lisp — lsblk: request JSON, parse into block-device objects.
;;;;
;;;; The top of SPEC §2's format-preference ladder is JSON flags.  lsblk has one
;;;; (`-J` / `--json`), so the wrapper rewrites `lsblk` to request it and parses
;;;; the whole document — `{"blockdevices": [...]}` — into block-device objects,
;;;; nesting `children` recursively.  Unlike the line-oriented wrappers this
;;;; consumes the entire stream as one JSON value (via com.inuoe.jzon).

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; The object
;;; ---------------------------------------------------------------------------

(defclass block-device ()
  ((name       :initarg :name       :reader block-device-name)
   (size       :initarg :size       :reader block-device-size)
   (type       :initarg :type       :reader block-device-type)
   (mountpoint :initarg :mountpoint :initform nil :reader block-device-mountpoint)
   (children   :initarg :children   :initform nil :reader block-device-children))
  (:documentation "A node of `lsblk --json` output."))

(defmethod print-object ((d block-device) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~A ~A ~A~@[ ~A~]~@[ +~D~]"
            (block-device-name d) (block-device-type d) (block-device-size d)
            (block-device-mountpoint d)
            (and (block-device-children d) (length (block-device-children d))))))

;;; ---------------------------------------------------------------------------
;;; The invocation
;;; ---------------------------------------------------------------------------

(defclass lsblk-invocation (command-invocation) ()
  (:documentation "An `lsblk` call.  Yields block-device objects."))

(defmethod rewrite-invocation ((c lsblk-invocation))
  (if (flag-present-p (invocation-arguments c) "-J" "--json")
      c
      (make-instance 'lsblk-invocation :program "lsblk"
                     :arguments (append (invocation-arguments c) '("-J")))))

(defun %json-string-or-nil (value)
  "A JSON string value, or NIL for JSON null (jzon's null is a symbol)."
  (and (stringp value) value))

(defun %device-from-json (ht)
  (make-instance 'block-device
                 :name (gethash "name" ht)
                 :size (gethash "size" ht)
                 :type (gethash "type" ht)
                 :mountpoint (%json-string-or-nil (gethash "mountpoint" ht))
                 :children (let ((kids (gethash "children" ht)))
                             (and (vectorp kids)
                                  (map 'list #'%device-from-json kids)))))

(defun %lsblk-devices (command text)
  "Parse TEXT (a full `lsblk --json` document) into a list of block-devices, or
signal PARSE-ERROR."
  (let ((doc (handler-case (com.inuoe.jzon:parse text)
               (error () (signal-parse-error command text)))))
    (let ((devices (and (hash-table-p doc) (gethash "blockdevices" doc))))
      (if (vectorp devices)
          (map 'list #'%device-from-json devices)
          (signal-parse-error command text)))))

(defmethod parse-output ((c lsblk-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (emitting (emit :on-parse-error on-parse-error)
    (let* ((text (slurp-stream stream))              ; the whole JSON document
           (result (parse-record c text (lambda () (%lsblk-devices c text)))))
      ;; a successful parse yields a list of block-devices; use-raw-lines
      ;; recovery yields TEXT (a string), which we emit as-is
      (if (listp result)
          (dolist (device result) (funcall emit device))
          (funcall emit result)))))

(register-wrapper "lsblk" 'lsblk-invocation)
