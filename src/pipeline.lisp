;;;; pipeline.lisp — pipeline objects and the plumbing/fusion compiler (SPEC §3).
;;;;
;;;; A pipeline is DATA, not execution: `pipe` builds a pipeline object whose
;;;; plan can be inspected with `describe` before anything runs.  The compiler
;;;; here groups adjacent stages so the executor (exec.lisp) knows where the
;;;; kernel pipes, parse/unparse pump boundaries, and fused lisp workers go:
;;;;
;;;;   external -> external : one real pipe(2), kernel-to-kernel, no Lisp bytes
;;;;   external -> lisp     : parse pump (stdout -> parse-output -> channel)
;;;;   lisp     -> external : unparse pump (channel -> unparse-input -> stdin)
;;;;   lisp     -> lisp     : fuse (compose transducers, one thread) unless a
;;;;                          stage is :expensive or :parallel

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Stage classes
;;; ---------------------------------------------------------------------------

(defclass stage ()
  ((name :initarg :name :initform nil :reader stage-name)))

(defclass external-stage (stage)
  ((invocation :initarg :invocation :reader stage-invocation)
   (redirections :initarg :redirections :initform nil :reader stage-redirections
                 :documentation "An alist of (KIND . path): :in :out :out-append
:err :err-append.  From shell < > >> 2> 2>> redirections."))
  (:documentation "A stage that runs an external command."))

(defclass lisp-stage (stage)
  ((xform :initarg :xform :initform nil :reader stage-xform
          :documentation "A transducer: (emit) -> (lambda (object) ...).  NIL for
a generator (source) stage.")
   (generator :initarg :generator :initform nil :reader stage-generator
              :documentation "For a source stage: a function (emit) that emits
values imperatively, ignoring upstream input.  NIL for transducer stages.")
   (collector :initarg :collector :initform nil :reader stage-collector
              :documentation "For a BARRIER stage: a function (list) -> list.  The
worker collects the whole input into a list, calls it, and emits the results.
For stages that need the full stream (sort).  NIL otherwise.")
   (expensive :initarg :expensive :initform nil :reader stage-expensive-p)
   (parallel  :initarg :parallel  :initform nil :reader stage-parallel-p))
  (:documentation
   "An in-image stage transforming an object sequence.  A transducer stage is
represented so consecutive ones fuse into a single thread by composition; a
generator stage is a source that emits imperatively."))

(defun generator-stage-p (s) (and (lisp-stage-p s) (stage-generator s)))

(defun external-stage-p (s) (typep s 'external-stage))
(defun lisp-stage-p (s) (typep s 'lisp-stage))

;;; --- constructors ---

(defun external (program &rest args)
  "An external stage for PROGRAM (a namestring, or a ready-made invocation) and
ARGS.  A trailing `:redirections ALIST` is peeled off (the shell parser adds it)."
  (let ((redirs nil))
    (when (and (>= (length args) 2)
               (eq (nth (- (length args) 2) args) :redirections))
      (setf redirs (car (last args))
            args (butlast args 2)))
    (make-instance 'external-stage
                   :invocation (if (typep program 'command-invocation)
                                   program
                                   (apply #'make-invocation program args))
                   :redirections redirs
                   :name (if (typep program 'command-invocation)
                             (invocation-program program)
                             program))))

(defun map-stage (fn &key name expensive parallel)
  "A lisp stage mapping each object through FN."
  (make-instance 'lisp-stage :name (or name "map") :expensive expensive
                 :parallel parallel
                 :xform (lambda (emit) (lambda (x) (funcall emit (funcall fn x))))))

(defun filter-stage (pred &key name expensive parallel)
  "A lisp stage keeping objects satisfying PRED."
  (make-instance 'lisp-stage :name (or name "filter") :expensive expensive
                 :parallel parallel
                 :xform (lambda (emit) (lambda (x) (when (funcall pred x) (funcall emit x))))))

(defun mapcat-stage (fn &key name expensive parallel)
  "A lisp stage where FN returns a list of objects to emit for each input."
  (make-instance 'lisp-stage :name (or name "mapcat") :expensive expensive
                 :parallel parallel
                 :xform (lambda (emit) (lambda (x) (mapc emit (funcall fn x))))))

(defun emit-stage (fn &key name expensive parallel)
  "An imperative transform stage: FN is called as (FN input emit) for each input
object, and may call (funcall emit value) zero or more times.  The imperative
sibling of map/filter/mapcat — use it when what you emit depends on accumulated
state or arbitrary control flow.  Fuses like the other transducer stages."
  (make-instance 'lisp-stage :name (or name "emit") :expensive expensive
                 :parallel parallel
                 :xform (lambda (emit) (lambda (x) (funcall fn x emit)))))

(defun generator-stage (fn &key name expensive parallel)
  "A source stage: FN is called as (FN emit) once and may call (funcall emit
value) any number of times.  Ignores upstream input, so it heads a pipeline (or
group).  Backpressure still applies — emit is a channel put."
  (make-instance 'lisp-stage :name (or name "generate") :expensive expensive
                 :parallel parallel :generator fn))

(defun collector-stage (fn &key name)
  "A BARRIER stage: the worker collects the whole input stream into a list, calls
(FN list), and emits each element of the returned list.  For transforms that
need every object at once (sorting).  Always a group of its own (non-fusible)."
  (make-instance 'lisp-stage :name (or name "collect") :expensive t :collector fn))

;;; --- native, in-image replacements for external filters (SPEC §2 endgame) ---
;;; These are ordinary pipeline stages — no subprocess, no fork — that do what
;;; grep/cat/sort/uniq do, but over the object stream and in Lisp.

(defun %render-for-match (object)
  "The string a native grep matches against: strings as-is, else the object's
printed representation."
  (if (stringp object) object (princ-to-string object)))

(defun grep-stage (pattern &key (key #'%render-for-match) invert name)
  "Native grep: keep objects whose KEY projection contains the literal substring
PATTERN (or, with :INVERT, those that do not).  No subprocess."
  (filter-stage (lambda (x)
                  (let ((hit (search pattern (funcall key x))))
                    (if invert (not hit) hit)))
                :name (or name "grep*")))

(defun cat-stage (&rest files)
  "Native cat: a source stage emitting each line (a string) of FILES, resolved
against *current-directory*.  No subprocess."
  (generator-stage
   (lambda (emit)
     (dolist (f files)
       (with-open-file (in (merge-pathnames f *current-directory*) :direction :input)
         (loop for line = (read-line in nil nil) while line do (funcall emit line)))))
   :name "cat*"))

(defun %generic-lessp (a b)
  "A total-ish order usable across the common object-stream element types:
numbers numerically, strings lexically, otherwise by printed representation."
  (cond ((and (realp a) (realp b)) (< a b))
        ((and (stringp a) (stringp b)) (string< a b))
        (t (string< (princ-to-string a) (princ-to-string b)))))

(defun sort-stage (&key (key #'identity) (test #'%generic-lessp) name)
  "Native, object-aware sort: order the whole stream by (KEY object) under TEST.
A barrier — it buffers the stream.  `(:sort :key #'file-size)` sorts file objects
by size numerically, not by their text."
  (collector-stage (lambda (items) (stable-sort (copy-list items) test :key key))
                   :name (or name "sort")))

(defun uniq-stage (&key (key #'identity) (test #'equal) name)
  "Native uniq: drop ADJACENT duplicates (like uniq(1)) comparing (KEY object)
under TEST (EQUAL by default, so equal strings/numbers collapse).  Streaming —
state lives in the transducer instance, fresh per run."
  (make-instance 'lisp-stage :name (or name "uniq")
                 :xform (lambda (emit)
                          (let ((first t) (previous nil))
                            (lambda (x)
                              (let ((k (funcall key x)))
                                (when (or first (not (funcall test k previous)))
                                  (funcall emit x))
                                (setf first nil previous k)))))))

;;; ---------------------------------------------------------------------------
;;; Pipeline
;;; ---------------------------------------------------------------------------

(defclass pipeline ()
  ((stages :initarg :stages :reader pipeline-stages)))

(defun make-pipeline (stages)
  (assert stages () "A pipeline needs at least one stage.")
  (make-instance 'pipeline :stages (coerce stages 'list)))

(defmethod print-object ((p pipeline) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~{~A~^ | ~}"
            (mapcar (lambda (s) (or (stage-name s) "?")) (pipeline-stages p)))))

;;; --- the pipe macro (surface sugar over stage constructors) ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-pipe-clause (clause)
    "Expand one `pipe` clause into a stage-constructing form.
  (cmd arg...)        -> external command named CMD (a symbol)
  (:map f ...keys)        -> map-stage
  (:filter p ...keys)     -> filter-stage
  (:mapcat f ...keys)     -> mapcat-stage
  (:emit (x emit) body)   -> emit-stage      (imperative per-object)
  (:generate (emit) body) -> generator-stage (imperative source)
  (:external p a...)      -> external with an explicit program string"
    (cond
      ((and (consp clause) (keywordp (car clause)))
       (ecase (car clause)
         (:map    `(map-stage    ,(second clause) ,@(cddr clause)))
         (:filter `(filter-stage ,(second clause) ,@(cddr clause)))
         (:mapcat `(mapcat-stage ,(second clause) ,@(cddr clause)))
         (:emit     `(emit-stage      (lambda ,(second clause) ,@(cddr clause))))
         (:generate `(generator-stage (lambda ,(second clause) ,@(cddr clause))))
         ;; native, in-image filters (no subprocess)
         (:grep `(grep-stage ,(second clause) ,@(cddr clause)))
         (:cat  `(cat-stage ,@(rest clause)))
         (:sort `(sort-stage ,@(rest clause)))
         (:uniq `(uniq-stage ,@(rest clause)))
         (:external `(external ,@(rest clause)))))
      ((and (consp clause) (symbolp (car clause)))
       `(external ,(string-downcase (symbol-name (car clause))) ,@(rest clause)))
      (t (error "malformed pipe clause: ~S" clause)))))

(defmacro pipe (&rest clauses)
  "Build a pipeline object (data, not execution) from CLAUSES.  See
%expand-pipe-clause for clause syntax."
  `(make-pipeline (list ,@(mapcar #'%expand-pipe-clause clauses))))

;;; ---------------------------------------------------------------------------
;;; Grouping / plumbing analysis
;;; ---------------------------------------------------------------------------

(defun pipeline-groups (pipeline)
  "Partition the stages into execution groups, each (KIND . STAGES):
  (:external e1 e2 ...) a maximal run of external stages — internally connected
                        by kernel pipes, one shared pgid;
  (:lisp l1 l2 ...)     a maximal run of FUSIBLE lisp stages — one worker with a
                        composed transducer.  An :expensive or :parallel lisp
                        stage always starts its own group (its own thread)."
  (let ((groups '()) (current '()) (current-kind nil))
    (flet ((flush ()
             (when current
               (push (cons current-kind (nreverse current)) groups)
               (setf current '() current-kind nil))))
      (dolist (s (pipeline-stages pipeline))
        (etypecase s
          (external-stage
           (unless (eq current-kind :external) (flush) (setf current-kind :external))
           (push s current))
          (lisp-stage
           (cond
             ;; a barrier stage stands alone
             ((or (stage-expensive-p s) (stage-parallel-p s))
              (flush)
              (push (list :lisp s) groups))
             ;; a generator (source) starts a fresh lisp group it heads; later
             ;; fusible transducers append to it
             ((stage-generator s)
              (flush)
              (setf current-kind :lisp)
              (push s current))
             (t
              (unless (eq current-kind :lisp) (flush) (setf current-kind :lisp))
              (push s current))))))
      (flush))
    (nreverse groups)))

(defun %boundary-kind (from-kind to-kind)
  (cond ((and (eq from-kind :external) (eq to-kind :lisp)) :parse)
        ((and (eq from-kind :lisp) (eq to-kind :external)) :unparse)
        (t :channel)))                                    ; lisp -> lisp (split)

(defun pipeline-plan (pipeline)
  "A structured description of how PIPELINE will run: a plist with :groups (each
:kind, :stages names, and internal :links) and :boundaries between groups."
  (let* ((groups (pipeline-groups pipeline))
         (group-descs
           (mapcar (lambda (g)
                     (list :kind (car g)
                           :stages (mapcar (lambda (s) (or (stage-name s) "?")) (cdr g))
                           :internal-links
                           (if (eq (car g) :external)
                               (make-list (max 0 (1- (length (cdr g))))
                                          :initial-element :kernel-pipe)
                               '())))
                   groups))
         (boundaries
           (loop for (a b) on groups
                 while b
                 collect (%boundary-kind (car a) (car b)))))
    (list :groups group-descs :boundaries boundaries
          :head (if (eq (caar groups) :external) :stdin :source)
          :tail (if (eq (car (car (last groups))) :external) :parse :channel))))

(defmethod describe-object ((p pipeline) stream)
  (let ((plan (pipeline-plan p)))
    (format stream "~&Pipeline of ~D stage~:P:~%" (length (pipeline-stages p)))
    (loop for (g . rest) on (getf plan :groups)
          for i from 0
          do (format stream "  [~(~A~)] ~{~A~^ -> ~}~@[  (~{~A~^, ~})~]~%"
                     (getf g :kind) (getf g :stages)
                     (when (getf g :internal-links)
                       (mapcar #'string-downcase
                               (mapcar #'symbol-name (getf g :internal-links)))))
             (when rest
               (format stream "      == ~(~A~) boundary ==~%"
                       (nth i (getf plan :boundaries)))))
    (format stream "  head: ~(~A~)   tail: ~(~A~)~%"
            (getf plan :head) (getf plan :tail))))

;;; --- transducer fusion ---

(defun compose-xforms (stages)
  "Compose the transducers of lisp STAGES left-to-right into a single one, so a
run of fused stages executes in one thread with no channels between them."
  (let ((xforms (mapcar #'stage-xform stages)))
    (lambda (emit)
      (reduce (lambda (downstream xf) (funcall xf downstream))
              (reverse xforms)
              :initial-value emit))))
