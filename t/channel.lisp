;;;; t/channel.lisp — Phase 2: bounded object channels.

(in-package #:consh/test)

(def-suite channel :in consh :description "Bounded object channels.")
(in-suite channel)

;;; ---------------------------------------------------------------------------
;;; Basics: identity-preserving FIFO, introspection
;;; ---------------------------------------------------------------------------

(test carries-objects-by-identity
  "A channel carries the very object put in, not a copy (objects, not bytes)."
  (let ((ch (make-channel))
        (obj (list 1 2 3)))
    (channel-put ch obj)
    (is (eq obj (channel-take ch)))))

(test fifo-order
  "Objects come out in the order they went in."
  (let ((ch (make-channel :capacity 8)))
    (dotimes (i 5) (channel-put ch i))
    (is (equal '(0 1 2 3 4) (loop repeat 5 collect (channel-take ch))))))

(test ring-buffer-wraparound
  "Interleaved put/take past capacity wraps the ring correctly."
  (let ((ch (make-channel :capacity 3))
        (out '()))
    (dotimes (i 10)
      (channel-put ch i)
      (push (channel-take ch) out))
    (is (equal (loop for i below 10 collect i) (nreverse out)))
    (is (channel-empty-p ch))))

(test introspection
  (let ((ch (make-channel :capacity 4)))
    (is (channelp ch))
    (is (= 4 (channel-capacity ch)))
    (is (channel-empty-p ch))
    (is-false (channel-full-p ch))
    (channel-put ch :a)
    (channel-put ch :b)
    (is (= 2 (channel-count ch)))
    (channel-put ch :c)
    (channel-put ch :d)
    (is (channel-full-p ch))
    (is-false (channel-writer-closed-p ch))
    (is-false (channel-reader-closed-p ch))))

;;; ---------------------------------------------------------------------------
;;; Backpressure: producer blocks at capacity
;;; ---------------------------------------------------------------------------

(test producer-blocks-at-capacity
  "channel-put on a full channel blocks until a slot frees, then completes."
  (let* ((ch (make-channel :capacity 2))
         (done nil))
    (channel-put ch :a)
    (channel-put ch :b)                    ; now full
    (is (= 2 (channel-count ch)))
    (let ((th (sb-thread:make-thread
               (lambda () (channel-put ch :c) (setf done t))
               :name "blocked-producer")))
      (sleep 0.2)
      (is (null done))                     ; parked on a full channel
      (is (sb-thread:thread-alive-p th))
      (is (eq :a (channel-take ch)))       ; free one slot
      (sb-thread:join-thread th)
      (is-true done)
      (is (= 2 (channel-count ch))))))     ; :b, :c

;;; ---------------------------------------------------------------------------
;;; EOF: take-after-close returns the sentinel
;;; ---------------------------------------------------------------------------

(test take-after-close-returns-eof
  "close-channel lets takers drain remaining objects, then yields EOF forever."
  (let ((ch (make-channel :capacity 4)))
    (channel-put ch 1)
    (channel-put ch 2)
    (close-channel ch)
    (is (= 1 (channel-take ch)))           ; buffered objects drain first
    (is (= 2 (channel-take ch)))
    (is (eof-p (channel-take ch)))         ; then EOF
    (is (eof-p (channel-take ch)))))       ; and stays EOF (idempotent)

(test close-is-idempotent
  (let ((ch (make-channel)))
    (close-channel ch)
    (close-channel ch)
    (is (channel-writer-closed-p ch))
    (is (eof-p (channel-take ch)))))

(test close-wakes-blocked-taker
  "A taker blocked on an empty channel is released with EOF when the writer
closes."
  (let* ((ch (make-channel))
         (result :none)
         (th (sb-thread:make-thread
              (lambda () (setf result (channel-take ch)))
              :name "blocked-taker")))
    (sleep 0.2)
    (is (eq :none result))                 ; still blocked, nothing to take
    (close-channel ch)
    (sb-thread:join-thread th)
    (is (eof-p result))))

;;; ---------------------------------------------------------------------------
;;; Downstream cancellation: put-after-close-for-reading signals
;;; ---------------------------------------------------------------------------

(test put-after-close-for-reading-signals
  (let ((ch (make-channel :capacity 4)))
    (close-for-reading ch)
    (signals channel-closed (channel-put ch 1))))

(test put-after-writer-close-signals
  (let ((ch (make-channel)))
    (close-channel ch)
    (signals channel-closed (channel-put ch 1))))

(test close-for-reading-reports-reason
  "The condition distinguishes downstream cancellation from writer close."
  (let ((ch (make-channel)))
    (close-for-reading ch)
    (handler-case (channel-put ch 1)
      (channel-closed (c)
        (is (eq :reader-closed (channel-closed-reason c)))
        (is (eql 1 (channel-closed-object c)))
        (is (eq ch (channel-closed-channel c)))))))

(test close-for-reading-wakes-blocked-putter
  "A producer blocked on a full channel is released with CHANNEL-CLOSED when
the consumer cancels."
  (let* ((ch (make-channel :capacity 1))
         (caught nil))
    (channel-put ch :x)                     ; full
    (let ((th (sb-thread:make-thread
               (lambda ()
                 (handler-case (channel-put ch :y)
                   (channel-closed (c) (setf caught c))))
               :name "blocked-producer-cancel")))
      (sleep 0.2)
      (is (null caught))                     ; still blocked
      (close-for-reading ch)
      (sb-thread:join-thread th)
      (is (typep caught 'channel-closed))
      (is (eq :reader-closed (channel-closed-reason caught))))))

(test close-for-reading-drops-buffer-and-yields-eof
  "After cancellation the buffer is emptied and takes see EOF."
  (let ((ch (make-channel :capacity 4)))
    (channel-put ch 1)
    (channel-put ch 2)
    (close-for-reading ch)
    (is (= 0 (channel-count ch)))
    (is (eof-p (channel-take ch)))))

;;; ---------------------------------------------------------------------------
;;; Stop-flag parking (the C-z analogue)
;;; ---------------------------------------------------------------------------

(test stop-flag-parks-and-resumes
  "A paused stop-flag parks a thread at its next channel op; resume releases it."
  (let* ((sf (make-stop-flag))
         (ch (make-channel :capacity 4 :stop-flag sf))
         (done nil))
    (stop-flag-pause sf)
    (is (stop-flag-paused-p sf))
    (let ((th (sb-thread:make-thread
               (lambda () (channel-put ch :v) (setf done t))
               :name "parked-worker")))
      (sleep 0.2)
      (is (null done))                       ; parked by the stop-flag
      (is (sb-thread:thread-alive-p th))
      (stop-flag-resume sf)
      (sb-thread:join-thread th)
      (is-true done)
      (is (= 1 (channel-count ch))))))

(test stop-flag-inert-when-not-paused
  "A channel with a stop-flag behaves normally while not paused."
  (let* ((sf (make-stop-flag))
         (ch (make-channel :stop-flag sf)))
    (channel-put ch 41)
    (is (= 41 (channel-take ch)))))

;;; ---------------------------------------------------------------------------
;;; The headline acceptance test: 100k-object ping-pong, bounded memory
;;; ---------------------------------------------------------------------------

(test ping-pong-100k-bounded
  "Two threads bounce 100k objects across a pair of small-capacity channels.
Every value round-trips exactly once and, because both channels are bounded,
memory stays flat throughout (the test simply completing proves it)."
  (let* ((n 100000)
         (cap 64)
         (req (make-channel :capacity cap))
         (resp (make-channel :capacity cap))
         (ponger (sb-thread:make-thread
                  (lambda ()
                    ;; echo each request back until the writer closes req
                    (loop for v = (channel-take req)
                          until (eof-p v)
                          do (channel-put resp v))
                    (close-channel resp))
                  :name "ponger")))
    (let ((sum 0)
          (max-seen 0))
      (dotimes (i n)
        (channel-put req i)
        (let ((r (channel-take resp)))
          (incf sum r))
        ;; sample the depth: it can never exceed capacity
        (setf max-seen (max max-seen (channel-count req))))
      (close-channel req)
      (sb-thread:join-thread ponger)
      (is (= sum (/ (* n (1- n)) 2)))        ; every value came back exactly once
      (is (<= max-seen cap))                 ; bounded: never over capacity
      (is (eof-p (channel-take resp))))))    ; ponger closed resp on its way out

;;; ---------------------------------------------------------------------------
;;; Concurrency stress: fan-out to multiple consumers, nothing lost/duplicated
;;; ---------------------------------------------------------------------------

(test fan-out-no-loss-no-duplication
  "One producer, several consumers: the multiset of consumed values equals the
produced values — condition-notify wakes exactly the right waiter each time."
  (let* ((n 20000)
         (consumers 4)
         (ch (make-channel :capacity 128))
         (lock (sb-thread:make-mutex))
         (total-count 0)
         (total-sum 0)
         (threads
           (loop repeat consumers
                 collect (sb-thread:make-thread
                          (lambda ()
                            (let ((c 0) (s 0))
                              (loop for v = (channel-take ch)
                                    until (eof-p v)
                                    do (incf c) (incf s v))
                              (sb-thread:with-mutex (lock)
                                (incf total-count c)
                                (incf total-sum s))))
                          :name "consumer"))))
    (dotimes (i n) (channel-put ch i))
    (close-channel ch)
    (mapc #'sb-thread:join-thread threads)
    (is (= n total-count))                          ; every object consumed once
    (is (= (/ (* n (1- n)) 2) total-sum))))         ; and none corrupted/dropped

;;; ---------------------------------------------------------------------------
;;; NIL is an ordinary value — never confused with "empty"
;;; ---------------------------------------------------------------------------

(test nil-is-a-real-value
  "Putting NIL stores a value; the channel is not empty and NIL is not EOF."
  (let ((ch (make-channel :capacity 2)))
    (channel-put ch nil)
    (is-false (channel-empty-p ch))
    (is (= 1 (channel-count ch)))
    (let ((v (channel-take ch)))
      (is (null v))
      (is-false (eof-p v)))
    (is (channel-empty-p ch))))

(test eof-p-only-true-for-sentinel
  "eof-p distinguishes the sentinel from every ordinary value, including NIL."
  (is-true (eof-p +channel-eof+))
  (dolist (v (list nil t 0 :eof "eof" '(eof) (make-symbol "CHANNEL-EOF")))
    (is-false (eof-p v))))

;;; ---------------------------------------------------------------------------
;;; Waking a blocked taker with data (not close): it gets the value
;;; ---------------------------------------------------------------------------

(test data-wakes-blocked-taker
  "A taker blocked on an empty channel is released with the value once a
producer puts one — the not-empty path, distinct from the EOF path."
  (let* ((ch (make-channel))
         (result :none)
         (th (sb-thread:make-thread
              (lambda () (setf result (channel-take ch)))
              :name "blocked-taker-data")))
    (sleep 0.2)
    (is (eq :none result))
    (channel-put ch 99)
    (sb-thread:join-thread th)
    (is (eql 99 result))
    (is-false (eof-p result))))

;;; ---------------------------------------------------------------------------
;;; FIFO order survives a blocking producer
;;; ---------------------------------------------------------------------------

(test fifo-preserved-across-blocked-producer
  "A value parked in a blocked producer takes its correct FIFO position once a
slot frees."
  (let ((ch (make-channel :capacity 2))
        (order '()))
    (channel-put ch 'a)
    (channel-put ch 'b)                        ; full
    (let ((th (sb-thread:make-thread
               (lambda () (channel-put ch 'c)) ; blocks until a slot frees
               :name "blocked-producer-order")))
      (sleep 0.1)
      (push (channel-take ch) order)           ; a  -> frees a slot, c lands
      (sb-thread:join-thread th)
      (push (channel-take ch) order)           ; b
      (push (channel-take ch) order)           ; c
      (is (equal '(a b c) (nreverse order))))))

;;; ---------------------------------------------------------------------------
;;; Capacity edges
;;; ---------------------------------------------------------------------------

(test capacity-one-lockstep
  "A capacity-1 channel wraps head/tail every op and stays strictly lock-step."
  (let ((ch (make-channel :capacity 1)))
    (channel-put ch :only)
    (is (channel-full-p ch))
    (is (eq :only (channel-take ch)))
    (is (channel-empty-p ch))
    (dotimes (i 200)                            ; many wraparounds through slot 0
      (channel-put ch i)
      (is (= i (channel-take ch))))
    (is (channel-empty-p ch))))

(test capacity-must-be-positive
  "make-channel rejects a non-positive capacity."
  (signals error (make-channel :capacity 0))
  (signals error (make-channel :capacity -5)))

(test drains-large-buffer-in-order-before-eof
  "Every buffered object is delivered in order after close, then EOF."
  (let ((ch (make-channel :capacity 100)))
    (dotimes (i 100) (channel-put ch i))
    (close-channel ch)
    (is (equal (loop for i below 100 collect i)
               (loop repeat 100 collect (channel-take ch))))
    (is (eof-p (channel-take ch)))))

;;; ---------------------------------------------------------------------------
;;; close-channel racing a blocked producer -> :writer-closed
;;; ---------------------------------------------------------------------------

(test close-channel-wakes-blocked-producer
  "Closing the writer end releases a producer blocked on a full channel with a
CHANNEL-CLOSED whose reason is :writer-closed."
  (let* ((ch (make-channel :capacity 1))
         (caught nil))
    (channel-put ch :x)                          ; full
    (let ((th (sb-thread:make-thread
               (lambda ()
                 (handler-case (channel-put ch :y)
                   (channel-closed (c) (setf caught c))))
               :name "blocked-producer-wclose")))
      (sleep 0.2)
      (is (null caught))
      (close-channel ch)
      (sb-thread:join-thread th)
      (is (typep caught 'channel-closed))
      (is (eq :writer-closed (channel-closed-reason caught))))))

;;; ---------------------------------------------------------------------------
;;; Broadcast on close wakes ALL blocked threads
;;; ---------------------------------------------------------------------------

(test close-for-reading-wakes-all-blocked-producers
  "close-for-reading releases every producer blocked on a full channel."
  (let* ((ch (make-channel :capacity 1))
         (n 5)
         (lock (sb-thread:make-mutex))
         (caught 0))
    (channel-put ch :fill)                       ; full; all producers will block
    (let ((threads
            (loop repeat n
                  collect (sb-thread:make-thread
                           (lambda ()
                             (handler-case (channel-put ch :v)
                               (channel-closed ()
                                 (sb-thread:with-mutex (lock) (incf caught)))))
                           :name "blocked-producer-broadcast"))))
      (sleep 0.3)
      (is (= 0 caught))                          ; all still parked
      (close-for-reading ch)
      (mapc #'sb-thread:join-thread threads)
      (is (= n caught)))))                        ; every one released with the condition

(test close-wakes-all-blocked-takers
  "close-channel releases every taker blocked on an empty channel with EOF."
  (let* ((ch (make-channel))
         (n 5)
         (lock (sb-thread:make-mutex))
         (eofs 0))
    (let ((threads
            (loop repeat n
                  collect (sb-thread:make-thread
                           (lambda ()
                             (when (eof-p (channel-take ch))
                               (sb-thread:with-mutex (lock) (incf eofs))))
                           :name "blocked-taker-broadcast"))))
      (sleep 0.3)
      (is (= 0 eofs))
      (close-channel ch)
      (mapc #'sb-thread:join-thread threads)
      (is (= n eofs)))))

;;; ---------------------------------------------------------------------------
;;; Interaction of the two closes; idempotence
;;; ---------------------------------------------------------------------------

(test close-for-reading-idempotent
  (let ((ch (make-channel)))
    (close-for-reading ch)
    (close-for-reading ch)
    (is (channel-reader-closed-p ch))
    (signals channel-closed (channel-put ch 1))
    (is (eof-p (channel-take ch)))))

(test writer-close-then-reader-close
  "Writer close then reader close: buffered data is dropped, takes see EOF,
puts are rejected as :reader-closed (reader close takes precedence)."
  (let ((ch (make-channel :capacity 4)))
    (channel-put ch 1)
    (channel-put ch 2)
    (close-channel ch)
    (close-for-reading ch)
    (is (= 0 (channel-count ch)))
    (is (eof-p (channel-take ch)))
    (handler-case (progn (channel-put ch 3) (fail "expected channel-closed"))
      (channel-closed (c) (is (eq :reader-closed (channel-closed-reason c)))))))

(test reader-close-then-writer-close
  "Reader close then writer close: takes see EOF, puts stay rejected."
  (let ((ch (make-channel)))
    (close-for-reading ch)
    (close-channel ch)
    (is (channel-reader-closed-p ch))
    (is (channel-writer-closed-p ch))
    (is (eof-p (channel-take ch)))
    (signals channel-closed (channel-put ch 1))))

;;; ---------------------------------------------------------------------------
;;; Stop-flag: parks TAKE too, and works across shared channels
;;; ---------------------------------------------------------------------------

(test stop-flag-parks-take
  "A paused stop-flag parks a taker even when data is available; resume delivers
the value."
  (let* ((sf (make-stop-flag))
         (ch (make-channel :stop-flag sf))
         (got :none))
    (channel-put ch 7)                           ; data is ready
    (stop-flag-pause sf)
    (let ((th (sb-thread:make-thread
               (lambda () (setf got (channel-take ch)))
               :name "parked-taker")))
      (sleep 0.2)
      (is (eq :none got))                        ; parked despite data present
      (stop-flag-resume sf)
      (sb-thread:join-thread th)
      (is (eql 7 got)))))

(test stop-flag-shared-across-channels
  "One stop-flag parks ops on every channel that references it."
  (let* ((sf (make-stop-flag))
         (a (make-channel :stop-flag sf))
         (b (make-channel :stop-flag sf))
         (da nil) (db nil))
    (stop-flag-pause sf)
    (let ((ta (sb-thread:make-thread
               (lambda () (channel-put a 1) (setf da t)) :name "park-a"))
          (tb (sb-thread:make-thread
               (lambda () (channel-put b 2) (setf db t)) :name "park-b")))
      (sleep 0.2)
      (is (null da))
      (is (null db))
      (stop-flag-resume sf)
      (sb-thread:join-thread ta)
      (sb-thread:join-thread tb)
      (is-true da)
      (is-true db)
      (is (= 1 (channel-count a)))
      (is (= 1 (channel-count b))))))

;;; ---------------------------------------------------------------------------
;;; Full M:N stress — many producers, many consumers, nothing lost or duplicated
;;; ---------------------------------------------------------------------------

(test many-to-many-no-loss-no-duplication
  "N producers over disjoint ranges, M consumers; a coordinator closes once all
producers finish.  The consumed multiset must equal the produced one."
  (let* ((per 5000)
         (producers 4)
         (consumers 4)
         (n (* per producers))
         (ch (make-channel :capacity 128))
         (lock (sb-thread:make-mutex))
         (total-count 0)
         (total-sum 0)
         (cs (loop repeat consumers
                   collect (sb-thread:make-thread
                            (lambda ()
                              (let ((c 0) (s 0))
                                (loop for v = (channel-take ch)
                                      until (eof-p v)
                                      do (incf c) (incf s v))
                                (sb-thread:with-mutex (lock)
                                  (incf total-count c)
                                  (incf total-sum s))))
                            :name "mn-consumer")))
         (ps (loop for p below producers
                   collect (let ((lo (* p per)) (hi (* (1+ p) per)))
                             (sb-thread:make-thread
                              (lambda () (loop for i from lo below hi
                                               do (channel-put ch i)))
                              :name "mn-producer")))))
    (mapc #'sb-thread:join-thread ps)            ; all produced
    (close-channel ch)                           ; coordinator signals EOF once
    (mapc #'sb-thread:join-thread cs)
    (is (= n total-count))                       ; every object consumed exactly once
    (is (= (/ (* n (1- n)) 2) total-sum))        ; and none corrupted or dropped
    (is (channel-empty-p ch))))
