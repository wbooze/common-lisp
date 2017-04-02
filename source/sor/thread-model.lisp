(defpackage "THREADS"
  (:use :CL)
  (:export "CREATE-COND" "CREATE-MUTEX" "LOCK-MUTEX" "RELEASE-MUTEX"
           "MUTEX-LOCKED-P"
           "WAIT-ON-COND" "AWAKEN-COND-WAITERS" "SPAWN-THREAD" "DESTROY-THREAD"
           "WITH-MUTEX-LOCK" "SYNCHRONIZED-BARRIER" "BARRIER-IN-USE"
           "BARRIER-SET-NUM-THREADS" "BARRIER-SET-PRE-BLOCK-CALLBACK"
           "BARRIER-SET-POST-BLOCK-CALLBACK" "BARRIER-SET-LOCK-CONDITION"
           "LOCK-BARRIER" "UNLOCK-BARRIER" "SYNCHRONIZE-AT-BARRIER"))

(in-package "THREADS")


#+clisp (error "Presently, clisp does not support condition objects")
#+gcl (error "Presently, gcl does not support multi-threading")


;; Some threading primitives.  When there is no thread support in the
;; compiling environment, they become no-ops, except for spawn-thread,
;; which returns an error

(defun create-cond ()
  #+(and sbcl sb-thread) (sb-thread:make-waitqueue)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of cond creation for this platform"))

(defun create-mutex ()
  #+(and sbcl sb-thread) (sb-thread:make-mutex :value nil)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of mutex creation for this platform"))


(defun lock-mutex (mutex)
  #+(and sbcl sb-thread) (sb-thread:get-mutex mutex)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of mutex-locking for this platform"))

(defun release-mutex (mutex)
  #+(and sbcl sb-thread) (sb-thread:release-mutex mutex)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of mutex-unlocking for this platform"))

(defun mutex-locked-p (mutex)
  #+(and sbcl sb-thread) (sb-thread:mutex-value mutex)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of mutex probing for this platform"))

(defmacro wait-on-cond (cond locked-mutex &key (test t))
  "Wait on a condition variable.  If the :test keyword is supplied, it
is taken as a test which must evaluate to non-nil before the wait will
return.  It is possible for threads to be awakened by other than the
condition being true, so if you are not using a :test you will
probably have to perform a manual test in a surrounding context.  Note
that if the test is true upon entry, no wait will occur."
  #+(and sbcl sb-thread) `(do ()
                           (,test)
                           (sb-thread:condition-wait ,cond ,locked-mutex))
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of condition waits for this platform"))


(defun awaken-cond-waiters (cond)
  #+(and sbcl sb-thread) (sb-thread:condition-broadcast cond)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of condition awaken for this platform"))



(defparameter *spawn-mutex* (create-mutex))
(defparameter *spawn-cond* (create-cond))
(defparameter *spawn-ready* nil)

(defun spawn-thread (fcn &rest args)
  ;; Note about the sbcl implementation: a spawned thread may not
  ;; actually get cycles right away, and it doesn't copy its
  ;; environment before it starts executing.  If the calling
  ;; environment changes before the spawned thread starts up, the
  ;; thread will see the state at the time the thread started running,
  ;; not at the time of the 'spawn-thread' call.  To avoid confusion
  ;; from this race condition, in-parameters should be copied into a
  ;; block surrounding the call to 'spawn-thread' (probably via a LET
  ;; form)
  #+(and sbcl sb-thread) (sb-thread:make-thread #'(lambda ()
                                                    (apply fcn args)))
  #+(and sbcl (not sb-thread)) (error "Cannot call 'spawn-thread' because the environment does not support threads.")
  #-(or sbcl) (error "We need an implementation of thread starting for this platform"))

(defun destroy-thread (thread)
  #+(and sbcl sb-thread) (sb-thread:terminate-thread thread)
  #+(and sbcl (not sb-thread)) nil
  #-(or sbcl) (error "We need an implementation of thread destroying for this platform"))


(defmacro with-mutex-lock ((mutex) &body body)
  "Evaluates the body in a locked context.  Uses an unwind-protect
form to unlock the mutex no matter how the body is exited.  Returns
the value of the body."
  `(let ((rval (gensym)))
    (lock-mutex ,mutex)
    (setf rval (unwind-protect (progn ,@body)
                 (release-mutex ,mutex)))
    rval))


(defclass synchronized-barrier ()
  ((num-threads		:initarg :number-of-threads
                        :documentation "The number of threads that will be using this barrier."
                        :initform 1)
   (callback1		:initarg :pre-block-callback
                        :documentation "The callback that will be called once by a single thread before the last thread blocks."
                        :initform nil)
   (callback2		:initarg :post-block-callback
                        :documentation "The callback that, as the threads become unblocked, will be called once by a single thread before redispatching all blocked threads."
                        :initform nil)
   (locked		:initarg :locked
                        :initform nil
                        :documentation "If locked, no threads will exit the barrier until it has been unlocked.")
   (lock-condition	:initform nil
                        :documentation "Set this to a test that will, if true, block all threads by activating the 'locked' state.")
   (n-blocked		:initform 0
                        :documentation "The number of threads blocked waiting for all threads to reach the barrier.")
   (permission		:initform nil
                        :documentation "When non-nil, threads are allowed to leave the blocking queue.")
   (n-outgoing		:initform 0
                        :documentation "The number of threads that have not yet been dispatched after the barrier was cleared.  Needed to protect against a race condition.")
   (mutex		:initform (create-mutex))
   (cond		:initform (create-cond))
   (ever-entered	:initform nil
                        :documentation "Some functions may not be called once the barrier has come into active use."))

  (:documentation "An object intended to be shared between a known
number 'N' of threads.  Threads that enter via the
'synchronize-at-barrier' function will block until 'N' threads have
arrived.  At this point, exactly one thread will execute the
preparation callback function (if defined).  When that thread returns,
all threads of execution will be released to go on their way, and the
barrier will be reset.  A barrier can also be locked, in which case
all entering threads are blocked until unlocked by an external
agency.")
  )

(defmethod initialize-instance :after ((obj synchronized-barrier) &key)
  (let ((n-threads (slot-value obj 'num-threads)))
    (assert (and (numberp n-threads)
                 (> n-threads 0)))))
  

(define-condition barrier-in-use () 
  ((text	:reader get-text
                :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~A~%" (get-text condition)))))

(defun barrier-set-num-threads (barrier n)
  (assert (and (numberp n)
               (> n 0)))
  (with-mutex-lock ((slot-value barrier 'mutex))
    (when (slot-value barrier 'ever-entered)
      (make-condition 'barrier-in-use :message "Cannot set the number of threads on a barrier which has ever been entered."))
    (setf (slot-value barrier 'num-threads) n)))
      

(defun barrier-set-pre-block-callback (barrier cb)
  (with-mutex-lock ((slot-value barrier 'mutex))
    (when (slot-value barrier 'ever-entered)
      (make-condition 'barrier-in-use :message "Cannot set the preparation callback on a barrier which has ever been entered."))
    (setf (slot-value barrier 'callback1) cb)))

(defun barrier-set-post-block-callback (barrier cb)
  (with-mutex-lock ((slot-value barrier 'mutex))
    (when (slot-value barrier 'ever-entered)
      (make-condition 'barrier-in-use :message "Cannot set the preparation callback on a barrier which has ever been entered."))
    (setf (slot-value barrier 'callback2) cb)))


(defun lock-barrier (barrier &key wait-p)
  (let ((mutex (slot-value barrier 'mutex)))
    (with-mutex-lock (mutex)
      (setf (slot-value barrier 'locked) t)
      (when wait-p
        (wait-on-cond (slot-value barrier 'cond) mutex 
                      :test (= (slot-value barrier 'n-blocked) (slot-value barrier 'num-threads)))))))


(defun unlock-barrier (barrier)
  (with-mutex-lock ((slot-value barrier 'mutex))
    (setf (slot-value barrier 'locked) nil)
    (awaken-cond-waiters (slot-value barrier 'cond))))
  
(defun barrier-set-lock-condition (barrier lock-test)
  (with-mutex-lock ((slot-value barrier 'mutex))
    (setf (slot-value barrier 'lock-condition) lock-test)))

(defun synchronize-at-barrier (barrier &key (safe-to-lock-p t))
  (let ((n-threads (slot-value barrier 'num-threads))
        (mutex (slot-value barrier 'mutex))
        (cond (slot-value barrier 'cond))
        (cb1 (slot-value barrier 'callback1))
        (cb2 (slot-value barrier 'callback2)))

    ;; First, make sure the starvation race condition isn't occuring.
    ;; Wait until the outgoing queue is empty before entering the
    ;; blocking queue.
    (with-mutex-lock (mutex)
      (setf (slot-value barrier 'ever-entered) t)
      (wait-on-cond cond mutex :test (= 0 (slot-value barrier 'n-outgoing))))

    ;; Next, enter the blocking queue.  If I'm the last one in the
    ;; queue, run the callback and set up to release the prisoners
    (with-mutex-lock (mutex)
      (if (= n-threads (incf (slot-value barrier 'n-blocked)))
          (progn
            (when cb1
              (funcall cb1))
            (let ((testfcn (slot-value barrier 'lock-condition)))
              (when (and testfcn
                         (not (slot-value barrier 'locked)))
                (setf (slot-value barrier 'locked) (funcall testfcn))))
            (when (and (slot-value barrier 'locked)
                       safe-to-lock-p)
              (awaken-cond-waiters cond)
              (wait-on-cond cond mutex :test (not (slot-value barrier 'locked))))
            (when cb2
              (funcall cb2))
            (setf (slot-value barrier 'n-blocked) 0)
            (setf (slot-value barrier 'n-outgoing) n-threads)
            (setf (slot-value barrier 'permission) t)
            (awaken-cond-waiters cond))
          (wait-on-cond cond mutex :test (slot-value barrier 'permission))))

    ;; Threads which get here have passed the barrier, and now have to
    ;; register themselves as they leave.  They also may have to wake
    ;; up the blocked inbound threads waiting to enter the blocking
    ;; queue.  Last one out closes the gate behind him.
    (with-mutex-lock (mutex)
      (when (= 0 (decf (slot-value barrier 'n-outgoing)))
        (setf (slot-value barrier 'permission) nil)
        (awaken-cond-waiters cond)))))



          


(defun test-threads (n-threads)
  (let ((print-mutex (create-mutex))
        (barrier (make-instance 'synchronized-barrier
                                :number-of-threads n-threads
                                :locked t)))
    (dotimes (i n-threads)
      (let ((i2 i))
        (spawn-thread #'(lambda () 
                          (do () (nil) 
                            (with-mutex-lock (print-mutex)
                              (format t "Thread ~D entering barrier~%" i2))
                            (synchronize-at-barrier barrier)
                            (dotimes (j 5)
                              (with-mutex-lock (print-mutex)
                                (format t "Thread ~D, step ~D~%" i2 j))
                              (sleep (1+ (/ i2 2.0)))))))))
    (with-mutex-lock (print-mutex)
      (dotimes (i 3)
        (format t "Main thread counting down ~D~%" (- 2 i)))
      (format t "Main thread opening barrier~%"))
    (unlock-barrier barrier)
    (with-mutex-lock (print-mutex)
      (format t "Main thread waiting for 30 seconds~%"))
    (sleep 30)
    (with-mutex-lock (print-mutex)
      (format t "Main thread closing barrier~%"))
    (lock-barrier barrier)
    (with-mutex-lock (print-mutex)
      (format t "Main thread sleeping 20 seconds~%"))
    (sleep 20)))
