;;;  A wrapper for drawing 2-dimensional plots with mcclim
;;;

; (asdf:oos 'asdf:load-op 'mcclim)

(defpackage "SOR"
  (:use :CLIM :CLIM-LISP :THREADS)
  (:export "APP-MAIN"))

(in-package "SOR")


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *bounds-modes* '(("Periodic" . :PERIODIC) 
                                 ("Clamped" . :CLAMPED)))
  (defparameter *type-modes* '(("Voltage" . :VOLTAGE) ("Charge" . :CHARGE)))
  (defparameter *draw-modes* '(("Line" . :LINE) ("Point" . :POINT)
                               ("Circle" . :CIRCLE) ("Ellipse" . :ELLIPSE)
                               ("Rectangle" . :RECTANGLE)))
  (defparameter *fill-modes* '(("Filled" . :FILLED) ("Unfilled" . :UNFILLED)))

  (defparameter *default-bounds-mode* :CLAMPED)
  (defparameter *default-type-mode* :VOLTAGE)
  (defparameter *default-draw-mode* :POINT)
  (defparameter *default-fill-mode* :UNFILLED))

(defparameter *border* 10)
(defparameter *pixmap-height* nil)
(defparameter *pixmap-width* nil)
(defparameter *command-height* 80)
(defparameter *control-width* 200)
(defparameter *voltage-steps* 40)

(defparameter *use-threading* t)
(defparameter *number-of-worker-threads* 6)
(defparameter *thread-barrier* 
  (make-instance 'synchronized-barrier 
                 :number-of-threads (if *use-threading* 
                                        *number-of-worker-threads* 
                                        1)
                 :locked *use-threading*))

(defparameter *thread-list* '())

(defparameter *object-lookup-alist*
  '((:VOLTAGE . ((:LINE . drawable-voltage-line)
                 (:POINT . drawable-voltage-point)
                 (:CIRCLE . drawable-voltage-circle)
                 (:ELLIPSE . drawable-voltage-ellipse)
                 (:RECTANGLE . drawable-voltage-rectangle)))
    (:CHARGE . ((:LINE . drawable-charge-line)
                (:POINT . drawable-charge-point)
                (:CIRCLE . drawable-charge-circle)
                (:ELLIPSE . drawable-charge-ellipse)
                (:RECTANGLE . drawable-charge-rectangle)))))


;; I'm going to modify some CLIM text-output gadgets in worker
;; threads, so I'm mutex-locking the modification of those gadgets
;; against the redisplay

;; (defmethod redisplay-frame-panes :around ((frame application-frame) &key force-p)
;;   (declare (ignorable force-p))
;;   (with-mutex-lock (*frame-display-lock*)
;;     (call-next-method)))

(defparameter *frame-display-lock* (create-mutex))

;; (defmethod handle-event :around ((frame application-frame) (event clim-internals::execute-command-event))
;;   (with-mutex-lock (*frame-display-lock*)
;;     (call-next-method)))

;; The load-solution button needs to be protected
(defmethod handle-event :around ((button clim:push-button-pane) (event clim:pointer-button-release-event))
  (with-mutex-lock (*frame-display-lock*)
    (call-next-method)))

(defmethod execute-frame-command :around ((frame application-frame) command)
  (with-mutex-lock (*frame-display-lock*)
    (call-next-method)))


(defmacro loop-with-neighbours ((loop-var repeat-size 
                                          &key (start 0) (increment 1))
                                &body body)
"Loops the variable 'loop-var' from 'start' adding 'increment' each
time, and stopping when the variable is equal to or greater than
'repeat-size' (the body is not executed when the variable meets this
condition).  At the same time, variables appear in scope, with the
name of 'loop-var' and appended with '+' or '-'.  These hold,
respectively, one more than and one less than the 'loop-var', except
at the borders, where it loops around.  Looping 'i' to 10 would create
a variable 'i-' which starts at 9, then goes to 0 through 8."
  (let ((loop+ (gensym)) (loop- (gensym)))
    (setf loop+ (intern (format nil "~A+" loop-var)))
    (setf loop- (intern (format nil "~A-" loop-var)))

    `(do* ((,loop-var ,start (incf ,loop-var ,increment))
           (,loop+ (mod (1+ ,loop-var) ,repeat-size) (mod (incf ,loop+ ,increment) ,repeat-size))
           (,loop- (mod (1- ,loop-var) ,repeat-size) (mod (incf ,loop- ,increment) ,repeat-size)))
      ((>= ,loop-var ,repeat-size))
      ,@body)))
                                

(defmacro forever (&body body)
  `(do () (nil) ,@body))



(defclass locked-object ()
  ((value	:initform nil
                :initarg :val)
   (mutex	:initform (create-mutex))))

(defgeneric get-value (obj))
(defmethod get-value ((obj locked-object))
  (with-mutex-lock ((slot-value obj 'mutex))
    (slot-value obj 'value)))

(defgeneric set-value (obj newval))
(defmethod set-value ((obj locked-object) newval)
  (with-mutex-lock ((slot-value obj 'mutex))
    (setf (slot-value obj 'value) newval)))



(defun distance (x1 y1 x2 y2)
  (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))


(defclass drawable-object ()
  ((x1		:accessor get-x1
                :initarg :x1)
   (y1		:accessor get-y1
                :initarg :y1)
   (x2		:accessor get-x2
                :initarg :x2)
   (y2		:accessor get-y2
                :initarg :y2)
   (type	:accessor get-type
                :initarg :type)
   (value	:accessor get-value
                :initarg :value)
   (filled	:accessor get-filled
                :initarg :filled)
   (thickness	:accessor get-thickness
                :initarg :thickness)))

(defclass voltage-object () ())
(defclass charge-object () ())

(defclass drawable-point (drawable-object) ())
(defclass drawable-line (drawable-object) ())
(defclass drawable-circle (drawable-object) ())
(defclass drawable-ellipse (drawable-object) ())
(defclass drawable-rectangle (drawable-object) ())
(defclass drawable-voltage-point (drawable-point voltage-object) ())
(defclass drawable-voltage-line (drawable-line voltage-object) ())
(defclass drawable-voltage-circle (drawable-circle voltage-object) ())
(defclass drawable-voltage-ellipse (drawable-ellipse voltage-object) ())
(defclass drawable-voltage-rectangle (drawable-rectangle voltage-object) ())
(defclass drawable-charge-point (drawable-point charge-object) ())
(defclass drawable-charge-line (drawable-line charge-object) ())
(defclass drawable-charge-circle (drawable-circle charge-object) ())
(defclass drawable-charge-ellipse (drawable-ellipse charge-object) ())
(defclass drawable-charge-rectangle (drawable-rectangle charge-object) ())

(defgeneric get-ink (obj frame))

(defmethod get-ink ((obj voltage-object) frame)
  (funcall (colour-fcn :RED) (get-voltage-colour frame (get-value obj))))

(defmethod get-ink ((obj charge-object) frame)
  (funcall (colour-fcn :BLUE) (get-rho-colour frame (get-value obj))))


(defgeneric record-value-seen (obj frame))

(defmethod record-value-seen ((obj voltage-object) frame)
  (set-voltage-seen frame (get-value obj)))

(defmethod record-value-seen ((obj charge-object) frame)
  (set-rho-seen frame (get-value obj)))

  

(defgeneric draw-one-object (obj pane))

(defmethod draw-one-object :before ((obj drawable-object) pane)
  (declare (ignore pane))
  (record-value-seen obj *application-frame*))

(defmethod draw-one-object ((obj drawable-point) pane)
  (draw-point* pane 
               (get-x1 obj) (get-y1 obj)
               :line-thickness (get-thickness obj)
               :ink (get-ink obj *application-frame*)))

(defmethod draw-one-object ((obj drawable-line) pane)
  (draw-line* pane 
               (get-x1 obj) (get-y1 obj) (get-x2 obj) (get-y2 obj)
               :line-thickness (get-thickness obj)
               :ink (get-ink obj *application-frame*)))
  
(defmethod draw-one-object ((obj drawable-circle) pane)
  (let* ((x1 (get-x1 obj)) (y1 (get-y1 obj))
         (x2 (get-x2 obj)) (y2 (get-y2 obj))
         (centre-x (/ (+ x1 x2) 2))
         (centre-y (/ (+ y1 y2) 2)))
    (draw-circle* pane centre-x centre-y 
                  (/ (distance x1 y1 x2 y2) 2)
                  :line-thickness (get-thickness obj) 
                  :filled (get-filled obj)
                  :ink (get-ink obj *application-frame*))))

(defmethod draw-one-object ((obj drawable-ellipse) pane)
  (let* ((x1 (get-x1 obj)) (y1 (get-y1 obj))
         (x2 (get-x2 obj)) (y2 (get-y2 obj))
         (centre-x (/ (+ x1 x2) 2))
         (centre-y (/ (+ y1 y2) 2))
         (delta-x (- x2 x1)) (delta-y (- y2 y1)))
    (draw-ellipse* pane centre-x centre-y 
                   (/ delta-x 2) 0 0 (/ delta-y 2)
                   :line-thickness (get-thickness obj) 
                   :filled (get-filled obj)
                   :ink (get-ink obj *application-frame*))))

(defmethod draw-one-object ((obj drawable-rectangle) pane)
  (draw-rectangle* pane (get-x1 obj) (get-y1 obj) (get-x2 obj) (get-y2 obj)
                   :line-thickness (get-thickness obj)
                   :filled (get-filled obj)
                   :ink (get-ink obj *application-frame*)))


(defgeneric get-regions (obj))
(defmethod get-regions ((obj drawable-point))
  (let ((thickness (get-thickness obj)))
    (if (= thickness 1)
        (values (make-rectangle* (get-x1 obj) (get-y1 obj)
                                 (1+ (get-x1 obj)) (1+ (get-y1 obj)))
                +nowhere+)
        (values (make-ellipse* (get-x1 obj) (get-y1 obj)
                               (/ thickness 2) 0 0 (/ thickness 2))
                +nowhere+))))

(defmethod get-regions ((obj drawable-line))
  (error "Cannot call 'get-regions' on a line object"))

(defmethod get-regions ((obj drawable-circle))
  (let* ((x1 (get-x1 obj)) (y1 (get-y1 obj))
         (x2 (get-x2 obj)) (y2 (get-y2 obj))
         (thickness (get-thickness obj))
         (distance (distance x1 y1 x2 y2)))
    (values (make-ellipse* (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                           (/ (+ distance thickness) 2) 0 
                           0 (/ (+ distance thickness) 2))
            (if (or (get-filled obj)
                    (>= thickness (/ distance 2)))
                +nowhere+
                (make-ellipse* (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                               (/ (- distance thickness) 2) 0
                               0 (/ (- distance thickness) 2))))))


(defmethod get-regions ((obj drawable-ellipse))
  (let* ((x1 (get-x1 obj)) (y1 (get-y1 obj))
         (x2 (get-x2 obj)) (y2 (get-y2 obj))
         (thickness (get-thickness obj))
         (abs-delta-x (abs (- x2 x1)))
         (abs-delta-y (abs (- y2 y1))))
    (values (make-ellipse* (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                           (/ (+ abs-delta-x thickness) 2) 0 
                           0 (/ (+ abs-delta-y thickness) 2))
            (if (or (get-filled obj)
                    (>= thickness abs-delta-x)
                    (>= thickness abs-delta-y))
                +nowhere+
                (make-ellipse* (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                               (/ (- abs-delta-x thickness) 2) 0
                               0 (/ (- abs-delta-y thickness) 2))))))

(defmethod get-regions ((obj drawable-rectangle))
  (let* ((x1 (get-x1 obj)) (y1 (get-y1 obj))
         (x2 (get-x2 obj)) (y2 (get-y2 obj))
         (px1 (min x1 x2)) (px2 (max x1 x2))
         (py1 (min y1 y2)) (py2 (max y1 y2))
         (half-thickness (/ (get-thickness obj) 2)))
    (values (make-rectangle* (- px1 half-thickness) (- py1 half-thickness)
                             (+ px2 half-thickness) (+ py2 half-thickness))
            (if (or (get-filled obj)
                    (>= half-thickness (- px2 px1))
                    (>= half-thickness (- py2 py1)))
                +nowhere+
                (make-rectangle* (+ px1 half-thickness) (+ py1 half-thickness)
                                 (- px2 half-thickness) (- py2 half-thickness))))))
    
  

(defgeneric load-array-from-object (obj frame))

(defmethod load-array-from-object ((obj drawable-object) frame)
  (let ((recorder-fcn (get-recorder-fcn obj frame)))
    (multiple-value-bind (region+ region-) (get-regions obj)
      (apply-to-all-points-in-region
       #'(lambda (i j)
           (funcall recorder-fcn i j (get-value obj) (get-show frame)))
       region+ region-))))

(defmethod load-array-from-object ((obj drawable-line) frame)
  (let* ((recorder-fcn (get-recorder-fcn obj frame))
         (x1 (get-x1 obj))
         (y1 (get-y1 obj))
         (x2 (get-x2 obj))
         (y2 (get-y2 obj))
         (angle (atan (- y2 y1) (- x2 x1)))
         (length (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))
         (thickness (get-thickness obj))
         (transformation (compose-transformations
                          (make-translation-transformation x1 y1)
                          (make-rotation-transformation angle))))

    (apply-to-all-points-in-region 
     #'(lambda (i j)
         (multiple-value-bind (i2 j2)
             (transform-position transformation (+ 0.5 i) (+ 0.5 j))
           (funcall recorder-fcn (floor i2) (floor j2) (get-value obj) (get-show frame)))
         (multiple-value-bind (i2 j2)
             (transform-position transformation i j)
           (funcall recorder-fcn (floor i2) (floor j2) (get-value obj) (get-show frame))))
     (make-rectangle* 0 (/ thickness -2) length (/ thickness 2)))))


(defgeneric get-recorder-fcn (obj frame))

(defmethod get-recorder-fcn ((obj voltage-object) frame)
  (let ((width (first (get-pixmap-size frame)))
        (height (second (get-pixmap-size frame)))
        (float-type (type-of (aref (get-solution-matrix frame) 0 0))))
    #'(lambda (i j value &optional (pixmap nil))
        (when (and (<= 0 i (1- width))
                   (<= 0 j (1- height)))
          (setf (aref (get-voltage-matrix frame) i j) (coerce value float-type))
          (when pixmap
            (draw-point* pixmap i j :ink +black+))))))

(defmethod get-recorder-fcn ((obj charge-object) frame)
  (let ((width (first (get-pixmap-size frame)))
        (height (second (get-pixmap-size frame)))
        (float-type (type-of (aref (get-charge-matrix frame) 0 0))))
    #'(lambda (i j value &optional (pixmap nil))
        (when (and (<= 0 i (1- width))
                   (<= 0 j (1- height)))
          (setf (aref (get-charge-matrix frame) i j) (coerce value float-type))
          (when pixmap
            (draw-point* pixmap i j :ink +black+))))))



(defmacro integ-radio-button (label modes-alist start-setting &key slot callback (orientation :vertical))
  "Creates a radio button object with label 'label' and which offers
the CAR of each element in 'modes-alist'.  When selected, assigns the
CDR of the corresponding element to the 'slot' position of
*application-frame*.  Optionally calls a user-supplied callback with
the gadget and the value.  The radio button defaults to the element
given by 'start-setting' of the list but this does *not* independently
guarantee that the corresponding slot in the application frame will be
correctly set."
  (let* ((ma (if (symbolp modes-alist) (symbol-value modes-alist) modes-alist))
         (setting (if (symbolp start-setting) (symbol-value start-setting) start-setting))
         (start-n (position setting ma :key #'cdr)))
    (assert (and (integerp start-n)
                 (consp ma)
                 (> (length ma) start-n -1)))
    `(labelling (:label ,label)
      (with-radio-box 
          (:orientation ,orientation
           :label ,label
           :value-changed-callback #'(lambda (gadget value)
                                       (declare (ignorable gadget))
                                       ,(when slot
                                              `(setf (slot-value *application-frame* ,slot) (cdr (assoc (gadget-label value) ,modes-alist :test #'string=))))
                                       ,(when callback
                                              `(funcall ,callback gadget value))))
        ,@(let (res)
               (dotimes (i (length ma))
                 (if (= i start-n)
                     (push `(radio-box-current-selection ,(car (nth i ma))) res)
                     (push `,(car (nth i ma)) res)))
               (nreverse res))))))

(defmacro restricted-text-field (label typespec slot &key callback (active t) bind-gadget)
  "Creates a labelled text-input field which accepts only objects of
type 'typespec'.  If the most recent change rendered the object
invalid, the 'slot' is set to nil.  Calls 'callback' with the gadget
and value.  If 'bind-gadget' is set, puts the value of the gadget into
that slot of the *application-frame*."  
  `(labelling (:label ,label)
     (let ((gadget-handle
            (make-pane 'text-field
                       :editable-p t
                       :active ,active
                       :scroll-bars nil
                       :value (format nil "~A" (slot-value *application-frame* ,slot))
                       :value-changed-callback 
                       #'(lambda (gadget value)
                           (declare (ignorable gadget))
                           (let ((*read-eval* nil))
                             (setf (slot-value *application-frame* ,slot)
                                   (handler-case 
                                       (multiple-value-bind (seen last-read) (read-from-string value)
                                         (and (= last-read (length value))
                                              (typep seen ,typespec)
                                              seen))
                                     (reader-error nil)
                                     (end-of-file nil))))
                           ,(when callback
                           `(funcall ,callback gadget value))))))
       ,(when bind-gadget
              `(setf (slot-value *application-frame* ,bind-gadget) gadget-handle))
       gadget-handle)))

 
(defmacro reporting-text-field (label bind-gadget &key (active t))
  "Creates a labelled text-output field which displays a message.  Stores the gadget address in 'bind-gadget' of *application-frame*."
  `(labelling (:label ,label)
     (let ((gadget-handle
            (make-pane 'text-field
                       :editable-p nil
                       :active ,active
                       :scroll-bars nil
                       :value "" )))
       (setf (slot-value *application-frame* ,bind-gadget) gadget-handle))))
              
    



(clim:define-application-frame plotter ()
  ((show-space	:accessor get-show
                :initform nil)
   (map-size	:accessor get-pixmap-size
                :initform '(0 0))
   (voltage-matrix	:accessor get-voltage-matrix
                        :initform nil)
   (charge-matrix	:accessor get-charge-matrix
                        :initform nil)
   (solution-matrix	:accessor get-solution-matrix
                        :initform nil)
   (skip-matrix-inits	:accessor get-skip-matrix-inits
                        :initform nil)
   (solution-max-v	:accessor get-sol-max-v
                        :initform nil)
   (solution-min-v	:accessor get-sol-min-v
                        :initform nil)
   (mutex		:accessor get-mutex
                        :initform (create-mutex))
   (bounds	:accessor get-bounds
                :initform *default-bounds-mode*)
   (voltage-text	:accessor get-voltage-text
                        :initform nil)
   (volts	:accessor get-volts
                :initform 0)
   (charge-text	:accessor get-charge-text
                :initform nil)
   (charge	:accessor get-charge
                :initform 0)
   (type	:accessor get-type
                :initform *default-type-mode*)
   (thickness	:accessor get-thickness
                :initform 1)
   (indicator-pane	:accessor get-indicator
                        :initform nil)
   (calc-indicator-pane	:accessor get-calc-indicator
                        :initform nil)
   (draw-mode	:accessor get-draw-mode
                :initform *default-draw-mode*)
   (fill-mode	:accessor get-fill-mode
                :initform *default-fill-mode*)
   (objlist	:accessor get-draw-list
                :initform '())
   (redraw-needed	:accessor get-redraw-needed
                        :initform nil)
   (stop-fcn	:accessor get-stop-fcn
                :initform #'(lambda (frame) (>= (get-num-passes frame) (get-desired-passes frame))))
   (min-v-seen	:accessor min-v-seen
                :initform nil)
   (max-v-seen	:accessor max-v-seen
                :initform nil)
   (min-rho-seen	:accessor min-rho-seen
                        :initform nil)
   (max-rho-seen	:accessor max-rho-seen
                        :initform nil)
   (run-once-button	:accessor get-run-once)
   (stop-run-button	:accessor get-stop-run)
   (run-complete-button	:accessor get-run-completely)
   (progress-text	:accessor get-progress-text)
   (epsilon-text	:accessor get-epsilon-text)
   (max-epsilon-text	:accessor get-max-epsilon-text)
   (passes		:accessor get-num-passes
                        :initform 0)
   (target-passes	:accessor get-desired-passes
                        :initform 0)
   (valid	:accessor get-valid
                :initform t))
  (:panes
   (com-text :interactor
             :incremental-redisplay t
             :height *command-height*)
   (app :application 
        :scroll-bars nil
        :height (+ *pixmap-height* (* 2 *border*))
        :max-height (+ *pixmap-height* (* 2 *border*))
        :min-height (+ *pixmap-height* (* 2 *border*))
        :width (+ *pixmap-width* (* 2 *border*))
        :max-width (+ *pixmap-width* (* 2 *border*))
        :min-width (+ *pixmap-width* (* 2 *border*))
        :draw nil :record nil
        :display-function 'draw)

   

   (ctrl1 (vertically ()
                      (let ((pane
                             (make-pane 'application-pane
                                        :height 10 :max-height 10
                                        :min-height 10)))
                        (setf (get-indicator *application-frame*) pane)
                        (setf (medium-background pane) +green+)
                        pane)

                      (integ-radio-button "Bounds" 
                                          *bounds-modes* 
                                          *default-bounds-mode* 
                                          :slot 'bounds)
                      (integ-radio-button "Placements" 
                                          *type-modes* 
                                          *default-type-mode* 
                                          :slot 'type
                                          :callback 'set-active-type)
                      (restricted-text-field "Voltage" 'number 'volts
                                             :active (eq *default-type-mode* :VOLTAGE)
                                             :bind-gadget 'voltage-text
                                             :callback 'check-frame-valid)
                      (restricted-text-field "Charge" 'number 'charge
                                             :active (eq *default-type-mode* :CHARGE)
                                             :bind-gadget 'charge-text
                                             :callback 'check-frame-valid)
                      (restricted-text-field "Thickness" '(integer 1)
                                             'thickness
                                             :callback 'check-frame-valid)

                      (integ-radio-button "Drawing Object"
                                          *draw-modes*
                                          *default-draw-mode*
                                          :slot 'draw-mode)
                      (integ-radio-button "Filling"
                                          *fill-modes*
                                          *default-fill-mode*
                                          :slot 'fill-mode)
                      (make-pane 'push-button
                                 :label "Go to solver"
                                 :activate-callback 'init-matrices)))

   (ctrl2 (vertically ()
                      (let ((pane
                             (make-pane 'application-pane
                                        :height 10 :max-height 10
                                        :min-height 10)))
                        (setf (get-calc-indicator *application-frame*) pane)
                        (setf (medium-background pane) +red+)
                        pane)

                      (setf (get-run-once *application-frame*)
                            (make-pane 'push-button
                                       :label "Run one step"
                                       :active nil
                                       :activate-callback 'solve-one-step))
                      (setf (get-stop-run *application-frame*)
                            (make-pane 'push-button
                                       :label "Stop solver"
                                       :active nil
                                       :activate-callback 'stop-solving))
                      (setf (get-run-completely *application-frame*)
                            (make-pane 'push-button
                                       :label "Load solution to pixmap"
                                       :active nil
                                       :activate-callback 'fill-in-solution))
                      (reporting-text-field "Progress in timestep"
                                            'progress-text
                                            :active nil)
                      (reporting-text-field "Maximum epsilon"
                                            'max-epsilon-text
                                            :active nil)
                      (reporting-text-field "Error per cell"
                                            'epsilon-text
                                            :active nil))))


  (:top-level (clim:default-frame-top-level))  ;  :prompt #'promptfcn))
  (:menu-bar t)
  (:layouts
   (positioning (vertically () (horizontally () app ctrl1) com-text))
   (computing (vertically () (horizontally () app ctrl2) com-text))))


(define-plotter-command (com-refresh :menu t
                                     :keystroke (:L :control))
    ()
  (draw *application-frame* (get-frame-pane *application-frame* 'app)))


(define-plotter-command (com-undo :menu t
                                  :keystroke (:U :control))
    ()
  (when (and (consp (get-draw-list *application-frame*))
             (eq (frame-current-layout *application-frame*) 'positioning)
             (not (null (get-draw-list *application-frame*))))
    (pop (get-draw-list *application-frame*))
    (perform-full-redraw *application-frame* (get-frame-pane *application-frame* 'app))
    (draw *application-frame* (get-frame-pane *application-frame* 'app))))



(define-presentation-to-command-translator add-object
    (blank-area com-add-object plotter
                :gesture :select
                :echo nil)
  (x y)
  (list x y))

(define-plotter-command (com-add-object :name nil) ((x 'real) (y 'real))
  (handle-add-object (find-pane-named *application-frame* 'app) x y))



(defun init-matrices (button)
  (declare (ignore button))
  (let* ((frame *application-frame*)
         (width (first (get-pixmap-size frame)))
         (height (second (get-pixmap-size frame))))

    (when (eq (get-bounds frame) :CLAMPED)
      (dotimes (i (1- width))
        (setf (aref (get-voltage-matrix frame) i 0) 0.0d0
              (aref (get-voltage-matrix frame) i (1- height)) 0.0d0))
      (dotimes (j (1- height))
        (setf (aref (get-voltage-matrix frame) 0 j) 0.0d0
              (aref (get-voltage-matrix frame) (1- width) j) 0.0d0)))
    ;; Now, "draw" the objects into the charge and voltage arrays.
    ;; Use the reverse list because we want only the most recent
    ;; modification to a pixel to be used.  Voltage always trumps
    ;; charge.
    (dolist (obj (reverse (get-draw-list frame)))
      (load-array-from-object obj frame))

    ;; Copy the clamped voltages into the solution matrix.
    (dotimes (i width)
      (dotimes (j height)
        (let ((v (aref (get-voltage-matrix frame) i j)))
          (when v
            (setf (aref (get-solution-matrix frame) i j) v)))))

    (setf (get-num-passes frame) 0)
    (activate-gadget (get-run-once frame))
    (activate-gadget (get-run-completely frame))
    (activate-gadget (get-progress-text frame))
    (activate-gadget (get-epsilon-text frame))
    (activate-gadget (get-max-epsilon-text frame))
    (setf (frame-current-layout frame) 'computing)))


(defun solve-one-step (button)
  (declare (ignore button))
  (activate-gadget (get-stop-run *application-frame*))
  (incf (get-desired-passes *application-frame*) (apply #'max (get-pixmap-size *application-frame*)))
  (if *use-threading*
      (unlock-barrier *thread-barrier*)
      (threaded-solver *application-frame* 0 1)))

(defun stop-solving (button)
  (deactivate-gadget button)
  (setf (get-desired-passes *application-frame*) (get-num-passes *application-frame*)))
  

(defun threaded-solver (frame thread-number n-threads)
;;  (declare (optimize (speed 3) (safety 0) (space 0)))
  (synchronize-at-barrier *thread-barrier*)

  (let* ((sol (get-solution-matrix frame))
         (rho (get-charge-matrix frame))
         (volt (get-voltage-matrix frame))
         (width (first (get-pixmap-size frame)))
         (height (second (get-pixmap-size frame)))
         (m-w-h (max width height))
         (flipflop t)   ;; ensure that the callbacks are only called
			;; on every second pass
         (overparm (/ 2 (+ 1 (/ pi m-w-h))))
         (error 0.0d0)
         max-v min-v
         (max-delta 0.0d0))

    (declare (type fixnum width) (type fixnum height)
             (type (simple-array double-float (* *)) sol)
             (type (simple-array double-float (* *)) rho))


    (barrier-set-lock-condition *thread-barrier*
                                #'(lambda ()
                                    (funcall (get-stop-fcn frame) frame)))
    (barrier-set-pre-block-callback 
     *thread-barrier* #'(lambda ()
                          (unless (setf flipflop (not flipflop))
                            (unless (mutex-locked-p *frame-display-lock*)
                              (with-mutex-lock (*frame-display-lock*)
                                (setf (gadget-value (get-epsilon-text frame)) (format nil "~F~%" (/ error (* height width))))
                                (setf (gadget-value (get-progress-text frame)) (format nil "~F~%" (/ (get-num-passes frame) m-w-h)))
                                (setf (gadget-value (get-max-epsilon-text frame)) (format nil "~F~%" max-delta))
                                (let ((np (/ (1+ (get-num-passes frame)) m-w-h))
                                      (calc-ind (get-calc-indicator frame)))
                                  (when (= np 1)
                                    (setf (medium-background calc-ind) +yellow+)
                                    (redisplay-frame-pane frame calc-ind :force-p t))
                                  (when (= np 2)
                                    (setf (medium-background calc-ind) +green+)
                                    (redisplay-frame-pane frame calc-ind :force-p t))))))))


    
    (barrier-set-post-block-callback
     *thread-barrier* #'(lambda ()
                          (unless flipflop
                            (with-mutex-lock ((get-mutex frame))
                              (setf (get-sol-max-v frame) nil)
                              (setf (get-sol-min-v frame) nil)
                              (incf (get-num-passes frame))))))

    ;; Checkerboard increment

    (forever
     (dotimes (stepstep m-w-h)
       (setf max-delta 0)
       (setf error 0.0d0)
       (setf max-v nil min-v nil)
       (dotimes (pass 2)
         (loop-with-neighbours 
          (i width :start thread-number :increment n-threads)
          (declare (type fixnum i) (type fixnum i-) (type fixnum i+))
          (loop-with-neighbours 
           (j height :start (mod (+ i pass) 2) :increment 2)
         
           (declare (type fixnum j) (type fixnum j-) (type fixnum j+))

           (let ((new-v (aref volt i j)))
             (unless new-v
               (let* ((avg-over-neighbours (/ (+ (aref sol i- j)
                                                 (aref sol i j+)
                                                 (aref sol i+ j)
                                                 (aref sol i j-)) 4))
                      (delta (- avg-over-neighbours (+ (aref sol i j) (aref rho i j))))
                      (correction (* delta overparm)))
                 (setf max-delta (max max-delta (abs delta)))
                 (incf (the double-float error) (the double-float (abs correction)))
                 (setf new-v (incf (aref sol i j) correction))))
             (when (or (not max-v)
                       (> (the double-float new-v) (the double-float max-v)))
               (setf max-v new-v))
             (when (or (not min-v)
                       (< (the double-float new-v) (the double-float min-v)))
               (setf min-v new-v)))))

         (when (= pass 1)
           (with-mutex-lock ((get-mutex frame))
             (when (or (null (get-sol-max-v frame))
                       (> max-v (get-sol-max-v frame)))
               (setf (get-sol-max-v frame) max-v))
             (when (or (null (get-sol-min-v frame))
                       (< min-v (get-sol-min-v frame)))
               (setf (get-sol-min-v frame) min-v))))

         (synchronize-at-barrier *thread-barrier* :safe-to-lock-p (= pass 1)))))))



(defun fill-in-solution (&rest args)
  (declare (ignore args))
  (draw-solution-on-pixmap (get-show *application-frame*) (get-solution-matrix *application-frame*))
  (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'app)))




(defun draw-solution-on-pixmap (pixmap sol)
  (let* ((frame *application-frame*)
         (ink-fcn (colour-fcn :GREY))
         (width (first (get-pixmap-size frame)))
         (height (second (get-pixmap-size frame)))
         (code-array (make-array (list width height))))

    (draw-rectangle* pixmap 0 0 width height :filled t :ink +yellow+)
    (labels ((colour-code (value min-v max-v)
               (if (= min-v max-v)
                   0
                   (floor (- value min-v) (/ (- max-v min-v) *voltage-steps*))))
             (should-draw-p (val val+ val-)
               (let ((code (car val)) (error (cdr val))
                     (code+ (car val+)) (error+ (cdr val+))
                     (code- (car val-)) (error- (cdr val-)))
                 (or (= error 0)
                     (and (/= code code+)
                          (/= code code-))
                     (or (and (= code code+)
                              (/= code code-)
                              (<= (abs error) (abs error+)))
                         (and (= code code-)
                              (/= code code+)
                              (<= (abs error) (abs error-))))))))

      (lock-barrier *thread-barrier* :wait-p t)
      (let ((max-v (get-sol-max-v frame))
            (min-v (get-sol-min-v frame)))
        (assert (and max-v min-v))
        (dotimes (i width)
          (dotimes (j height)
            (multiple-value-bind (code error) (colour-code (aref sol i j) min-v max-v)
              (setf (aref code-array i j) (cons code error))))))
      (unlock-barrier *thread-barrier*)

      (loop-with-neighbours
       (i width)
       (loop-with-neighbours
        (j height)
        (when (or (should-draw-p (aref code-array i j) (aref code-array i- j) (aref code-array i+ j))
                  (should-draw-p (aref code-array i j) (aref code-array i j-) (aref code-array i j+)))
          (draw-point* pixmap i j :ink (funcall ink-fcn (/ (car (aref code-array i j)) *voltage-steps*)))))))))
    
    


(defun handle-add-object (pane x1 y1)
  (when (and (get-valid *application-frame*)
             (get-show *application-frame*))
    (draw-point* pane x1 y1)
    (multiple-value-bind (x2 y2)
        (block processor
          (if (eq (get-draw-mode *application-frame*) :POINT)
              (values x1 y1)
              (tracking-pointer (pane)
                                (:pointer-button-release (&key event x y)
                                                         (when (= (pointer-event-button event) +pointer-left-button+)
                                                           (return-from processor (values x y)))))))
      (let* ((px1 (max 0 (- x1 *border*)))
             (py1 (max 0 (- y1 *border*)))
             (px2 (max 0 (- x2 *border*)))
             (py2 (max 0 (- y2 *border*)))
             (frame *application-frame*)
             (shape (get-draw-mode frame))
             (type (get-type frame))
             (fill-mode (eq (get-fill-mode frame) :FILLED))
             value)
        (when (and (eq shape :LINE)
                   (= px1 px2)
                   (= py1 py2))
          (setf shape :POINT))
        (case type
          (:VOLTAGE
           (setf value (get-volts frame)))
          (:CHARGE
           (setf value (get-charge frame)))
          (t
           (error "Unrecognized object type")))

        (let ((obj (create-one-object px1 py1 px2 py2 shape 
                                      type value fill-mode (get-thickness frame))))
          (push obj (get-draw-list frame))
          (draw-one-object obj (get-show frame)))
        (when (redraw-needed frame)
          (perform-full-redraw frame (get-frame-pane *application-frame* 'app)))))))


(defun create-one-object (x1 y1 x2 y2 shape type value filled thickness)
  (let ((object (cdr (assoc shape (cdr (assoc type *object-lookup-alist*))))))
    (make-instance object :x1 x1 :y1 y1 :x2 x2 :y2 y2 :value value :filled filled :thickness thickness)))



(defgeneric redraw-needed (obj))
(defmethod redraw-needed ((obj plotter))
  (or (not (get-show obj))
      (get-redraw-needed obj)))
  

(defun perform-full-redraw (frame pane)
  (let ((width (pixmap-width (get-show frame)))
        (height (pixmap-height (get-show frame))))
    (setf (get-redraw-needed frame) nil)
    (setf (get-show frame) 
          (with-output-to-pixmap (my-pixmap pane :width width :height height)
            (setf (medium-background pane) +white+)
            (window-clear pane)
            (dolist (obj (get-draw-list frame))
              (draw-one-object obj my-pixmap))))))



(defun set-active-type (gadget value)
  (cond
    ((string= (gadget-label value) "Voltage")
     (let ((active-gadget (get-voltage-text *application-frame*)))
       (activate-gadget active-gadget)
       (deactivate-gadget (get-charge-text *application-frame*))))
    ((string= (gadget-label value) "Charge")
     (let ((active-gadget (get-charge-text *application-frame*)))
       (activate-gadget active-gadget)
       (deactivate-gadget (get-voltage-text *application-frame*))))
    (t (format t "~A ~A unknown~%" value (gadget-label value))))
  (check-frame-valid gadget value))

(defun set-voltage-seen (frame value)
  (unless (and (min-v-seen frame)
               (max-v-seen frame)
               (<= (min-v-seen frame) value (max-v-seen frame)))
    (when (or (not (min-v-seen frame))
              (< value (min-v-seen frame)))
      (setf (min-v-seen frame) value))
    (when (or (not (max-v-seen frame))
              (> value (max-v-seen frame)))
      (setf (max-v-seen frame) value))
    (setf (get-redraw-needed frame) t)))

(defun set-rho-seen (frame value)
  (unless (and (min-rho-seen frame)
               (max-rho-seen frame)
               (<= (min-rho-seen frame) value (max-rho-seen frame)))
    (when (or (not (min-rho-seen frame))
              (< value (min-rho-seen frame)))
      (setf (min-rho-seen frame) value))
    (when (or (not (max-rho-seen frame))
              (> value (max-rho-seen frame)))
      (setf (max-rho-seen frame) value))
    (setf (get-redraw-needed frame) t)))


(defun get-voltage-colour (frame value)
  (if (= (min-v-seen frame) (max-v-seen frame))
      0.5
      (/ (- value (min-v-seen frame)) (- (max-v-seen frame) (min-v-seen frame)))))

(defun get-rho-colour (frame value)
  (if (= (min-rho-seen frame) (max-rho-seen frame))
      0.5
      (/ (- value (min-rho-seen frame)) (- (max-rho-seen frame) (min-rho-seen frame)))))


(defun check-frame-valid (gadget value)
  (declare (ignore gadget value))
  (let* ((frame *application-frame*)
         (old-valid (get-valid frame))
         (new-valid (and (get-thickness frame)
                         (case (get-type frame)
                           (:VOLTAGE (get-volts frame))
                           (:CHARGE (get-charge frame))))))

    (unless (eq (not old-valid) (not new-valid))
      (setf (get-valid frame) new-valid)
      (if new-valid
          (setf (medium-background (get-indicator frame)) +green+)
          (setf (medium-background (get-indicator frame)) +red+))
      (window-clear (get-indicator frame)))))
       



;; (defun build-pixmap (pane width height)
;;   (with-output-to-pixmap (my-pixmap pane :width width :height height)
;;     (clim:draw-rectangle* my-pixmap 0 0 width height :filled t :ink +white+)
;;     (dotimes (j height)
;;       (clim:draw-line* my-pixmap (/ width 2) j width j :ink (funcall (colour-fcn :BLUE) (/ j height)))
;;       (clim:draw-line* my-pixmap (- (/ width 2) 1) j 0 j :ink (funcall (colour-fcn :RED) (/ j height))))))

(defun build-pixmap (pane width height)
  (with-output-to-pixmap (my-pixmap pane :width width :height height)
    (setf (medium-background pane) +white+)
    (window-clear pane)))



(defun colour-fcn (type)
  (case type
    (:GREY
     #'(lambda (frac) (make-gray-color frac)))
    (:RED
     #'(lambda (frac) (make-rgb-color (+ 0.2 (* 0.799 frac)) 0.1 0.1)))
    (:GREEN
     #'(lambda (frac) (make-rgb-color 0.1 (+ 0.2 (* 0.799 frac)) 0.1)))
    (:BLUE
     #'(lambda (frac) (make-rgb-color 0.1 0.1 (+ 0.2 (* 0.799 frac)))))
    (:TRIANGLES
     #'(lambda (frac) 
         (labels ((restrictor (x) 
                    (cond ((< x 0) 0)
                          ((> x 1) 1)
                          (t x))))
           (make-rgb-color (restrictor (* 3 frac))
                           (restrictor (- (* 3 frac) 1))
                           (restrictor (- (* 3 frac) 2))))))
    (:HSV
     (let* ((repeat-avoid-amt 0.04)
            (size (- 1 (* 2 repeat-avoid-amt))))
       #'(lambda (frac)
           (make-ihs-color 1.6 (+ repeat-avoid-amt (* size frac)) 0.9))))
    (t
     (error "Unrecognized type passed to color-fcn"))))


(defun draw (frame pane)
  (setf (get-pixmap-size frame) (list *pixmap-width* *pixmap-height*))
  (clim:draw-rectangle* pane (1- *border*) (1- *border*) (+ 1 *border* *pixmap-width*) (+ 1 *border* *pixmap-height*) :filled nil :ink +green+)
  (when (redraw-needed frame)
    (setf (get-show frame) (build-pixmap pane *pixmap-width* *pixmap-height*)))
              
  (clim:copy-from-pixmap (get-show frame) 0 0 *pixmap-width* *pixmap-height* pane *border* *border*))


(defun apply-to-all-points-in-region (mapfcn positive-region &optional (negative-region +nowhere+))
  "Takes a positive region and pixellates it into integers.  If the
region contains the centre of pixel (i,j) where we define the pixel by
the coordinates of its upper-left corner, and the negative-region does
not, then it applies the given function with those two integers as
arguments."
  (labels ((int-surround (r)
             "Integer bounding box.  Always a superset of the real
bounding box"
             (make-rectangle* (floor (rectangle-min-x r)) 
                              (floor (rectangle-min-y r))
                              (ceiling (rectangle-max-x r))
                              (ceiling (rectangle-max-y r))))
           (split-vertically (r)
             "Splits the integer bounding box into two integer
bounding boxes side by side."
             (let ((half-width (floor (/ (rectangle-width r) 2))))
               (list (make-rectangle* (rectangle-min-x r)
                                      (rectangle-min-y r)
                                      (+ (rectangle-min-x r) half-width)
                                      (rectangle-max-y r))
                     (make-rectangle* (+ (rectangle-min-x r) half-width)
                                      (rectangle-min-y r)
                                      (rectangle-max-x r)
                                      (rectangle-max-y r)))))
           (split-horizontally (r)
             "Splits the integer bounding box into two integer
bounding boxes one above the other."
             (let ((half-height (floor (/ (rectangle-height r) 2))))
             (list (make-rectangle* (rectangle-min-x r)
                                    (rectangle-min-y r)
                                    (rectangle-max-x r)
                                    (+ (rectangle-min-y r) half-height))
                   (make-rectangle* (rectangle-min-x r)
                                    (+ (rectangle-min-y r) half-height)
                                    (rectangle-max-x r)
                                    (rectangle-max-y r)))))
           (recurse-work (region+ region- bounds)
             (let ((rect-w (rectangle-width bounds))
                   (rect-h (rectangle-height bounds))
                   (rect-min-x (rectangle-min-x bounds))
                   (rect-min-y (rectangle-min-y bounds)))
               (cond
                 ((and (= 1 rect-w) (= 1 rect-h))
                  (when (and (region-contains-position-p region+
                                                         (+ 0.5 rect-min-x)
                                                         (+ 0.5 rect-min-y))
                             (not (region-contains-position-p region-
                                                              (+ 0.5 rect-min-x)
                                                              (+ 0.5 rect-min-y))))
                    (funcall mapfcn rect-min-x rect-min-y)))
                 ((= 1 rect-w)
                  (dolist (r (split-horizontally bounds))
                    (when (region-intersects-region-p region+ r)
                      (recurse-work region+ region- r))))
                 ((= 1 rect-h)
                  (dolist (r (split-vertically bounds))
                    (when (region-intersects-region-p region+ r)
                      (recurse-work region+ region- r))))
                 (t
                  (dolist (r (split-vertically bounds))
                    (dolist (q (split-horizontally r))
                      (when (region-intersects-region-p region+ q)
                        (recurse-work region+ region- q)))))))))

    (let ((bounded-by (int-surround (bounding-rectangle positive-region))))
      (recurse-work positive-region negative-region bounded-by))))

  


(defun app-main ()
  (setf *pixmap-height* 900
        *pixmap-width*  1200)
  (unwind-protect
       (clim:run-frame-top-level (clim:make-application-frame 'plotter))
    (mapc #'threads:destroy-thread *thread-list*)
    (setf *thread-list* '())))
    


;; add a :before method for run-frame-top-level to do initialization


(defmethod run-frame-top-level :before ((frame application-frame) 
                                        &key &allow-other-keys)

  (unless (get-skip-matrix-inits frame)
    (format t "Initializing matrices~%")
    (let ((dimensions (list *pixmap-width* *pixmap-height*)))
      (setf (get-voltage-matrix frame) (make-array dimensions :initial-element nil)
            (get-charge-matrix frame) (make-array dimensions :initial-element 0.0d0 :element-type 'double-float)
            (get-solution-matrix frame) (make-array dimensions :initial-element 0.0d0 :element-type 'double-float)
            (get-pixmap-size frame) dimensions))
    (setf (get-skip-matrix-inits frame) t)

    (when (and *use-threading*
               (null *thread-list*))

      (setf *thread-barrier* 
            (make-instance 'synchronized-barrier 
                           :number-of-threads *number-of-worker-threads* 
                           :locked t))
      (format t "Starting ~D worker threads~%" *number-of-worker-threads*)
      (dotimes (i *number-of-worker-threads*)
        (let ((j i))
          (push (threads:spawn-thread 'threaded-solver frame j *number-of-worker-threads*) *thread-list*))))))
