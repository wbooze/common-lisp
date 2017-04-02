;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TAB-LAYOUT; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A Tab Layout Pane based on the Radio Layout Pane
;;;   Created: 2005/09/16-19
;;;    Author: Max-Gerd Retzlaff <m.retzlaff@gmx.net>, http://bl0rg.net/~mgr
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Max-Gerd Retzlaff

(in-package :cl-user)

(defpackage :tab-layout
  (:use :clim :clim-lisp :radio-layout)
  (:export :tab-layout-pane :add-pane :remove-pane
           :enabled-pane :switch-to-pane
           :find-in-tab-panes-list :set-drawing-options-for-pane-in-tab-layout
           :make-tab-pane-from-list :with-tab-layout
           :com-switch-to-tab-pane :com-remove-tab-pane))

(in-package :tab-layout)


(defclass tab-layout-pane (vrack-pane)
  ((tab-panes :initform nil :accessor tab-panes-of-tab-layout :initarg :tab-panes)
   (radio-layout-pane :initform nil :accessor radio-layout-pane-of-tab-layout :initarg :radio-layout-pane)
   (tab-bar-pane :initform nil :accessor tab-bar-pane-of-tab-layout :initarg :tab-bar-pane)))

(defclass tab-pane ()
  ((title :initform nil :accessor tab-pane-title :initarg :title)
   (pane :initform nil :accessor tab-pane-pane :initarg :pane)
   (ptype :initform nil :accessor tab-pane-ptype :initarg :ptype)
   (drawing-options :initform nil :accessor drawing-options-of-tab-pane)))

(defgeneric find-in-tab-panes-list (thing parent))

(defmethod find-in-tab-panes-list ((thing sheet) (parent tab-layout-pane))
  (find thing (tab-panes-of-tab-layout parent)
        :key #'tab-pane-pane :test #'equal))

(defmethod find-in-tab-panes-list ((thing string) (parent tab-layout-pane))
  (find thing (tab-panes-of-tab-layout parent)
        :key #'tab-pane-title :test #'string-equal))

(defmethod find-in-tab-panes-list ((thing sheet) (parent (eql 'tab-layout-pane)))
  (find-in-tab-panes-list thing (sheet-parent (sheet-parent thing))))

(defun set-drawing-options-for-pane-in-tab-layout (pane drawing-options)
  (setf (drawing-options-of-tab-pane (find-in-tab-panes-list pane 'tab-layout-pane))
        drawing-options))

(defmethod enabled-pane ((parent tab-layout-pane))
  (find-in-tab-panes-list
   (enabled-pane (radio-layout-pane-of-tab-layout parent))
   parent))

(defmethod switch-to-pane ((pane sheet) (parent (eql 'tab-layout-pane)))
  (switch-to-pane pane 'radio-layout-pane))

(defun make-tab-pane-from-list (title tab-pane &optional ptype)
  (make-instance 'tab-pane
                 :title title
                 :pane tab-pane
                 :ptype ptype))

(defun make-list-from-tab-pane (tab-pane)
  (list (tab-pane-title tab-pane)
        (tab-pane-pane tab-pane)
        (tab-pane-ptype tab-pane)))



(define-command (com-switch-to-tab-pane :command-table clim:global-command-table)
    ((tab-pane 'tab-pane :prompt "Tab pane"))
  (let ((pane (tab-pane-pane tab-pane)))
    (switch-to-pane pane 'tab-layout-pane)))

(define-presentation-to-command-translator switch-via-tab-button
    (tab-pane com-switch-to-tab-pane clim:global-command-table
                 :gesture :select
                 :documentation "Switch to this pane"
                 :pointer-documentation "Switch to this pane")
    (object)
  (list object))

(define-command (com-remove-tab-pane :command-table clim:global-command-table)
    ((tab-pane 'tab-pane :prompt "Tab pane"))
  (remove-pane tab-pane 'tab-layout-pane))

;;;You probably don't want to uncomment this general command translator.
;;;
;;; (define-presentation-to-command-translator remove-tab-pane
;;;     (tab-pane com-remove-tab-pane clim:global-command-table
;;;                  :gesture :describe
;;;                  :documentation "remove this tab pane")
;;;     (object)
;;;   (list object))

;;; a test command
(define-command (com-narf :name t
                          :command-table clim:global-command-table)
    ((pane 'pane :prompt "Tab pane"))
  (print (pane-name pane))
  (set-drawing-options-for-pane-in-tab-layout pane (list :ink +red+))
  #+nil (break))



(defclass tab-bar-view (gadget-view)
  ())

(defparameter +tab-bar-view+ (make-instance 'tab-bar-view))

(define-presentation-method present (tab-pane (type tab-pane) stream
                                                (view tab-bar-view) &key)
  (stream-increment-cursor-position stream 5 0)
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (let* ((length-top-line (+ x 6 (text-size stream (tab-pane-title tab-pane)) 3))
           (tab-button-polygon (list x (+ y 14)   (+ x 6) y
                                    (+ x 6) y   length-top-line y
                                    length-top-line y   (+ length-top-line 6) (+ y 14))))
      
      ;; grey-filled polygone for the disabled panes
      (unless (sheet-enabled-p (tab-pane-pane tab-pane))
        (draw-polygon* stream tab-button-polygon :ink +grey+))
      
      ;; black non-filled polygon
      (draw-polygon* stream tab-button-polygon :ink +black+ :filled nil)
      
      ;; "breach" the underline for the enabled pane
      (when (sheet-enabled-p (tab-pane-pane tab-pane))
        ;; (draw-line* stream x (+ y 14) (+ length-top-line 6) (+ y 14) :ink +white+)
        (draw-line stream (apply #'make-point (subseq tab-button-polygon 0 2))
                          (apply #'make-point (subseq tab-button-polygon (- (length tab-button-polygon) 2)))
                          :ink +background-ink+))))
  
  (stream-increment-cursor-position stream 8 0)
  (apply #'invoke-with-drawing-options stream
         (lambda (rest)
           (declare (ignore rest))
           (write-string (tab-pane-title tab-pane) stream))
         (drawing-options-of-tab-pane tab-pane))
  (stream-increment-cursor-position stream 10 0))



(defmacro with-tab-layout ((default-ptype &key name)
                              &body body)
  (let* ((radio-layout-pane (gensym "radio-layout-pane-"))
         (tab-bar-pane (gensym "tab-bar-pane-"))
         (tab-layout-name-gensym (gensym "tab-layout-"))
         (tab-layout-name (or name `',tab-layout-name-gensym)))
    `(let ((,tab-bar-pane (make-clim-stream-pane
                                   :default-view +tab-bar-view+
                                   :display-time :command-loop
                                   :scroll-bars nil
                                   :borders nil
                                   :height 22
                                   :display-function
                                   (lambda (frame pane)
                                     (declare (ignore frame))
                                     (stream-increment-cursor-position pane 0 3)
                                     (draw-line* pane 0 17 (slot-value pane 'climi::current-width) 17 :ink +black+)
                                     (mapcar (lambda (tab-pane)
                                                 (with-output-as-presentation (pane (tab-pane-pane tab-pane)
                                                                                    (or (tab-pane-ptype tab-pane)
                                                                                        ,default-ptype))
                                                   (present tab-pane 'tab-pane :stream pane)))
                                             (tab-panes-of-tab-layout (sheet-parent
                                                                             (sheet-parent
                                                                              (or (climi::pane-border pane) pane))))))))
           (,radio-layout-pane (make-pane 'radio-layout-pane
                                          :contents (list ,@(mapcar #'second body)))))
       (make-pane 'tab-layout-pane
                  :name ,tab-layout-name
                  :tab-panes (list ,@(mapcar (lambda (tab-spec)
                                                  `(apply #'make-tab-pane-from-list (list ,@tab-spec)))
                                                body))
                  :radio-layout-pane ,radio-layout-pane
                  :tab-bar-pane ,tab-bar-pane
                  :contents (list ,tab-bar-pane
                                  `(+fill+ ,,radio-layout-pane))))))



(defmethod add-pane ((pane tab-pane) (parent tab-layout-pane) &optional enabled)
  (add-pane (tab-pane-pane pane) (radio-layout-pane-of-tab-layout parent) enabled)
  (push pane (tab-panes-of-tab-layout parent)))

(defmethod remove-pane ((pane tab-pane) (parent tab-layout-pane))
  (remove-pane (tab-pane-pane pane) (radio-layout-pane-of-tab-layout parent))
  (setf (tab-panes-of-tab-layout parent)
        (delete pane (tab-panes-of-tab-layout parent)
                :test #'equal)))

(defmethod remove-pane ((pane sheet) (parent tab-layout-pane))
  (remove-pane (find-in-tab-panes-list pane parent) parent))

(defmethod remove-pane ((pane string) (parent tab-layout-pane))
  (remove-pane (find-in-tab-panes-list pane parent) parent))

(defmethod remove-pane ((pane tab-pane) parent)
  (declare (ignore parent))
  (remove-pane pane (sheet-parent (sheet-parent (tab-pane-pane pane)))))

(defmethod remove-pane ((pane sheet) (parent (eql 'tab-layout-pane)))
  (let ((parent (sheet-parent (sheet-parent pane))))
    (remove-pane (find-in-tab-panes-list pane parent) parent)))
