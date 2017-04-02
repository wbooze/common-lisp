(in-package :cl-user)

(defpackage :stack-layout
  (:use :clim :clim-lisp)
  (:export :stack-layout-pane :add-pane :remove-pane))

(in-package :stack-layout)

(defclass stack-layout-pane (clim:sheet-multiple-child-mixin
                             clim:basic-pane)
  ())

(defmethod compose-space ((pane stack-layout-pane) &key width height)
  (declare (ignore width height))
  (reduce (lambda (x y)
            (space-requirement-combine #'max x y))
          (mapcar #'compose-space (sheet-children pane))
          :initial-value
          (make-space-requirement :width 0 :min-width 0 :max-width 0
                                  :height 0 :min-height 0 :max-height 0)))

(defmethod allocate-space ((pane stack-layout-pane) width height)
  (dolist (child (sheet-children pane))
    (move-and-resize-sheet child 0 0 width height)
    (allocate-space child width height)))

(defmethod initialize-instance :after ((pane stack-layout-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (dolist (child contents)
    (sheet-adopt-child pane child)))

(defgeneric add-pane (pane parent &optional enabled))

(defmethod add-pane ((pane sheet) (parent stack-layout-pane)  &optional enabled)
  (setf (sheet-enabled-p pane) enabled)
  (sheet-adopt-child parent pane))

(defgeneric remove-pane (pane parent))

(defmethod remove-pane ((pane sheet) (parent stack-layout-pane))
  (sheet-disown-child parent pane))

(defmethod remove-pane ((pane sheet) (parent (eql 'stack-layout-pane)))
  (remove-pane pane (sheet-parent pane)))
