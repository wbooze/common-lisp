;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RADIO-LAYOUT; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A Radio Layout Pane based on the Stack Layout Pane
;;;   Created: 2005/09/16-19
;;;    Author: Max-Gerd Retzlaff <m.retzlaff@gmx.net>, http://bl0rg.net/~mgr
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Max-Gerd Retzlaff

(in-package :cl-user)

(defpackage :radio-layout
  (:use :clim :clim-lisp :stack-layout)
  (:export :radio-layout-pane :add-pane :remove-pane
           :enabled-pane :switch-to-pane))

(in-package :radio-layout)


(defclass radio-layout-pane (stack-layout-pane)
  ((enabled-radio-pane :initform nil :accessor enabled-radio-pane)))

(defgeneric enabled-pane (parent))

(defmethod enabled-pane ((parent radio-layout-pane))
  (enabled-radio-pane parent))

;; (defmethod initialize-instance :after ((pane radio-layout-pane)
;; 				       &rest args
;; 				       &key contents
;; 				       &allow-other-keys)
;;   (declare (ignore args))
;;   (let (in-cdr)
;;     (dolist (child contents)
;;       (if in-cdr
;;           (setf (sheet-enabled-p child) nil)
;;           (progn 
;;             (setf in-cdr t)
;;             (setf (enabled-radio-pane pane) child)))
;;       (sheet-adopt-child pane child))))

(defmethod initialize-instance :after ((pane radio-layout-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (setf (enabled-radio-pane pane) (car contents))
  (dolist (child (cdr contents))
    (setf (sheet-enabled-p child) nil)))

(defgeneric switch-to-pane (pane parent))

(defmethod switch-to-pane ((pane sheet) (parent radio-layout-pane))
  (unless (equal pane
                 (enabled-radio-pane parent))
    (setf (sheet-enabled-p (enabled-radio-pane parent))
          nil
          (sheet-enabled-p pane)
          t
          (enabled-radio-pane parent)
          pane)))

(defmethod switch-to-pane ((pane sheet) (parent (eql 'radio-layout-pane)))
  (switch-to-pane pane (sheet-parent pane)))

(defmethod add-pane :after ((pane sheet) (parent radio-layout-pane)  &optional enabled)
  (when enabled
    (switch-to-pane pane parent)))

(defmethod remove-pane :after ((pane sheet) (parent radio-layout-pane))
  (let ((last-of-children (car (last (sheet-children parent)))))
    (when (and last-of-children
               (equal pane
                      (enabled-radio-pane parent)))
      (switch-to-pane last-of-children parent))))

(defmethod remove-pane ((pane sheet) (parent (eql 'radio-layout-pane)))
  (remove-pane pane (sheet-parent pane)))
