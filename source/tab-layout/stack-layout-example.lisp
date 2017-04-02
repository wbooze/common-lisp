(in-package :cl-user)

(defpackage :stack-layout-example
  (:use :clim :clim-lisp :stack-layout)
  (:export :zoo))

(in-package :stack-layout-example)

;;; example code

(define-application-frame zoo ()
  ()
  (:panes
   (foo (make-pane 'push-button :label "Foo"))
   (bar (make-pane 'push-button :label "Bar"))
   (baz (make-pane 'push-button :label "Baz"))
   (io :interactor))
  (:layouts
   (default
       (vertically ()
         (make-pane 'stack-layout-pane
                    :contents (list foo bar baz))
         io))))

(defun zoo ()
  (run-frame-top-level (make-application-frame 'zoo)))

(define-zoo-command (com-foo :name "foo") ()
  (let ((s (find-pane-named *application-frame* 'foo)))
    (dolist (k (sheet-children (sheet-parent s)))
      (setf (sheet-enabled-p k) (eq k s)))))

(define-zoo-command (com-bar :name "bar") ()
  (let ((s (find-pane-named *application-frame* 'bar)))
    (dolist (k (sheet-children (sheet-parent s)))
      (setf (sheet-enabled-p k)(eq k s)))))

(define-zoo-command (com-baz :name "baz") ()
  (let ((s (find-pane-named *application-frame* 'baz)))
    (dolist (k (sheet-children (sheet-parent s)))
      (setf (sheet-enabled-p k)(eq k s)))))
