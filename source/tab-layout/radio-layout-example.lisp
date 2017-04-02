(in-package :cl-user)

(defpackage :radio-layout-example
  (:use :clim :clim-lisp :radio-layout)
  (:export :ooz))

(in-package :radio-layout-example)

;;; example code

(define-application-frame ooz ()
  ()
  (:panes
   (foo (make-pane 'push-button :label "Foo"))
   (bar (make-pane 'push-button :label "Bar"))
   (baz (make-pane 'push-button :label "Baz"))
   (io :interactor))
  (:layouts
   (default
       (vertically ()
         (make-pane 'radio-layout-pane
                    :contents (list foo bar baz))
         io))))

(defun ooz ()
  (run-frame-top-level (make-application-frame 'ooz)))

(defmacro make-switch-command (pane-name)
  `(define-ooz-command (,(read-from-string (concatenate 'string "com-" (string pane-name)))
                         :name ,(nstring-downcase (string pane-name)))
       ()
     (let ((pane (find-pane-named *application-frame* ',pane-name)))
       (switch-to-pane pane 'radio-layout-pane))))

(make-switch-command foo)
(make-switch-command bar)
(make-switch-command baz)

(define-ooz-command (com-break :name t)
    ()
  (break))

(define-ooz-command (com-add-pane :name t)
    ()
  (let ((fm (frame-manager *application-frame*)))
    (with-look-and-feel-realization (fm *application-frame*)
      (let ((foo-pane (find-pane-named *application-frame* 'foo)))
        (add-pane (make-pane 'push-button :name 'brr :label "Brr")
                  (sheet-parent foo-pane) t))))
  (make-switch-command brr)
  nil)

(define-ooz-command (com-remove-pane :name t)
    ()
  (let ((pane (find-pane-named *application-frame* '|brr|)))
    ;; (remove-pane pane (sheet-parent pane))
    (remove-pane pane 'radio-layout-pane))
  (remove-command-from-command-table 'com-brr 'ooz))
