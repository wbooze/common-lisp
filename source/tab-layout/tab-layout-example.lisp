(in-package :cl-user)

(defpackage :tab-layout-example
  (:use :clim :clim-lisp :tab-layout)
  (:export :ozo))

(in-package :tab-layout-example)

;;; example and testing code

(define-application-frame ozo ()
  ()
  (:panes
   (editor :text-editor :value "Text Editor")
   (foo :text-editor :value "Foo")
   (bar (make-pane 'push-button :label "Bar" :name "bar pane"))
   (baz (make-pane 'push-button :label "Baz"))
   (io :interactor :height 20 :width 600)
   (pointer-doc :pointer-documentation))
  (:layouts
   (default
      (vertically ()
        (with-tab-layout ('pane :name 'my-tab-pane)
           ("Text Editor" editor)
           ("foo pane(i)" foo 'integer)
           ("bar pane" bar)
           ("baz pane" baz))
        io
        pointer-doc))))

(defun ozo ()
  (run-frame-top-level (make-application-frame 'ozo)))

(define-presentation-to-command-translator remove-pane
    (pane com-remove-tab-pane ozo
                 :gesture :describe
                 :documentation "remove this pane"
                 :pointer-documentation "remove this pane")
    (object)
  (list object))


(define-ozo-command (com-remove-pane-by-title :name t)
    ((title 'string :prompt "Pane title"))
  (remove-pane title (find-pane-named *application-frame* 'my-tab-pane)))

(define-ozo-command (com-remove-pane-by-pane :name t)
    ((sheet 'sheet :prompt "Sheet"))
  (remove-pane sheet (find-pane-named *application-frame* 'my-tab-pane)))


(define-ozo-command (com-add-brr-pane :name t)
    ()
  (let ((fm (frame-manager *application-frame*)))
    (with-look-and-feel-realization (fm *application-frame*)
      (add-pane (make-tab-pane-from-list "Brr"
                                            (make-pane 'text-editor-pane :name 'brr :value "Brr"))
                (find-pane-named *application-frame* 'my-tab-pane) t))))

(define-ozo-command (com-remove-brr-pane-by-pane :name t)
    ()
  (let ((pane (find-pane-named *application-frame* 'brr)))
    ;; (remove-pane pane (sheet-parent (sheet-parent pane)))
    (remove-pane pane 'tab-layout-pane)))

(define-ozo-command (com-remove-brr-pane-by-title :name t)
    ()
  (remove-pane "Brr" (find-pane-named *application-frame* 'my-tab-pane)))

;;; This does not work class instances are not equal (even not under EQUALP).
;;; It could be made working by replacing the class TAB-PANE by a STRUCTure, though.
;;;
;;; (define-ozo-command (com-remove-pane :name t)
;;;     ()
;;;   (let ((pane (find-pane-named *application-frame* 'brr)))
;;;     (remove-pane (make-tab-pane-from-list "Brr" pane)
;;;                  (sheet-parent (sheet-parent pane)))))
