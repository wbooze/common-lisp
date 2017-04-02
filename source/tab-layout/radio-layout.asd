(defsystem :radio-layout
  :depends-on (:mcclim :stack-layout)
  :components ((:file "radio-layout")))

(defsystem :radio-layout-example
  :depends-on (:radio-layout)
  :components ((:file "radio-layout-example")))
