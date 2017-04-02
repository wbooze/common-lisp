(defsystem :stack-layout
  :depends-on (:mcclim)
  :components ((:file "stack-layout")))

(defsystem :stack-layout-example
  :depends-on (:stack-layout)
  :components ((:file "stack-layout-example")))
