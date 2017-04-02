(defsystem :tab-layout
  :depends-on (:mcclim :radio-layout)
  :components ((:file "tab-layout")))

(defsystem :tab-layout-example
  :depends-on (:tab-layout)
  :components ((:file "tab-layout-example")))
