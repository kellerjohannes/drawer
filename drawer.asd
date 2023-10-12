(asdf:defsystem "drawer"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "data-types")))
