(asdf:defsystem "drawer"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "data-types")
               (:file "operations")
               (:file "backend-generic")
               (:file "backend-text")
               (:file "backend-svg")
               (:file "backend-html")))
