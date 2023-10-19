(asdf:defsystem "drawer"
  :depends-on (:cl-svg :uiop :alexandria)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "data-types")
               (:file "operations")
               (:file "tonsystem")
               (:file "backend-generic")
               (:file "backend-text")
               (:file "backend-svg")
               (:file "backend-html")
               (:file "backend-tikz")))
