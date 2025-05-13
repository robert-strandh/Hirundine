(cl:in-package #:asdf-user)

(defsystem "hirundine-basic-blocks"
  :depends-on ("hirundine-graph")
  :serial t
  :components
  ((:file "packages")))
