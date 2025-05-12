(cl:in-package #:asdf-user)

(defsystem "hirundine-utilities"
  :depends-on ("hirundine-graph")
  :serial t
  :components
  ((:file "packages")
   (:file "map-instructions-arbitrary-order")))
