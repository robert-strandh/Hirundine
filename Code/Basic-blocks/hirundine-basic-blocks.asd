(cl:in-package #:asdf-user)

(defsystem "hirundine-basic-blocks"
  :depends-on ("hirundine-graph"
               "hirundine-predecessors")
  :serial t
  :components
  ((:file "packages")
   (:file "basic-blocks")))
