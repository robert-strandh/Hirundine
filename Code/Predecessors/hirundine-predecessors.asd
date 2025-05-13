(cl:in-package #:asdf-user)

(defsystem "hirundine-predecessors"
  :depends-on ("hirundine-graph"
               "hirundine-utilities")
  :serial t
  :components
  ((:file "packages")
   (:file "predecessors")))
