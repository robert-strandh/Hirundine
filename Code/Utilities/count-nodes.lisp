(cl:in-package #:hirundine-utilities)

(defun count-nodes (start-node successors-function)
  (length (hirundine-utilities:depth-first-search-preorder
           start-node successors-function)))
