(cl:in-package #:hirundine-utilities)

;;; This function does a depth-first postorder traversal of a graph of
;;; nodes starting with START-NODE.  The function SUCCESSORS-FUNCTION
;;; must return a proper list of successors of a node.  The value
;;; returned is a list of the nodes traversed this way.

(defun depth-first-search-postorder (start-node successors-function)
  (let ((table (make-hash-table :test #'eq))
        (result '()))
    (labels ((traverse (node)
               (unless (gethash node table)
                 (setf (gethash node table) t)
                 (loop for successor in (funcall successors-function node)
                       do (traverse successor))
                 (push node result))))
      (traverse start-node))
    (nreverse result)))
