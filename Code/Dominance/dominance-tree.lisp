(cl:in-package #:hirundine-dominance)

;;; Compute the complete dominance tree.
;;;
;;; We represent the complete dominance tree as a hash table with the
;;; nodes of the flowgraph as keys.  The value for a particular node
;;; is a list starting with the node itself, then the immediate
;;; dominator of the node, then the immediate dominator of the
;;; immediate dominator of the node, etc.  The last element of the
;;; list is always the root node of the flowgraph.
;;;
;;; By representing it this way, we can use the dominance tree to
;;; return all interesting dominance relations.  By itself, a value
;;; in the table is a set of all dominators of a node.  The CADR of
;;; that list is the IMMEDIATE dominator of a node, The CDR of the
;;; list is the set of all STRICT dominators of the node.

;;; From the point of view of client code, this function returns an
;;; opaque object to be used as an argument to the three dominator
;;; functions below.
(defun dominance-tree (start-node successors-function)
  (let ((immediate-dominators
          (immediate-dominators start-node successors-function))
        (dominators (make-hash-table :test #'eq)))
    (setf (gethash start-node dominators) (list start-node))
    (labels ((find-dominator-tree (node)
               (or (gethash node dominators)
                   (setf (gethash node dominators)
                         (cons node (find-dominator-tree
                                     (gethash node immediate-dominators)))))))
      (loop for node being each hash-key of immediate-dominators
            do (find-dominator-tree node)))
    dominators))

(defun dominators (dominance-tree node)
  (gethash node dominance-tree))

(defun immediate-dominator (dominance-tree node)
  (cadr (gethash node dominance-tree)))

(defun strict-dominators (dominance-tree node)
  (cdr (gethash node dominance-tree)))
