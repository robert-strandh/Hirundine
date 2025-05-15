(cl:in-package #:hirundine-dominance)

;;; For each node in the graph, compute its dominance frontier.

;;; Compute a hash table that maps every node in a dominance tree to a
;;; list of its children in that tree.
(defun children (dominance-tree)
  (let ((result (make-hash-table :test #'eq)))
    (loop for node being each hash-key of dominance-tree
          do (let ((idom (immediate-dominator dominance-tree node)))
               (unless (null idom)
                 (push node (gethash idom result)))))
    result))

;;; Given a flow chart or a flow graph, compute the dominance frontier
;;; for each node.  To client code, the object returned is opaque, and
;;; can be used to pass to the function DOMINANCE-FRONTIER.
(defun dominance-frontiers (start-node successors-function)
  (let* ((dominance-tree (dominance-tree start-node successors-function))
         (children (children dominance-tree))
         (result (make-hash-table :test #'eq)))
    (flet ((children (node)
             (gethash node children))
           (successors (node)
             (funcall successors-function node))
           (df (node)
             (gethash node result))
           ((setf df) (new-df node)
             (setf (gethash node result) new-df)))
      (labels
          ((traverse (x)
             (mapc #'traverse (children x))
             (loop for y in (successors x)
                   do (unless (eq (immediate-dominator dominance-tree y)
                                  x)
                        (pushnew y (df x) :test #'eq)))
             (loop for z in (children x)
                   do (loop for y in (df z)
                            do (unless (eq (immediate-dominator dominance-tree y)
                                           x)
                                 (pushnew y (df x) :test #'eq))))))
        (traverse start-node)))
    result))

(defun dominance-frontier (dominance-frontiers node)
  (gethash node dominance-frontiers))
