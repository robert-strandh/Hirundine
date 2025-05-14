(cl:in-package #:hirundine-dominance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the immediate dominator of each node in an arbitrary
;;; flowgraph.
;;;
;;; This function is a very close implementation of the algorithm by
;;; Thomas Lengauer and Robert Endre Tarjan, as described in their
;;; paper "A Fast Algorithm for Finding Dominators in a Flowgraph",
;;; published in ACM Transactions on Programming Languages and
;;; Systems, Vol 1, No 1, July 1979, pages 121-141.
;;;
;;; It is close in that I even duplicated quirks such as using numbers
;;; instead of objects in some cases, and starting numbering by 1.  I
;;; also used the variable names of the paper, which makes the code a
;;; bit harder to follow by itself, but easier to verify against the
;;; paper.
;;;
;;; Although advertised as working on flowgraphs (i.e. a graph where
;;; the nodes are basic blocks), it works for any types of graph with
;;; similar characteristics, and in particular for flowcharts (i.e.,
;;; graph where the nodes are individual instructions).  All that we
;;; require is that the nodes in the graph can be compared using EQ,
;;; and that a function is supplied that returns a list of successors
;;; for each node in the graph.
;;;
;;; We have tested this implementation on randomly generated graphs by
;;; comparing it to the result of a trivial algorithm for computing
;;; dominators (see the file test-dominance.lisp).
;;;
;;; The result is returned as an EQ hash table where the keys
;;; represent each node in the graph, and the value associated with
;;; the key is the immediate dominator of the node that is the key.
;;;
;;; The name of this function is not exported, because there is no
;;; reason for client code to use it directly.  It is used as a
;;; subroutine for computing the dominance tree and the dominance
;;; frontiers.

(defun immediate-dominators (start-node successors-function)
  (let* ((pred (make-hash-table :test #'eq))
         (parent (make-hash-table :test #'eq))
         (count (utilities:count-nodes start-node successors-function))
         (vertex (make-array count))
         (semi (make-hash-table :test #'eq))
         (dom (make-hash-table :test #'eq))
         (bucket (make-hash-table :test #'eq))
         (label (make-hash-table :test #'eq))
         (ancestor (make-hash-table :test #'eq))
         (n 0))
    (flet ((successors (node)
             (funcall successors-function node))
           (pred (node)
             (gethash node pred))
           ((setf pred) (new-pred node)
             (setf (gethash node pred) new-pred))
           (parent (node)
             (gethash node parent))
           ((setf parent) (new-parent node)
             (setf (gethash node parent) new-parent))
           (vertex (i)
             (aref vertex (1- i)))
           ((setf vertex) (new-vertex i)
             (setf (aref vertex (1- i)) new-vertex))
           (semi-default (node)
             (gethash node semi 0))
           (semi (node)
             (multiple-value-bind (value present-p)
                 (gethash node semi)
               (assert present-p)
               value))
           ((setf semi) (new-semi node)
             (setf (gethash node semi) new-semi))
           (dom (node)
             (gethash node dom))
           ((setf dom) (new-dom node)
             (setf (gethash node dom) new-dom))
           (bucket (node)
             (gethash node bucket))
           ((setf bucket) (new-bucket node)
             (setf (gethash node bucket) new-bucket))
           (label (node)
             (multiple-value-bind (value present-p)
                 (gethash node label)
               (assert present-p)
               value))
           ((setf label) (new-label node)
             (setf (gethash node label) new-label))
           (ancestor (node)
             (multiple-value-bind (value present-p)
                 (gethash node ancestor)
               (assert present-p)
               value))
           ((setf ancestor) (new-ancestor node)
             (setf (gethash node ancestor) new-ancestor)))
      ;; Step 1
      (labels ((dfs (v)
                 (setf (semi v) (incf n))
                 (setf (vertex n) v)
                 (loop for w in (successors v)
                       do (when (zerop (semi-default w))
                            (setf (parent w) v)
                            (dfs w))
                          (push v (pred w)))))
        (dfs start-node))
      (labels ((evaluate (v)
                 (if (null (ancestor v))
                     v
                     (progn (compress v)
                            (label v))))
               (compress (v)
                 (unless (null (ancestor (ancestor v)))
                   (compress (ancestor v))
                   (when (< (semi (label (ancestor v))) (semi (label v)))
                     (setf (label v) (label (ancestor v))))
                   (setf (ancestor v) (ancestor (ancestor v)))))
               (link (v w)
                 (setf (ancestor w) v)))
        ;; Step 2 and 3
        (loop for i from 1 to n
              for v = (vertex i)
              do (setf (ancestor v) nil)
                 (setf (label v) v))
        (loop for i downfrom n to 2
              for w = (vertex i)
              do (loop for v in (pred w)
                       for u = (evaluate v)
                       do (when (< (semi u) (semi w))
                            (setf (semi w) (semi u))))
                 (push w (bucket (vertex (semi w))))
                 (link (parent w) w)
                 (loop for v in (bucket (parent w))
                       do (setf (bucket (parent w))
                                (remove v (bucket (parent w)) :test #'eq))
                          (let ((u (evaluate v)))
                            (setf (dom v)
                                  (if (< (semi u) (semi v))
                                      u
                                      (parent w))))))
        (loop for i from 2 to n
              for w = (vertex i)
              do (unless (eq (dom w) (vertex (semi w)))
                   (setf (dom w) (dom (dom w))))))
      dom)))
