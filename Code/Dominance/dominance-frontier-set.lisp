(cl:in-package #:hirundine-dominance)

;;; For a set of nodes, compute the dominance frontier of the set.
;;;
;;; The DOMINANCE-FRONTIERS argument is the result of calling
;;; DOMINANCE-FRONTIERS, i.e. a table mapping each individual node to
;;; its dominance frontier.

(defun dominance-frontier-set (dominance-frontiers nodes)
  (loop with result = '()
        for node in nodes
        for df = (dominance-frontier dominance-frontiers node)
        do (setf result (union result df))
        finally (return result)))
