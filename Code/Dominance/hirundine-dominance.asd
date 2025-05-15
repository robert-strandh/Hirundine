(cl:in-package #:asdf-user)

(defsystem "hirundine-dominance"
  :depends-on ("hirundine-graph"
               "hirundine-utilities")
  :serial t
  :components
  ((:file "packages")
   (:file "immediate-dominators")
   (:file "dominance-tree")
   (:file "dominance-frontiers")
   (:file "dominance-frontier-set")))

;;;; Recall that a node A is said to DOMINATE a node B if and only if
;;;; every path from the start node to B includes A.  The dominance
;;;; relation is reflexive, transitive, and antisymmetric.  In other
;;;; words:
;;;;
;;;;   * For every node A, A dominates A.
;;;;
;;;;   * For nodes A, B, and C, if A dominates B and B dominates C,
;;;;     then A dominates C.
;;;;
;;;;   * For nodes A and B, if A dominates B and B dominates A, then
;;;;     A = B.
;;;;
;;;; A node A is said to IMMEDIATELY DOMINATE a node B if and only if
;;;; A and B are distinct, A dominates B, and there does not exist a
;;;; node C distinct from A and B, such that A dominates C and C
;;;; dominates B.  A node A that IMMEDIATELY DOMINATES a node B is
;;;; said to be THE IMMEDIATE DOMINATOR of B, because it is unique.
;;;;
;;;; A node A is said to STRICTLY DOMINATE a node B if and only if A
;;;; dominates B and A is distinct from B.
;;;;
;;;; The dominance relation can be represented as a TREE containing
;;;; each node of the flow graph, where the root of the tree is the
;;;; start node of the graph, and the parent of each node in the tree
;;;; other than the root is its unique immediate dominator.  Such a
;;;; tree is called a DOMINANCE TREE.
;;;;
;;;; For a node A, the DOMINANCE FRONTIER of A is the set of all nodes
;;;; B such that A dominates an immediate predecessor of B, but A does
;;;; not strictly dominate B.
;;;;
;;;; The concept of dominance frontier is extended to a set of nodes,
;;;; where it simply means the union of the dominance frontiers of each
;;;; node in the set.
