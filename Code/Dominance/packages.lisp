(cl:in-package #:common-lisp-user)

(defpackage #:hirundine-dominance
  (:use #:common-lisp)
  (:local-nicknames (#:graph #:hirundine-graph)
                    (#:utilities #:hirundine-utilities))
  (:export
   #:immediate-dominators
   #:dominance-tree
   #:dominators
   #:immediate-dominator
   #:strict-dominators
   #:dominance-frontiers
   #:dominance-frontier))
