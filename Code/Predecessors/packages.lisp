(cl:in-package #:common-lisp-user)

(defpackage #:hirundine-predecessors
  (:use #:common-lisp)
  (:local-nicknames (#:graph #:hirundine-graph)
                    (#:utilities #:hirundine-utilities))
  (:export
   #:compute-predecessors
   #:predecessors))
