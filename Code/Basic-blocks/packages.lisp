(cl:in-package #:common-lisp-user)

(defpackage #:hirundine-basic-blocks
  (:use #:common-lisp)
  (:local-nicknames (#:graph #:hirundine-graph)
                    (#:predecessors #:hirundine-predecessors)
                    (#:utilities #:hirundine-utilities))
  (:export
   #:comput-basic-blocks
   #:leader
   #:trailer))
