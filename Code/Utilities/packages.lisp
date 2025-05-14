(cl:in-package #:common-lisp-user)

(defpackage #:hirundine-utilities
  (:use #:common-lisp)
  (:local-nicknames (#:graph #:hirundine-graph))
  (:export
   #:map-instructions-arbitrary-order
   #:depth-first-search-preorder
   #:depth-first-search-postorder
   #:count-nodes))

