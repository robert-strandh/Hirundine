(cl:in-package #:common-lisp-user)

(defpackage #:hirundine-utilities
  (:use #:common-lisp)
  (:local-nicknames (#:graph #:hirundine-graph))
  (:export
   #:map-instructions-arbitrary-order))

