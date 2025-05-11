(cl:in-package #:hirundine-graph)

(defclass global-function-reference-instruction (instruction)
  ((%function-name
    :initarg :function-name
    :reader function-name)))
