(cl:in-package #:hirundine-graph)

(defclass special-variable-reference-instruction (instruction)
  ((%variable-name
    :initarg :variable-name
    :reader variable-name)))
