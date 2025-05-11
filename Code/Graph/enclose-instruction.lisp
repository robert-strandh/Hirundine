(cl:in-package #:hirundine-graph)

(defclass enclose-instruction (instruction)
  ((%parse-arguments-instruction
    :initarg :parse-arguments-instruction
    :reader parse-arguments-instruction)))
