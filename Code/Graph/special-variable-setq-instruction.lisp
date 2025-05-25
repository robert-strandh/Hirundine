(cl:in-package #:hirundine-graph)

;;; This instruction has two inputs.  The first input is a
;;; dynamic-environment object.  The second input is the value to
;;; assign.  This instruction has no outputs.

(defclass special-variable-setq-instruction (instruction)
  ((%variable-name
    :initarg :variable-name
    :reader variable-name)))
