(cl:in-package #:hirundine-graph)

;;; This instruction has three inputs and no outputs.  The first input
;;; is a dynamic-environment object representing the current dynamic
;;; environment.  The second input is the unique identifier of an exit
;;; point as created by the BLOCK-INSTRUCTION.  The third input is a
;;; register or a literal containing the values to transmit as the
;;; values of the corresponding BLOCK special form.  The successor of
;;; this instruction is a RECEIVE-INSTRUCTION that receives the values
;;; and transmits them to the right register.

(defclass return-from-instruction (instruction)
  ())
