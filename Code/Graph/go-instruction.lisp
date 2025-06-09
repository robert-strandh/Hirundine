(cl:in-package #:hirundine-graph)

;;; This instruction has two inputs and no outputs.  The first input
;;; is a dynamic-environment object representing the current dynamic
;;; environment.  The second input is the unique identifier of an exit
;;; point as created by the TAGBODY-INSTRUCTION.  The successor of
;;; this instruction is the instruction following a TABODY tag.

(defclass go-instruction (instruction)
  ())
