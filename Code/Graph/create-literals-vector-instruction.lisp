(cl:in-package #:hirundine-graph)

;;; When code is compiled with the file compiler, there are
;;; restrictions on the kind of literals that the result can contain.
;;; It is possible that code has to be generated to create a "similar"
;;; object to the one present in the original source code.  The result
;;; of the creation of such a similar object is then stored in a
;;; "literals vector" to be referenced by code that would otherwise
;;; refer to the literal directly.  Furthermore, the original source
;;; code may contain calls to LOAD-TIME-VALUE.  Again, code needs to
;;; be generated to create a literal that is then stored in the
;;; literals vector.
;;;
;;; This instruction type creates such a literals vector.  A single
;;; instance of this instruction is typically present in a HIR
;;; program, and it then appears at the top level, to be executed when
;;; the code is loaded.

(defclass create-literals-vector-instruction (instruction)
  ())

(setf (documentation 'create-literals-vector-instruction 'type)
      (format nil
              "Class precedence list:~@
               create-literals-vector-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has a single input and~@
               a single output.
               ~@
               An instruction of this type has a single successor~@
               ~@
               The input is a literal non-negative integer indicating~@
               the lenght of the literals vector to be created.~@
               The output is a register.~@
               ~@
               Executing an instruction of this type makes the output~@
               register contain a vector of the length determined~@
               by the input."))
