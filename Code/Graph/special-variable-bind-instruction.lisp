(cl:in-package #:hirundine-graph)

(defclass special-variable-bind-instruction (instruction)
  ())

(setf (documentation 'special-variable-bind-instruction 'type)
      (format nil
              "Class precedence list:~@
               special-variable-bind-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has two or three inputs~@
               and one output.  The first input is a register containing~@
               a dynamic-environment object.  The second input is~@
               a register or a literal.  If present, the third input~@
               is a register or a literal.  The output is a register~@
               that contains a dynamic-environment object.
               ~@
               An instruction of this type has a single successor.~@
               ~@
               The result of executing an instruction of this type~@
               is that a new binding is created for the special~@
               variable named by the second input.  The output~@
               dynamic-environment object contains that new binding.~@
               If the third input is present, then its value is the~@
               value of the new binding.  If the third input is not~@
               present, the new binding will have no values, as is~@
               possible if the new binding was created using PROGV."))
