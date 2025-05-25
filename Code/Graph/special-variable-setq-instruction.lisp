(cl:in-package #:hirundine-graph)

(defclass special-variable-setq-instruction (instruction)
  ((%variable-name
    :initarg :variable-name
    :reader variable-name)))

(setf (documentation 'special-variable-setq-instruction 'type)
      (format nil
              "Class precedence list:~@
               special-variable-setq-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has two inputs and no~@
               outputs.  The first input is a register containing~@
               a dynaic-environment object.  The second input is~@
               a register or a literal.
               ~@
               An instruction of this type has a single successor.~@
               It also has an additional slot containing the name~@
               of a special variable.
               ~@
               The result of executing an instruction of this type~@
               is that the value of the special variable named in~@
               the additional slot in the dynamic environment of~@
               the first input, is changed to be the value of the~@
               second input."))
