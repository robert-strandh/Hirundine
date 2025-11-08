(cl:in-package #:hirundine-graph)

(defclass nop-instruction (instruction)
  ())

(setf (documentation 'nop-instruction 'type)
      (format nil
              "Class precedence list:~@
               nop-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has no inputs and no~@
               outputs.
               ~@
               An instruction of this type has a single successor~@
               ~@
               Executing an instruction of this type has no effect."))
