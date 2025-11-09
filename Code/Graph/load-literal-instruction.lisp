(cl:in-package #:hirundine-graph)

(defclass load-literal-instruction (instruction)
  ())

(setf (documentation 'load-literal-instruction 'type)
      (format nil
              "Class precedence list:~@
               load-literal-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has one input and one~@
               output.  The input is a small non-negative integer~@
               literal, indicating an index into the unique literals~@
               vector of this HIR program.  The output is a register~@
               that will contain the literal at the given index in the~@
               literals vector.~@
               ~@
               An instruction of this type has a single successor."))
