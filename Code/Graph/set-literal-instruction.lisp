(cl:in-package #:hirundine-graph)

(defclass set-literal-instruction (instruction)
  ())

(setf (documentation 'set-literal-instruction 'type)
      (format nil
              "Class precedence list:~@
               set-literal-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has two inputs and no~@
               outputs.  The first input is a small non-negative~@
               integer literal, indicating an index into the unique~@
               literals vector of this HIR program.  The second input~@
               is a register containing the object that will be stored~@
               in the literals vector at the given index.~@
               ~@
               An instruction of this type has a single successor."))
