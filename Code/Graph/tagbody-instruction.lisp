(cl:in-package #:hirundine-graph)

(defclass tagbody-instruction (instruction)
  ())

(setf (documentation 'tagbody-instruction 'type)
      (format nil
              "Class precedence list:~@
               tagbody-instruction, instruction, standard-object, t~@
               ~@
               An instruction of this type has one input and two~@
               outputs.  The input is a register containing a~@
               dynamic-environment object.  Each of the two outputs~@
               is a register.
               ~@
               An instruction of this type has a single successor.~@
               ~@
               The result of executing an instruction of this type~@
               normally, is that the register of the first output~@
               contains a dynamic-environment object which is like~@
               the one in the input, except that it is augmented with~@
               a new exit point, and the register of the second output~@
               contains a unique identifier for this exit point.~@
               That unique identifier is used by the GO-INSTRUCTION~@
               to find the unique exit point where control is to be~@
               transferred."))
