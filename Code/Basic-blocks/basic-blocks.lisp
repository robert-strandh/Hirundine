(cl:in-package #:hirundine-basic-blocks)

(defun instruction-is-leader-p (instruction predecessors)
  (let ((predecessors
          (predecessors:predecessors instruction predecessors)))
    (or (/= (length predecessors) 1)
        (/= (length (graph:successors (first predecessors))) 1))))

(defun instruction-is-trailer-p (instruction predecessors)
  (let ((successors (graph:successors instruction)))
    (or (/= (length successors) 1)
        (/= (length (predecessors:predecessors
                     (first successors) predecessors))
            1))))

(defgeneric leader (basic-block))

(defgeneric trailer (basic-block))

(defclass basic-block ()
  ((%leader :initarg :leader :accessor leader)
   (%trailer :initarg :trailer :accessor trailer)))

(defun fill-basic-block (basic-block predecessors)
  (loop with last = (trailer basic-block)
        until (instruction-is-trailer-p last predecessors)
        do (setf last (first (graph:successors last)))
        finally (setf (trailer basic-block) last)))

(defun compute-basic-blocks (initial-instruction)
  (let ((predecessors
          (predecessors:compute-predecessors initial-instruction))
        (basic-blocks '()))
    (utilities:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (instruction-is-leader-p instruction predecessors)
         (push (make-instance 'basic-block
                 :leader instruction
                 :trailer instruction)
               basic-blocks)))
     initial-instruction)
    (loop for basic-block in basic-blocks
          do (fill-basic-block basic-block predecessors))
    basic-blocks))
