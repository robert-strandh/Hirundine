(cl:in-package #:hirundine-graph)

(defun map-instructions-arbitrary-order (function first-instruction)
  (let ((visited (make-hash-table :test #'eq)))
    (labels ((aux (instruction)
               (unless (gethash instruction visited)
                 (setf (gethash instruction visited) t)
                 (funcall function instruction)
                 (mapcar #'aux (successors instruction))
                 (when (typep instruction 'enclose-instruction)
                   (aux (parse-arguments-instruction instruction))))))
      (aux first-instruction))))

(defun initialize-predecessors-and-data (initial-instruction)
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (setf (predecessors instruction) '())
     (loop for input in (inputs instruction)
           do (setf (readers input) '()))
     (loop for output in (outputs instruction)
           do (setf (writers output) '())))
   initial-instruction)
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for successor in (successors instruction)
           do (push instruction (predecessors successor)))
     (loop for input in (inputs instruction)
           do (push instruction (readers input)))
     (loop for output in (outputs instruction)
           do (push instruction (writers output))))
   initial-instruction))

(defun check-hir (initial-instruction)
  (initialize-predecessors-and-data initial-instruction)
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for input in (inputs instruction)
           do (when (typep input 'register)
                (assert (not (null (writers input)))))))
   initial-instruction))
     
