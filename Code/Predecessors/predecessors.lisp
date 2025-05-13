(cl:in-package #:hirundine-predecessors)

(defgeneric table (predecessors))

(defclass predecessors ()
  ((%table :initform (make-hash-table :test #'eq) :reader table)))
  
(defun predecessors (instruction predecessors)
  (gethash instruction (table predecessors)))

(defun compute-predecessors (initial-instruction)
  (let* ((result (make-instance 'predecessors))
         (table (table result)))
    (utilities:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for successor in (graph:successors instruction)
             do (push instruction (gethash successor table '()))))
     initial-instruction)
    result))
