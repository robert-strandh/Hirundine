(cl:in-package #:hirundine-utilities)

;;; Traverse an instruction graph in some arbitrary order.
;;; INITIAL-INSTRUCTION is the root of the instruction graph.
;;; FUNCTION will be called exactly once for each instruction in the
;;; instruction graph, and it will be called with that instruction as
;;; the only argument.

(defun map-instructions-arbitrary-order (function initial-instruction)
  (let ((visited-instructions (make-hash-table :test #'eq))
        (instructions-to-process '()))
    (flet ((register-if-unvisited (instruction)
             (unless (gethash instruction visited-instructions)
               (setf (gethash instruction visited-instructions) t)
               (push instruction instructions-to-process))))
      (register-if-unvisited initial-instruction)
      (loop until (null instructions-to-process)
            do (let ((instruction (pop instructions-to-process)))
                 (when (typep instruction 'graph:enclose-instruction)
                   ;; When the instruction is an ENCLOSE-INSTRUCTION,
                   ;; we must also account for the slot
                   ;; PARSE-ARGUMENTS-INSTRUCTION of the instruction,
                   ;; because it contains the
                   ;; PARSE-ARGUMENTS-INSTRUCTION of a nested
                   ;; function.
                   (register-if-unvisited
                    (graph:parse-arguments-instruction instruction)))
                 ;; For each successor of the current instruction,
                 ;; register it so that it will be processed
                 ;; ultimately, unless, of course, it has already been
                 ;; processed.
                 (loop for successor in (graph:successors instruction)
                       do (register-if-unvisited successor)))))
    (loop for instruction being each hash-key of visited-instructions
          do (funcall function instruction))))
