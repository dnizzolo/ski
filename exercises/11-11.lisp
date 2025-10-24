(defpackage #:ex-11-11
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-11)

(defun solve ()
  (do ((dovekie-p (make-extensional-equality-predicate (get-combinator 'D2)))
       (bluebird (get-combinator 'B))
       (n 1 (1+ n)))
      (())
    (let ((result (remove-if-not
                   dovekie-p
                   (full-binary-trees n :leaf bluebird))))
      (when result
        (report-terms result)
        (return)))))
