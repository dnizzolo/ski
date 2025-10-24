(defpackage #:ex-11-06
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-06)

(defun solve ()
  (do ((blackbird-p (make-extensional-equality-predicate (get-combinator 'B1)))
       (bluebird (get-combinator 'B))
       (n 1 (1+ n)))
      (())
    (let ((result (remove-if-not
                   blackbird-p
                   (full-binary-trees n :leaf bluebird))))
      (when result
        (report-terms result)
        (return)))))
