(defpackage #:ex-11-10
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-10)

(defun solve ()
  (do ((becard-p (extensionally-equal (get-combinator 'B3)))
       (bluebird (get-combinator 'B))
       (n 1 (1+ n)))
      (())
    (let ((result (remove-if-not
                   becard-p
                   (full-binary-trees n :leaf bluebird))))
      (when result
        (report-terms result)
        (return)))))
