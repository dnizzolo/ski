(defpackage #:ex-11-27
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-27)

(defun solve ()
  (do ((vireo-p (make-extensional-equality-predicate (get-combinator 'V)))
       (allowed (list (get-combinator 'C) (get-combinator 'F)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n vireo-p)))
      (when result
        (report-terms result)
        (return)))))
