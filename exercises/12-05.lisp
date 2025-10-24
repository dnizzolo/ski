(defpackage #:ex-12-05
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-05)

(defun solve ()
  (do ((converse-warbler-p (make-extensional-equality-predicate (get-combinator 'W1)))
       (allowed (list (get-combinator 'R) (get-combinator 'M2)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n converse-warbler-p)))
      (when result
        (report-terms result)
        (return)))))
