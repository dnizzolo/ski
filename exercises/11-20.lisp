(defpackage #:ex-11-20
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-20)

(defun solve ()
  (do ((robin-p (make-extensional-equality-predicate (get-combinator 'R)))
       (allowed (list (get-combinator 'B) (get-combinator 'T)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n robin-p)))
      (when result
        (report-terms result)
        (return)))))
