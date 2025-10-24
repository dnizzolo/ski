(defpackage #:ex-11-25
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-25)

(defun solve ()
  (do ((finch-p (make-extensional-equality-predicate (get-combinator 'F)))
       (allowed (list (get-combinator 'E) (get-combinator 'T)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n finch-p)))
      (when result
        (report-terms result)
        (return)))))
