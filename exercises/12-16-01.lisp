(defpackage #:ex-12-16-01
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16-01)

(defun solve ()
  (do ((good-p (make-extensional-equality-predicate (get-combinator 'G1)))
       (allowed (list (get-combinator 'B) (get-combinator 'G)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n good-p)))
      (when result
        (report-terms result)
        (return)))))
