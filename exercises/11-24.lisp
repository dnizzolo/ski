(defpackage #:ex-11-24
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-24)

(defun solve ()
  (do ((finch-p (extensionally-equal (get-combinator 'F)))
       (allowed (list (get-combinator 'B) (get-combinator 'C)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n finch-p)))
      (when result
        (report-terms result)
        (return)))))
