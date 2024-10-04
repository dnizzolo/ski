(defpackage #:ex-12-16
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16)

(defun solve ()
  (do ((mockingbird-p (extensionally-equal (get-combinator 'M)))
       (allowed (list (get-combinator 'S) (get-combinator 'T)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n mockingbird-p)))
      (when result
        (report-terms result)
        (return)))))
