(defpackage #:ex-11-23
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-23)

(defun solve ()
  (do ((robin-p (extensionally-equal (get-combinator 'R)))
       (cardinal (get-combinator 'C))
       (n 1 (1+ n)))
      (())
    (let ((result (remove-if-not
                   robin-p
                   (full-binary-trees n :leaf cardinal))))
      (when result
        (report-terms result)
        (return)))))
