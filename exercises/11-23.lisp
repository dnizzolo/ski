(defpackage #:ex-11-23
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-23)

(defun robin-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z)))
    (term-equal
     (make-combinator-application (make-combinator-application y z) x)
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z)
              :initial-value term)))))

(defun solve ()
  (loop for n from 1
        for result = (remove-if-not
                      #'robin-p
                      (compute-full-binary-trees n (get-combinator 'C)))
        until result
        finally (report-terms result)))
