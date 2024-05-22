(defpackage #:ex-11-10
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-10)

(defun becard-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z))
        (w (make-combinator-variable #\w)))
    (term-equal
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z w)
              :initial-value (get-combinator 'B3)))
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z w)
              :initial-value term)))))

(defun solve ()
  (loop for n from 1
        for result = (remove-if-not
                      #'becard-p
                      (compute-full-binary-trees n (get-combinator 'B)))
        until result
        finally (report-terms result)))
