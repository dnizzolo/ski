(defpackage #:ex-11-11
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-11)

(defun dovekie-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z))
        (w (make-combinator-variable #\w))
        (v (make-combinator-variable #\v)))
    (term-equal
     (reduce-term (parse-combinator-term "D2xyzwv"))
     (reduce-term
      (make-combinator-application
       (make-combinator-application
        (make-combinator-application
         (make-combinator-application
          (make-combinator-application
           term
           x)
          y)
         z)
        w)
       v)))))

(defun solve ()
  (loop for n from 1
        for result = (remove-if-not
                      #'dovekie-p
                      (compute-full-binary-trees n (get-combinator 'B)))
        until result
        finally (report-terms result)))
