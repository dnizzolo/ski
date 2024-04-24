(defpackage #:ex-11-06
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-06)

(defun blackbird-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z))
        (w (make-combinator-variable #\w)))
    (term-equal
     (reduce-term (parse-combinator-term "B1xyzw"))
     (reduce-term
      (make-combinator-application
       (make-combinator-application
        (make-combinator-application
         (make-combinator-application
          term
          x)
         y)
        z)
       w)))))

(defun solve ()
  (loop for n from 1
        for result = (remove-if-not
                      #'blackbird-p
                      (compute-full-binary-trees n (get-combinator 'B)))
        until result
        finally (report-terms result)))
