(defpackage #:ex-11-23
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-23)

(defun solve ()
  (loop with robin-p = (make-extensional-equality-predicate (get-combinator 'R))
        with cardinal = (get-combinator 'C)
        for n from 1
        for terms = (remove-if-not robin-p (full-binary-trees n :leaf cardinal))
        until terms
        finally (report-terms terms)))
