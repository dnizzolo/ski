(defpackage #:ex-11-10
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-10)

(defun solve ()
  (loop with becard-p = (make-extensional-equality-predicate (get-combinator 'B3))
        with bluebird = (get-combinator 'B)
        for n from 1
        for terms = (remove-if-not becard-p (full-binary-trees n :leaf bluebird))
        until terms
        finally (report-terms terms)))
