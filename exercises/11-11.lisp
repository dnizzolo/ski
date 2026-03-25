(defpackage #:ex-11-11
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-11)

(defun solve ()
  (loop with dovekie-p = (make-extensional-equality-predicate (get-combinator 'D2))
        with bluebird = (get-combinator 'B)
        for n from 1
        for terms = (delete-if-not dovekie-p (full-binary-trees n :leaf bluebird))
        until terms
        finally (report-terms terms)))
