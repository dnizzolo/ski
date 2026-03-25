(defpackage #:ex-11-06
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-06)

(defun solve ()
  (loop with blackbird-p = (make-extensional-equality-predicate (get-combinator 'B1))
        with bluebird = (get-combinator 'B)
        for n from 1
        for terms = (delete-if-not blackbird-p (full-binary-trees n :leaf bluebird))
        until terms
        finally (report-terms terms)))
