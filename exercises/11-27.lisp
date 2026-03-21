(defpackage #:ex-11-27
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-27)

(defun solve ()
  (loop with vireo-p = (make-extensional-equality-predicate (get-combinator 'V))
        with allowed = (list (get-combinator 'C) (get-combinator 'F))
        for n from 1
        for terms = (search-n-terms allowed n vireo-p)
        until terms
        finally (report-terms terms)))
