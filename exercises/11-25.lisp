(defpackage #:ex-11-25
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-25)

(defun solve ()
  (loop with finch-p = (make-extensional-equality-predicate (get-combinator 'F))
        with allowed = (list (get-combinator 'E) (get-combinator 'T))
        for n from 1
        for terms = (search-n-terms allowed n finch-p)
        until terms
        finally (report-terms terms)))
