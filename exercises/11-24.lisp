(defpackage #:ex-11-24
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-24)

(defun solve ()
  (loop with finch-p = (make-extensional-equality-predicate (get-combinator 'F))
        with allowed = (list (get-combinator 'B) (get-combinator 'C))
        for n from 1
        for terms = (search-n-terms allowed n finch-p)
        until terms
        finally (report-terms terms)))
