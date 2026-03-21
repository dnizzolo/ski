(defpackage #:ex-12-12
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-12)

(defun solve ()
  (loop with starling-p = (make-extensional-equality-predicate (get-combinator 'S))
        with allowed = (list (get-combinator 'B) (get-combinator 'W**) (get-combinator 'G))
        for n from 1
        for terms = (search-n-terms allowed n starling-p)
        until terms
        finally (report-terms terms)))
