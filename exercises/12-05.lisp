(defpackage #:ex-12-05
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-05)

(defun solve ()
  (loop with converse-warbler-p = (make-extensional-equality-predicate (get-combinator 'W1))
        with allowed = (list (get-combinator 'R) (get-combinator 'M2))
        for n from 1
        for terms = (search-n-terms allowed n converse-warbler-p)
        until terms
        finally (report-terms terms)))
