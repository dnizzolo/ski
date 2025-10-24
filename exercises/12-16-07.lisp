(defpackage #:ex-12-16-07
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16-07)

(defun wrap (term)
  (make-combinator-application
   (make-combinator-application
    (get-combinator 'C)
    term)
   (get-combinator 'W)))

(defun solve ()
  (report-terms
   (remove-if-not
    (make-extensional-equality-predicate (get-combinator 'S))
    (full-binary-trees 6 :leaf (get-combinator 'Q))
    :key #'wrap)))
