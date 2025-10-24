(defpackage #:ex-12-12
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-12)

(defun solve ()
  (do ((starling-p (make-extensional-equality-predicate (get-combinator 'S)))
       (allowed (list (get-combinator 'B) (get-combinator 'W**) (get-combinator 'G)))
       (n 1 (1+ n)))
      (())
    (let ((result (search-n-terms allowed n starling-p)))
      (when result
        (report-terms result)
        (return)))))
