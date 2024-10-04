(defpackage #:ex-utils
  (:use #:cl #:ski)
  (:shadowing-import-from #:ski #:variable)
  (:export
   #:report-terms
   #:full-binary-trees
   #:candidates
   #:substitute-nils
   #:search-n-terms
   #:extensionally-equal))

(in-package #:ex-utils)

(defun report-terms (terms)
  "Display to standard output each term in the list TERMS."
  (format t "~&Got ~d term~:p." (length terms))
  (dolist (term terms)
    (fresh-line)
    (print-term term))
  (values))

(defun full-binary-trees (n &key leaf (node #'make-combinator-application))
  "Return a list of all binary trees with N leaves. LEAF is used for all
the leaves of the trees. NODE is the binary function used to build the
trees."
  (let ((table (make-array n :initial-element nil)))
    (setf (aref table 0) (list leaf))
    (loop for j from 1 below n do
      (loop for i from 0 below j do
        (loop for left in (aref table i) do
          (loop for right in (aref table (- j i 1)) do
            (push (funcall node left right)
                  (aref table j))))))
    (aref table (1- n))))

(defun candidates (set n)
  "Return a list of all permutations with repetitions of
SET with length N."
  (loop with result = (list nil)
        repeat n
        do (setf result
                 (loop for item in result
                       nconc (loop for elem in set collect (cons elem item))))
        finally (return result)))

(defun substitute-nils (object replacements)
  "Substitute all NILs in OBJECT with REPLACEMENTS, in order."
  (if (combinator-application-p object)
      (with-accessors ((left left) (right right)) object
        (multiple-value-bind (new-left replacements-after-left)
            (substitute-nils left replacements)
          (multiple-value-bind (new-right replacements-after-right)
              (substitute-nils right replacements-after-left)
            (values (make-combinator-application new-left new-right)
                    replacements-after-right))))
      (if (null object)
          (if (null replacements)
              (error "Cannot substitute ~a because there are no more replacements."
                     object)
              (values (first replacements) (rest replacements)))
          (values object replacements))))

(defun search-n-terms (set n criterion)
  "Search all terms with N operators from SET and return those that
satisfy CRITERION."
  (loop with partials = (full-binary-trees n)
        with candidates = (candidates set n)
        for part in partials
        nconc (loop for candidate in candidates
                    for candidate-term = (substitute-nils part candidate)
                    when (funcall criterion candidate-term)
                      collect candidate-term)))

(defun extensionally-equal (reference)
  "Return a function that takes a TERM and returns non-NIL if TERM is
functionally equivalent to REFERENCE. REFERENCE must be a combinatory
logic object with an ARITY."
  (let ((vars (loop with arity = (arity reference)
                    with g = (make-variable-name-generator)
                    repeat arity
                    collect (make-combinator-variable (generate-name g)))))
    (lambda (term)
      (term-equal
       (reduce-term (reduce #'make-combinator-application vars :initial-value reference))
       (reduce-term (reduce #'make-combinator-application vars :initial-value term))))))
