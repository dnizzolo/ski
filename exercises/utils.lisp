(defpackage #:ex-utils
  (:use #:cl #:ski)
  (:shadowing-import-from #:ski #:variable)
  (:export
   #:report-terms
   #:compute-full-binary-trees
   #:candidates
   #:substitute-nils
   #:search-n-terms))

(in-package #:ex-utils)

(defun report-terms (terms)
  "Display to standard output each term in the list TERMS."
  (format t "~&Got ~D term~:p." (length terms))
  (dolist (term terms)
    (fresh-line)
    (print-term term))
  (values))

(defun compute-full-binary-trees (n &optional leaf (builder #'make-combinator-application))
  "Return a list of all binary trees with N leaves. Optional LEAF is used
for all the leaves of the trees, default is NIL. BUILDER is the binary
function used to build the trees, default is
MAKE-COMBINATOR-APPLICATION."
  (let ((table (make-array n :initial-element nil)))
    (setf (aref table 0) (list leaf))
    (loop for j from 1 below n do
      (loop for i from 0 below j do
        (loop for left in (aref table i) do
          (loop for right in (aref table (- j i 1)) do
            (push (funcall builder left right)
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
      (with-accessors ((left application-left) (right application-right)) object
        (multiple-value-bind (new-left replacements-after-left)
            (substitute-nils left replacements)
          (multiple-value-bind (new-right replacements-after-right)
              (substitute-nils right replacements-after-left)
            (values (make-combinator-application new-left new-right)
                    replacements-after-right))))
      (if (null object)
          (if (null replacements)
              (error "Cannot substitute ~A because there are no more replacements."
                     object)
              (values (first replacements) (rest replacements)))
          (values object replacements))))

(defun search-n-terms (set n criterion)
  "Search all terms with N operators from SET and return those that
satisfy CRITERION."
  (loop with partials = (compute-full-binary-trees n)
        with candidates = (candidates set n)
        for part in partials
        nconc (loop for candidate in candidates
                    for candidate-term = (substitute-nils part candidate)
                    when (funcall criterion candidate-term)
                      collect candidate-term)))
