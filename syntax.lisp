(in-package #:ski)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common rules.

(esrap:defrule whitespace*
    (* (or #\Space #\Newline #\Tab))
  (:constant nil))

(esrap:defrule variable
    (and whitespace*
         (or (esrap:character-ranges (#\a #\z))
             (and "{"
                  whitespace*
                  (+ (esrap:character-ranges (#\a #\z)))
                  whitespace*
                  "}"))
         whitespace*)
  (:destructure (w1 var w2)
    (declare (ignore w1 w2))
    (if (atom var) var (third var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Combinatory logic grammar.

(esrap:defrule combinator-term
    (and (esrap:? combinator-term) combinator-factor)
  (:destructure (left right)
    (if left
        (make-combinator-application left right)
        right)))

(esrap:defrule combinator-factor
    (or combinator
        combinator-variable
        parenthesized-combinator-term))

(defun literal-combinator (text position end)
  (let ((length-matched 0)
        (combinator-matched nil))
    (maphash (lambda (name combinator)
               (let* ((lexeme (symbol-name name))
                      (lexeme-length (length lexeme)))
                 (when (and (> lexeme-length length-matched)
                            (string= lexeme text
                                     :start2 position
                                     :end2 (min end (+ position lexeme-length))))
                   (setf length-matched lexeme-length
                         combinator-matched combinator))))
             *combinators*)
    (if combinator-matched
        (values combinator-matched (+ position length-matched) t)
        (values nil position "Expected a combinator"))))

(esrap:defrule combinator
    (and whitespace*
         #'literal-combinator
         whitespace*)
  (:destructure (w1 combinator w2)
    (declare (ignore w1 w2))
    combinator))

(esrap:defrule combinator-variable
    variable
  (:function make-combinator-variable))

(esrap:defrule combinator-application
    (and combinator-term (or combinator combinator-variable combinator-term))
  (:destructure (left right)
    (make-combinator-application left right)))

(esrap:defrule parenthesized-combinator-term
    (and whitespace*
         "("
         combinator-term
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defun parse-combinator-term (input)
  "Parse a COMBINATOR-TERM from the string INPUT and return it."
  (esrap:parse 'combinator-term input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lambda calculus grammar.

(esrap:defrule lambda-term
    (and (esrap:? lambda-term) lambda-factor)
  (:destructure (left right)
    (if left
        (make-lambda-application left right)
        right)))

(esrap:defrule lambda-factor
    (or lambda-abstraction
        lambda-variable
        parenthesized-lambda-term))

(esrap:defrule lambda-abstraction
    (and whitespace*
         (or "λ" "@")
         (+ lambda-variable)
         "."
         lambda-term
         whitespace*)
  (:destructure (w1 lambda variables dot body w2)
    (declare (ignore w1 lambda dot w2))
    (reduce #'make-lambda-abstraction
            variables
            :from-end t
            :initial-value body)))

(esrap:defrule lambda-variable
    variable
  (:function make-lambda-variable))

(esrap:defrule parenthesized-lambda-term
    (and whitespace*
         "("
         lambda-term
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defun parse-lambda-term (input)
  "Parse a LAMBDA-TERM from the string INPUT and return it."
  (esrap:parse 'lambda-term input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common utilities for programs.

(defgeneric read-program (source)
  (:documentation "Read a program from SOURCE, skipping comments. Comments are identified
with the # character and they extend to the end of the line."))

(defmethod read-program ((source stream))
  (with-output-to-string (sink)
    (loop
      (let ((char (read-char source nil)))
        (cond ((null char) (return))
              ((char= char #\#)
               (unread-char char source)
               (read-line source)
               (write-char #\Newline sink))
              (t (write-char char sink)))))))

(defmethod read-program ((source string))
  (read-program (make-string-input-stream source)))

(defmethod read-program ((source pathname))
  (read-program (open source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lambda calculus programs.

(defvar *lambda-program-definitions* nil
  "The definitions in a lambda program to keep track of as we parse
 it in order to resolve references to definitions in terms.")

(esrap:defrule lambda-program
    (and (* lambda-program-definition)
         (+ (and lambda-program-term ";" whitespace*)))
  (:destructure (definitions exprs)
    (declare (ignore definitions))
    (mapcar #'first exprs)))

(esrap:defrule lambda-program-definition-name
    (and whitespace*
         (+ (esrap:character-ranges (#\A #\Z)))
         whitespace*)
  (:text t))

;; When a name appears in term context we must check that it was
;; previously defined, in which case we return the corresponding pure
;; lambda calculus term, otherwise an error is signaled.
(esrap:defrule lambda-program-definition-as-term
    lambda-program-definition-name
  (:lambda (name)
    (loop for definition in *lambda-program-definitions*
          when (string= name (name definition))
            return (term definition)
          finally (error "Undefined term ~a in lambda program." name))))

(esrap:defrule lambda-program-definition
    (and whitespace*
         lambda-program-definition-name
         whitespace*
         "="
         whitespace*
         lambda-program-term
         whitespace*
         ";"
         whitespace*)
  (:destructure (w1 name w2 eq w3 expr w4 semicolon w5)
    (declare (ignore w1 w2 eq w3 w4 semicolon w5))
    (push (make-lambda-program-definition name expr)
          *lambda-program-definitions*)))

(esrap:defrule lambda-program-term
    (and (esrap:? lambda-program-term) lambda-program-factor)
  (:destructure (left right)
    (if left
        (make-lambda-application left right)
        right)))

(esrap:defrule lambda-program-factor
    (or lambda-program-abstraction
        lambda-variable
        lambda-program-definition-as-term
        parenthesized-lambda-program-term))

(esrap:defrule lambda-program-abstraction
    (and whitespace*
         (or "λ" "@")
         (+ lambda-variable)
         "."
         lambda-program-term
         whitespace*)
  (:destructure (w1 lambda variables dot body w2)
    (declare (ignore w1 lambda dot w2))
    (reduce #'make-lambda-abstraction
            variables
            :from-end t
            :initial-value body)))

(esrap:defrule parenthesized-lambda-program-term
    (and whitespace*
         "("
         lambda-program-term
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defclass lambda-program-definition (lambda-term)
  ((name :initarg :name :accessor name)
   (term :initarg :term :accessor term))
  (:documentation "A lambda calculus term defined in a program."))

(defun make-lambda-program-definition (name term)
  "Construct and return a LAMBDA-PROGRAM-DEFINITION called NAME that
 stands for TERM."
  (make-instance 'lambda-program-definition :name name :term term))

(defun lambda-program-definition-p (object)
  "Return true if OBJECT is a LAMBDA-PROGRAM-DEFINITION, and NIL
otherwise."
  (typep object 'lambda-program-definition))

(defmethod print-object ((object lambda-program-definition) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~a)" name))))

(defmethod print-term ((term lambda-program-definition) &optional (stream *standard-output*))
  (princ (name term) stream))

(defun parse-lambda-program (input)
  "Parse a lambda program from INPUT."
  (esrap:parse 'lambda-program (read-program input)))

(defun build-lambda-program (program)
  "Parse the lambda PROGRAM and return a list of pure lambda calculus
terms to be reduced."
  (let ((*lambda-program-definitions* nil))
    (parse-lambda-program program)))

(defun run-lambda-program (program &optional (stream *standard-output*))
  "Parse and run the lambda PROGRAM. Return the last reduced term. Print
each reduced term to STREAM."
  (let (last)
    (dolist (expr (build-lambda-program program) last)
      (setf last (reduce-term expr))
      (print-term last stream)
      (terpri stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Combinatory logic programs.

(esrap:defrule combinator-program
    (and (* combinator-program-definition)
         (+ (and combinator-program-term ";" whitespace*)))
  (:destructure (definitions exprs)
    (let ((definitions-table (make-hash-table :test #'equal)))
      (dolist (def definitions)
        (setf (gethash (name def) definitions-table) def))
      (list definitions-table (mapcar #'first exprs)))))

(esrap:defrule combinator-name
    (and whitespace*
         "@"
         (+ (esrap:character-ranges (#\A #\Z)))
         whitespace*)
  (:destructure (w1 at name w2)
    (declare (ignore w1 at w2))
    (make-combinator-program-definition
     (coerce name 'string)
     nil
     nil)))

(esrap:defrule combinator-program-definition
    (and whitespace*
         combinator-name
         (* combinator-variable)
         "="
         whitespace*
         combinator-program-term
         whitespace*
         ";"
         whitespace*)
  (:destructure (w1 name vars eq w2 expr w3 semicolon w4)
    (declare (ignore w1 eq w2 w3 semicolon w4))
    (make-combinator-program-definition
     (name name)
     (length vars)
     (compile nil (expand-operation vars expr)))))

(esrap:defrule combinator-program-term
    (and (esrap:? combinator-program-term) combinator-program-factor)
  (:destructure (left right)
    (if left
        (make-combinator-application left right)
        right)))

(esrap:defrule combinator-program-factor
    (or combinator
        combinator-variable
        combinator-name
        parenthesized-combinator-program-term))

(esrap:defrule parenthesized-combinator-program-term
    (and whitespace*
         "("
         combinator-program-term
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defclass combinator-program-definition (combinator-term)
  ((name :initarg :name :accessor name)
   (arity :initarg :arity :accessor arity)
   (operation :initarg :operation :accessor operation))
  (:documentation "A combinatory logic term defined in a program."))

(defun make-combinator-program-definition (name arity operation)
  "Construct and return a COMBINATOR-PROGRAM-DEFINITION that refers to
 NAME has ARITY and performs OPERATION."
  (make-instance 'combinator-program-definition
                 :name name
                 :arity arity
                 :operation operation))

(defun combinator-program-definition-p (object)
  "Return true if OBJECT is a COMBINATOR-PROGRAM-DEFINITION, and NIL
otherwise."
  (typep object 'combinator-program-definition))

(defmethod print-object ((object combinator-program-definition) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~a)" name))))

(defmethod print-term ((term combinator-program-definition) &optional (stream *standard-output*))
  (format stream "@~a" (name term))
  term)

(defun expand-operation (variables expr)
  "Compute the lambda form that implements the operation of a combinator
definition."
  (labels ((operation-form (term)
             (etypecase term
               (combinator-variable
                (if (member term variables :test #'same-variable-p)
                    (variable->symbol term)
                    term))
               (combinator-application
                `(make-combinator-application
                  ,(operation-form (left term))
                  ,(operation-form (right term))))
               (combinator-term term))))
    (let ((stack-variable (gensym))
          (let-variables (mapcar #'variable->symbol variables)))
      `(lambda (,stack-variable)
         (let ,(expand-bindings-for-stack-access let-variables stack-variable)
           (declare (ignorable ,@let-variables))
           (values ,(operation-form expr)
                   (nthcdr ,(length variables) ,stack-variable)))))))

(defparameter *combinator-program-definitions* nil
  "The table of defined combinatory logic terms in a combinator
 program to be consulted when running the program.")

(defmethod step-combinator-term ((term combinator-program-definition) (stack list))
  (let ((definition (gethash
                     (name term)
                     *combinator-program-definitions*)))
    (cond ((null definition)
           (error "Undefined term ~a in combinator program." term))
          ((<= (arity definition) (length stack))
           (funcall (operation definition) stack))
          (t (call-next-method)))))

(defun parse-combinator-program (input)
  "Parse a combinator program from INPUT."
  (esrap:parse 'combinator-program (read-program input)))

(defun run-combinator-program (program &optional (stream *standard-output*))
  "Parse and run the combinator PROGRAM. Return the last reduced term. Print
each reduced term to STREAM."
  (destructuring-bind (definitions expressions)
      (parse-combinator-program program)
    (let ((*combinator-program-definitions* definitions)
          last)
      (dolist (expr expressions last)
        (setf last (reduce-term expr))
        (print-term last stream)
        (terpri stream)))))
