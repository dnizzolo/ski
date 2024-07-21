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
    combinator-expr)

(esrap:defrule combinator-expr
    (and (esrap:? combinator-expr) combinator-factor)
  (:destructure (left right)
    (if left
        (make-combinator-application left right)
        right)))

(esrap:defrule combinator-factor
    (or combinator
        combinator-variable
        parenthesized-combinator-term))

(esrap:defrule combinator
    (and whitespace*
         (or "Q1"
             "Q2"
             "Q3"
             "Q4"
             "Q"
             "B1"
             "B2"
             "B3"
             "D1"
             "D2"
             "I2"
             "G1"
             "G2"
             "K2"
             "J1"
             "M2"
             "R**"
             "R*"
             "R"
             "S1"
             "F**"
             "F*"
             "H*"
             "C**"
             "C*"
             "W**"
             "W*"
             "w1"
             "V**"
             "V*"
             "Θ"
             "Ψ"
             "Φ"
             "Γ"
             "Ê"
             "O"
             (esrap:character-ranges (#\B #\M) (#\S #\W)))
         whitespace*)
  (:destructure (w1 combinator w2)
    (declare (ignore w1 w2))
    (get-combinator (intern (string combinator)))))

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
    lambda-expr)

(esrap:defrule lambda-expr
    (and (esrap:? lambda-expr) lambda-factor)
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
            :initial-value body
            :from-end t)))

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
  (:documentation "Read a program from SOURCE skipping comments. Comments are identified
with the # character and extend to the end of the line."))

(defmethod read-program ((source stream))
  (with-output-to-string (sink)
    (loop
      (let ((char (read-char source nil)))
        (cond ((null char)
               (return))
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

(esrap:defrule lambda-program
    (and (* lambda-definition)
         (+ (and lambda-program-expr ";" whitespace*)))
  (:destructure (definitions exprs)
    (list (coerce definitions 'vector)
          (mapcar #'first exprs))))

(esrap:defrule lambda-name
    (and whitespace*
         (+ (esrap:character-ranges (#\A #\Z)))
         whitespace*)
  (:text t))

(esrap:defrule lambda-definition
    (and whitespace*
         lambda-name
         whitespace*
         "="
         whitespace*
         lambda-program-expr
         whitespace*
         ";"
         whitespace*)
  (:destructure (w1 name w2 eq w3 expr w4 semicolon w5)
    (declare (ignore w1 w2 eq w3 w4 semicolon w5))
    (cons (coerce name 'string) expr)))

(esrap:defrule lambda-program-expr
    (and (esrap:? lambda-program-expr) lambda-program-factor)
  (:destructure (left right)
    (if left
        (make-lambda-application left right)
        right)))

(esrap:defrule lambda-program-factor
    (or lambda-program-abstraction
        lambda-variable
        lambda-name
        parenthesized-lambda-program-expr))

(esrap:defrule lambda-program-abstraction
    (and whitespace*
         (or "λ" "@")
         (+ lambda-variable)
         "."
         lambda-program-expr
         whitespace*)
  (:destructure (w1 lambda variables dot body w2)
    (declare (ignore w1 lambda dot w2))
    (reduce #'make-lambda-abstraction
            variables
            :initial-value body
            :from-end t)))

(esrap:defrule parenthesized-lambda-program-expr
    (and whitespace*
         "("
         lambda-program-expr
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defgeneric lambda-names (term)
  (:documentation "Return all unresolved names in the lambda calculus TERM."))

(defmethod lambda-names ((term string))
  (list term))

(defmethod lambda-names ((term lambda-variable))
  nil)

(defmethod lambda-names ((term lambda-abstraction))
  (lambda-names (body term)))

(defmethod lambda-names ((term lambda-application))
  (append (lambda-names (left term)) (lambda-names (right term))))

(defgeneric substitute-definitions (definitions term)
  (:documentation "Recursively replace all occurrences of names in the lambda calculus
TERM with their DEFINITIONS."))

(defmethod substitute-definitions (definitions (term lambda-variable))
  term)

(defmethod substitute-definitions (definitions (term string))
  (substitute-definitions
   definitions
   (cdr (find term definitions :key #'car :test #'string=))))

(defmethod substitute-definitions (definitions (term lambda-abstraction))
  (with-accessors ((variable variable) (body body))
      term
    (make-lambda-abstraction
     variable
     (substitute-definitions definitions body))))

(defmethod substitute-definitions (definitions (term lambda-application))
  (with-accessors ((left left) (right right)) term
    (make-lambda-application
     (substitute-definitions definitions left)
     (substitute-definitions definitions right))))

(defun parse-lambda-program (input)
  "Parse a lambda program from INPUT."
  (esrap:parse 'lambda-program (read-program input)))

(defun build-lambda-program (program)
  "Parse the lambda PROGRAM and return a list of pure lambda calculus
terms to be reduced."
  (flet ((build (definitions expressions)
           (dotimes (i (length definitions))
             (let* ((curr-expr (cdr (aref definitions i)))
                    (items (lambda-names curr-expr)))
               (dolist (item items)
                 (or (dotimes (j i)
                       (when (string= item (car (aref definitions j)))
                         (return t)))
                     (error "Definition of ~a contains undefined term ~a."
                            (car (aref definitions i))
                            item)))))
           (mapcar (lambda (expr)
                     (substitute-definitions definitions expr))
                   expressions)))
    (multiple-value-call #'build (values-list (parse-lambda-program program)))))

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
    (and (* combinator-definition)
         (+ (and combinator-program-expr ";" whitespace*)))
  (:destructure (definitions exprs)
    (list definitions (mapcar #'first exprs))))

(esrap:defrule combinator-name
    (and whitespace*
         "@"
         (+ (esrap:character-ranges (#\A #\Z)))
         whitespace*)
  (:destructure (w1 at name w2)
    (declare (ignore w1 at w2))
    (make-combinator-definition
     (coerce name 'string)
     nil
     nil)))

(esrap:defrule combinator-definition
    (and whitespace*
         combinator-name
         (* combinator-variable)
         "="
         whitespace*
         combinator-program-expr
         whitespace*
         ";"
         whitespace*)
  (:destructure (w1 name vars eq w2 expr w3 semicolon w4)
    (declare (ignore w1 eq w2 w3 semicolon w4))
    (make-combinator-definition
     (name name)
     (length vars)
     (compile nil (expand-operation vars expr)))))

(esrap:defrule combinator-program-expr
    (and (esrap:? combinator-program-expr) combinator-program-factor)
  (:destructure (left right)
    (if left
        (make-combinator-application left right)
        right)))

(esrap:defrule combinator-program-factor
    (or combinator
        combinator-variable
        combinator-name
        parenthesized-combinator-program-expr))

(esrap:defrule parenthesized-combinator-program-expr
    (and whitespace*
         "("
         combinator-program-expr
         ")"
         whitespace*)
  (:destructure (w1 open-parenthesis term close-parenthesis w2)
    (declare (ignore w1 open-parenthesis close-parenthesis w2))
    term))

(defclass combinator-definition (combinator-term)
  ((name :initarg :name :accessor name)
   (arity :initarg :arity :accessor arity)
   (operation :initarg :operation :accessor operation))
  (:documentation "A combinatory logic term defined in a program."))

(defun make-combinator-definition (name arity operation)
  "Construct and return a COMBINATOR-DEFINITION that refers to NAME has
ARITY and performs OPERATION."
  (make-instance 'combinator-definition
                 :name name
                 :arity arity
                 :operation operation))

(defun combinator-definition-p (object)
  "Return true if OBJECT is a COMBINATOR-DEFINITION, and NIL otherwise."
  (typep object 'combinator-definition))

(defmethod print-object ((object combinator-definition) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~a)" name))))

(defmethod print-term ((term combinator-definition)
                       &optional (stream *standard-output*))
  (format stream "@~a" (name term))
  term)

(defgeneric operation-form (term variables)
  (:documentation "Compute the form of a combinator operation."))

(defmethod operation-form ((term combinator-term) variables)
  term)

(defmethod operation-form ((term combinator-variable) variables)
  (if (member term variables :test #'same-variable-p)
      (variable->symbol term)
      term))

(defmethod operation-form ((term combinator-application) variables)
  `(make-combinator-application
    ,(operation-form (left term) variables)
    ,(operation-form (right term) variables)))

(defun expand-operation (variables expr)
  "Compute the lambda form that implements the operation of a combinator
definition."
  (let ((stack-variable (gensym))
        (let-variables (mapcar #'variable->symbol variables)))
    `(lambda (,stack-variable)
       (let ,(expand-bindings-for-stack-access
              let-variables
              stack-variable)
         (declare (ignorable ,@let-variables))
         (values ,(operation-form expr variables)
                 (nthcdr ,(length variables) ,stack-variable))))))

(defparameter *combinator-program-definitions* nil
  "The table of defined combinatory logic terms in a combinator program.")

(defun lookup-combinator-definition (def)
  "Return the combinatory logic term associated with DEF."
  (find (name def)
        *combinator-program-definitions*
        :key #'name
        :test #'string=))

(defmethod step-combinator-term ((term combinator-definition) (stack list))
  (let ((def (lookup-combinator-definition term)))
    (cond ((null def)
           (error "Undefined combinator term ~a." term))
          ((<= (arity def) (length stack))
           (funcall (operation def) stack))
          (t (call-next-method)))))

(defun parse-combinator-program (input)
  "Parse a combinator program from INPUT."
  (esrap:parse 'combinator-program (read-program input)))

(defun run-combinator-program (program &optional (stream *standard-output*))
  "Parse and run the combinator PROGRAM. Return the last reduced term. Print
each reduced term to STREAM."
  (destructuring-bind (definitions exprs)
      (parse-combinator-program program)
    (let ((*combinator-program-definitions* definitions)
          last)
      (dolist (expr exprs last)
        (setf last (reduce-term expr))
        (print-term last stream)
        (terpri stream)))))
