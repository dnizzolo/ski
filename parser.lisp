(defpackage #:new-lambda-parser
  (:use #:cl #:ski)
  (:shadow #:parse-lambda-term)
  (:shadowing-import-from #:ski #:variable))

(in-package #:new-lambda-parser)

;;;; Grammar.
;;;;
;;;; digit = "0" ... "9" ;
;;;; lower-alpha = "a" ... "z" ;
;;;; ident = ( lower-alpha | "_" ) ( lower-alpha | "_" | digit )* ;
;;;; abs = "λ" ident+ "." lam ;
;;;; atom = ident | abs | "(" lam ")" ;
;;;; lam = atom ( " "+ lam )* ;
;;;; binding = ident "=" lam ;
;;;; toplevel = binding | lam ;

(deftype token-type ()
  '(member
    :eof
    :lambda :dot
    :identifier :binding-name :equal
    :left-paren :right-paren))

(defclass token ()
  ((token-type :initarg :token-type :reader token-type :type token-type)
   (lexeme :initarg :lexeme :reader lexeme :type string)
   (line :initarg :line :reader line)
   (column :initarg :column :reader column)
   (literal :initarg :literal :reader literal))
  (:default-initargs
   :token-type (error "Token type required.")
   :lexeme (error "Lexeme required.")
   :line (error "Line required.")
   :column (error "Column required.")
   :literal nil))

(defun make-token (token-type lexeme line column &optional literal)
  (check-type token-type token-type)
  (check-type lexeme string)
  (make-instance 'token
                 :token-type token-type
                 :lexeme lexeme
                 :line line
                 :column column
                 :literal literal))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (token-type lexeme line column literal) object
      (format stream "~s ~s~@[ ~a~] At ~d:~d"
              token-type lexeme literal line column))))

(defclass scanner ()
  ((source :initarg :source :reader source :type string)
   (tokens :initform (make-array 32 :adjustable t :fill-pointer 0) :accessor tokens)
   (start :initform 0 :accessor start)
   (current :initform 0 :accessor current)
   (line :initform 1 :accessor line)
   (column :initform 0 :accessor column))
  (:default-initargs
   :source (error "Source required.")))

(defun make-scanner (source)
  (check-type source string)
  (make-instance 'scanner :source source))

(defmethod print-object ((object scanner) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (source tokens start current line column) object
      (format stream "~s ~a ~a ~a At ~d:~d"
              source tokens start current line column))))

(defun extract-line (source line-number)
  (loop for i from 1
        for start = 0 then (1+ end)
        for end = (position #\Newline source :start start)
        while (and end (< i line-number))
        finally (return (when (= i line-number)
                          (subseq source start end)))))

(define-condition scanner-error (error)
  ((scanner :initarg :scanner :reader scanner)
   (message :initarg :message :reader message))
  (:report report-scanner-error))

(defun report-scanner-error (condition stream)
  (with-slots (source line column) (scanner condition)
    (format stream "~a~%At line ~d, column ~d:~%" (message condition) line column)
    (format stream "~a~%" (extract-line source line))
    (format stream "~v@a~%" column #\^)))

(defun scanner-error (scanner message)
  (error 'scanner-error :scanner scanner :message message))

(defun scanner-in-bounds-p (scanner)
  (array-in-bounds-p (source scanner) (current scanner)))

(defun peek-scanner (scanner)
  (char (source scanner) (current scanner)))

(defun advance-scanner (scanner)
  (prog1 (char (source scanner) (current scanner))
    (incf (current scanner))
    (incf (column scanner))))

(defun add-token (scanner token-type &optional literal)
  (with-slots (source tokens start current line column) scanner
    (let ((text (subseq source start current)))
      (vector-push-extend
       (make-token token-type text line column literal)
       tokens))))

(defun scan-tokens (scanner)
  (with-slots (tokens start current) scanner
    (if (zerop (length tokens))
        (loop while (scanner-in-bounds-p scanner)
              do (scan-token scanner)
                 (setf start current)
              finally (add-token scanner :eof)
                      (return (adjust-array tokens (length tokens))))
        tokens)))

(defun scan-token (scanner &aux (char (advance-scanner scanner)))
  (case char
    (#\λ (add-token scanner :lambda))
    (#\( (add-token scanner :left-paren))
    (#\) (add-token scanner :right-paren))
    (#\. (add-token scanner :dot))
    (#\= (add-token scanner :equal))
    (#\Newline
     (incf (line scanner))
     (setf (column scanner) 0))
    ((#\Space #\Tab #\Return))
    (t (if (or (char= char #\_) (actual-lower-case-p char))
           (scan-identifier scanner char)
           (scanner-error
            scanner (format nil "Unexpected character '~a'."
                            (char (source scanner) (1- (current scanner)))))))))

(defun scan-identifier (scanner initial-char)
  (loop with literal = (make-array 1 :adjustable t :fill-pointer t
                                     :element-type 'character
                                     :initial-contents (list initial-char))
        while (scanner-in-bounds-p scanner)
        for char = (peek-scanner scanner)
        while (or (char= char #\_) (actual-lower-case-p char) (digit-char-p char))
        do (vector-push-extend char literal)
           (advance-scanner scanner)
        finally (add-token scanner :identifier literal)))

;; CL:LOWER-CASE-P is too general, includes #\λ.
(defun actual-lower-case-p (char)
  (<= #.(char-code #\a) (char-code char) #.(char-code #\z)))

(defclass parser ()
  ((tokens :initarg :tokens :reader tokens)
   (current :initform 0 :accessor current)
   (source :initarg :source :reader source))
  (:default-initargs
   :tokens (error "Tokens required.")
   :source (error "Source required.")))

(defun make-parser (tokens source)
  (make-instance 'parser :tokens tokens :source source))

(defmethod print-object ((object parser) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (tokens current) object
      (format stream "~a ~a" tokens current))))

(define-condition parser-error (error)
  ((parser :initarg :parser :reader parser)
   (message :initarg :message :reader message))
  (:report report-parser-error))

(defun report-parser-error (condition stream)
  (let* ((parser (parser condition))
         (source (source parser))
         (token (peek-parser parser))
         (line (line token)) (column (column token)))
    (format stream "~a~%At line ~d, column ~d:~%" (message condition) line column)
    (format stream "~a~%" (extract-line source line))
    (format stream "~v@a~%" column #\^)))

(defun parser-error (parser message)
  (error 'parser-error :parser parser :message message))

(defun parser-get (parser index)
  (with-slots (tokens) parser
    (when (array-in-bounds-p tokens index)
      (aref tokens index))))

(defun parser-in-bounds-p (parser)
  (let ((maybe-token (parser-get parser (current parser))))
    (when maybe-token
      (not (eql (token-type maybe-token) :eof)))))

(defun peek-parser (parser)
  (parser-get parser (current parser)))

(defun advance-parser (parser)
  (let ((maybe-token (peek-parser parser)))
    (when maybe-token
      (prog1 maybe-token
        (incf (current parser))))))

(defun check-parser (parser &rest token-types)
  (loop for expected in token-types
        for index = (current parser) then (1+ index)
        for actual = (parser-get parser index)
        always (and actual (eql (token-type actual) expected))))

(defun match-parser (parser expected)
  (when (check-parser parser expected)
    (advance-parser parser)))

(defun consume-parser (parser token-type message)
  (if (check-parser parser token-type)
      (advance-parser parser)
      (parser-error parser message)))

(defun parse-lambda-variable (parser message)
  (make-lambda-variable (literal (consume-parser parser :identifier message))))

(defun parse-lambda-abstraction (parser)
  (let ((variables
          (loop
            collect (parse-lambda-variable parser "Expect identifier after lambda.")
            until (match-parser parser :dot)))
        (body (parse-lambda parser)))
    (reduce #'make-lambda-abstraction
            variables
            :initial-value body
            :from-end t)))

(defun parse-lambda-atom (parser)
  (cond
    ((match-parser parser :lambda)
     (parse-lambda-abstraction parser))
    ((match-parser parser :left-paren)
     (prog1 (parse-lambda parser)
       (consume-parser parser :right-paren "Expect ')' after parenthesized expression.")))
    ((check-parser parser :identifier)
     (parse-lambda-variable parser "Expect lambda variable."))
    (t
     (parser-error parser "Expect either an abstraction, an application or a variable."))))

(defun parse-lambda (parser)
  (loop with term = (parse-lambda-atom parser)
        until (or (check-parser parser :eof) (check-parser parser :right-paren))
        for next = (parse-lambda-atom parser)
        do (setf term (make-lambda-application term next))
        finally (return term)))

(defun parse-lambda-term (parser)
  (prog1 (parse-lambda parser)
    (consume-parser parser :eof "Leftover input at end.")))

(defun parse-lambda-binding (parser)
  (let ((name (prog1 (consume-parser parser :identifier "Expect binding name.")
                (consume-parser parser :equal "Expect '=' after binding name."))))
    (make-lambda-binding (literal name) (parse-lambda-term parser))))

(defun parse-lambda-toplevel (source)
  (let* ((scanner (make-scanner source))
         (parser (make-parser (scan-tokens scanner) source)))
    (if (check-parser parser :identifier :equal)
        (parse-lambda-binding parser)
        (parse-lambda-term parser))))

(defclass lambda-binding ()
  ((name :initarg :name :reader name)
   (term :initarg :term :accessor term))
  (:default-initargs
   :name (error "Name required.")
   :term (error "Term required.")))

(defun make-lambda-binding (name term)
  (make-instance 'lambda-binding :name name :term term))

(defmethod print-object ((object lambda-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name term) object
      (format stream "~a " name)
      (print-term term stream))))

(defvar *lambda-bindings* nil)
