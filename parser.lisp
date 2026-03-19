(in-package #:ski)

(defun extract-line (source line-number)
  (loop for i from 1
        for start = 0 then (1+ end)
        for end = (position #\Newline source :start start)
        while (and end (< i line-number))
        finally (return (when (= i line-number)
                          (subseq source start end)))))

(defun latin-lower-case-p (char)
  (<= #.(char-code #\a) (char-code char) #.(char-code #\z)))

(defclass token ()
  ((token-type :initarg :token-type :reader token-type)
   (lexeme :initarg :lexeme :reader lexeme)
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

(defgeneric peek (object))
(defgeneric advance (object))

(defgeneric scan-token (scanner))

(defclass scanner ()
  ((source :initarg :source :reader source)
   (tokens :initform (make-array 0 :adjustable t :fill-pointer t) :reader tokens)
   (start :initform 0 :accessor start)
   (current :initform 0 :accessor current)
   (line :initform 1 :accessor line)
   (column :initform 0 :accessor column))
  (:default-initargs
   :source (error "Source required.")))

(defmethod print-object ((object scanner) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (source tokens start current line column) object
      (format stream "~s ~a ~a ~a At ~d:~d"
              source tokens start current line column))))

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

(defmethod peek ((object scanner))
  (char (source object) (current object)))

(defmethod advance ((object scanner))
  (prog1 (char (source object) (current object))
    (incf (current object))
    (incf (column object))))

(defun add-token (scanner token-type &optional literal)
  (with-slots (source tokens start current line column) scanner
    (let ((text (subseq source start current)))
      (vector-push-extend
       (make-token token-type text line column literal)
       tokens))))

(defun scan-tokens (scanner)
  (with-slots (tokens start current column) scanner
    (if (zerop (length tokens))
        (loop while (scanner-in-bounds-p scanner)
              do (scan-token scanner)
                 (setf start current)
              finally (add-token scanner :eof)
                      (return tokens))
        tokens)))

(defun skip-comment (scanner)
  (loop until (or (not (scanner-in-bounds-p scanner))
                  (char= (peek scanner) #\Newline))
        do (advance scanner)))

;;;; Lambda calculus grammar.
;;;;
;;;; digit = "0" ... "9" ;
;;;; lower-alpha = "a" ... "z" ;
;;;; ident = ( lower-alpha | "_" ) ( lower-alpha | "_" | digit )* ;
;;;; abs = ( "λ" | "/" ) ident+ "." lam ;
;;;; atom = ident | abs | "(" lam ")" ;
;;;; lam = atom ( " "+ lam )* ;
;;;; binding = ident "=" lam ;
;;;; toplevel = binding | lam ;
;;;; file = ( toplevel ";" )* ;

(defclass lambda-scanner (scanner) ())

(defun make-lambda-scanner (source)
  (make-instance 'lambda-scanner :source source))

(defmethod scan-token ((scanner lambda-scanner))
  (let ((char (advance scanner)))
    (case char
      (#\# (skip-comment scanner))
      ((#\/ #\λ) (add-token scanner :lambda))
      (#\( (add-token scanner :left-paren))
      (#\) (add-token scanner :right-paren))
      (#\. (add-token scanner :dot))
      (#\; (add-token scanner :semicolon))
      (#\= (add-token scanner :equal))
      (#\Newline
       (incf (line scanner))
       (setf (column scanner) 0))
      ((#\Space #\Tab #\Return))
      (t (if (or (char= char #\_) (latin-lower-case-p char))
             (scan-identifier scanner char)
             (scanner-error
              scanner (format nil "Unexpected character '~a'." char)))))))

(defun scan-identifier (scanner initial-char)
  (loop with literal = (make-array 1 :adjustable t :fill-pointer t
                                     :element-type 'character
                                     :initial-contents (list initial-char))
        while (scanner-in-bounds-p scanner)
        for char = (peek scanner)
        while (or (char= char #\_) (latin-lower-case-p char) (digit-char-p char))
        do (vector-push-extend char literal)
           (advance scanner)
        finally (add-token scanner :identifier literal)))

(defclass parser ()
  ((tokens :initarg :tokens :reader tokens)
   (current :initform 0 :accessor current)
   (source :initarg :source :reader source))
  (:default-initargs
   :tokens (error "Tokens required.")
   :source (error "Source required.")))

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
         (token (peek parser))
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

(defmethod peek ((object parser))
  (parser-get object (current object)))

(defmethod advance ((object parser))
  (let ((maybe-token (peek object)))
    (when maybe-token
      (prog1 maybe-token
        (incf (current object))))))

(defun parser-check (parser &rest token-types)
  (loop for expected in token-types
        for index = (current parser) then (1+ index)
        for actual = (parser-get parser index)
        always (and actual (eql (token-type actual) expected))))

(defun parser-match (parser expected)
  (when (parser-check parser expected)
    (advance parser)))

(defun parser-consume (parser token-type message)
  (if (parser-check parser token-type)
      (advance parser)
      (parser-error parser message)))

(defclass lambda-parser (parser) ())

(defun make-lambda-parser (tokens source)
  (make-instance 'lambda-parser :tokens tokens :source source))

(defun parse-lambda-abstraction (parser)
  (let ((variables
          (loop
            collect (make-lambda-variable
                     (literal
                      (parser-consume
                       parser :identifier "Expect identifier after lambda.")))
            until (parser-match parser :dot)))
        (body (parse-lambda-tree parser)))
    (reduce #'make-lambda-abstraction
            variables
            :initial-value body
            :from-end t)))

(defun parse-lambda-atom (parser)
  (cond
    ((parser-match parser :lambda)
     (parse-lambda-abstraction parser))
    ((parser-match parser :left-paren)
     (prog1 (parse-lambda-tree parser)
       (parser-consume parser :right-paren "Expect ')' after parenthesized expression.")))
    ((parser-check parser :identifier)
     (make-lambda-variable (literal (advance parser))))
    (t
     (parser-error parser "Expect either an abstraction, an application or a variable."))))

(defun parse-lambda-tree (parser)
  (loop with term = (parse-lambda-atom parser)
        until (or (parser-check parser :eof)
                  (parser-check parser :right-paren)
                  (parser-check parser :semicolon))
        for next = (parse-lambda-atom parser)
        do (setf term (make-lambda-application term next))
        finally (return term)))

(defun parse-lambda-binding (parser)
  (let ((name (prog1 (advance parser) (advance parser))))
    (make-lambda-binding (literal name) (parse-lambda-tree parser))))

(defun parse-lambda-toplevel (parser)
  (if (parser-check parser :identifier :equal)
      (parse-lambda-binding parser)
      (parse-lambda-tree parser)))

(defun parse-lambda-file (parser)
  (loop with toplevels = (make-array 0 :adjustable t :fill-pointer t)
        until (parser-check parser :eof)
        do (vector-push-extend
            (prog1 (parse-lambda-toplevel parser)
              (parser-consume parser :semicolon "Expect ';' after toplevel item."))
            toplevels)
        finally (return toplevels)))

(defun parse-lambda-term (source)
  (let* ((scanner (make-lambda-scanner source))
         (tokens (scan-tokens scanner))
         (parser (make-lambda-parser tokens source)))
    (prog1 (parse-lambda-toplevel parser)
      (parser-consume parser :eof "Leftover input at the end."))))

(defclass lambda-binding ()
  ((name :initarg :name :reader name)
   (term :initarg :term :reader term))
  (:default-initargs
   :name (error "Name required.")
   :term (error "Term required.")))

(defun make-lambda-binding (name term)
  (make-instance 'lambda-binding :name name :term term))

(defun lambda-binding-p (object)
  (typep object 'lambda-binding))

(defmethod print-object ((object lambda-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name term) object
      (format stream "~a ~a" name term))))

(defun run-lambda-program (file &optional (stream *standard-output*))
  (let* ((source (uiop:read-file-string file))
         (scanner (make-lambda-scanner source))
         (tokens (scan-tokens scanner))
         (parser (make-lambda-parser tokens source))
         (toplevels (parse-lambda-file parser))
         (terms (remove-if #'lambda-binding-p toplevels))
         (bindings (remove-if-not #'lambda-binding-p toplevels)))
    (loop for i from (1- (length bindings)) downto 0
          for binding = (aref bindings i)
          for var = (make-lambda-variable (name binding))
          for sub = (term binding)
          do (map-into terms (lambda (term) (substitute-avoiding-capture term var sub)) terms))
    (loop with last
          for term across terms
          do (setf last (reduce-term term))
             (fresh-line stream)
             (print-term last stream)
          finally (return last))))

;;;; Combinatory logic grammar.
;;;;
;;;; digit = "0" ... "9" ;
;;;; lower-alpha = "a" ... "z" ;
;;;; ident = ( lower-alpha | "_" ) ( lower-alpha | "_" | digit )* ;
;;;; combinator = any string that's the name of some symbol in *combinators*
;;;; atom = ident | combinator | "(" comb ")" ;
;;;; comb = atom ( " "+ comb )* ;
;;;; binding = ident+ "=" comb ;
;;;; toplevel = binding | comb ;
;;;; file = ( toplevel ";" )* ;

(defclass combinator-scanner (scanner) ())

(defun make-combinator-scanner (source)
  (make-instance 'combinator-scanner :source source))

(defmethod scan-token ((scanner combinator-scanner))
  (let ((char (advance scanner)))
    (case char
      (#\# (skip-comment scanner))
      (#\] (add-token scanner :left-bracket))
      (#\[ (add-token scanner :right-bracket))
      (#\( (add-token scanner :left-paren))
      (#\) (add-token scanner :right-paren))
      (#\; (add-token scanner :semicolon))
      (#\= (add-token scanner :equal))
      (#\Newline
       (incf (line scanner))
       (setf (column scanner) 0))
      ((#\Space #\Tab #\Return))
      (t (cond
           ((or (char= char #\_) (latin-lower-case-p char))
            (scan-identifier scanner char))
           ((scan-combinator scanner))
           (t
            (scanner-error
             scanner (format nil "Unexpected character '~a'." char))))))))

(defun scan-combinator (scanner)
  (with-slots (source current) scanner
    (loop with source-length = (length source)
          with matched-length = 0
          with matched-comb
          initially (decf current)
          for symb being the hash-keys of *combinators* using (hash-value comb)
          for name = (symbol-name symb)
          for length = (length name)
          when (and (> length matched-length)
                    (>= (- source-length current) length)
                    (string= source name :start1 current :end1 (+ current length)))
            do (setf matched-length length
                     matched-comb comb)
          finally (return (when matched-comb
                            (incf current matched-length)
                            (incf (column scanner) (1- matched-length))
                            (add-token scanner :combinator matched-comb))))))

(defclass combinator-parser (parser) ())

(defun make-combinator-parser (tokens source)
  (make-instance 'combinator-parser :tokens tokens :source source))

(defun parse-combinator-atom (parser)
  (cond
    ((parser-check parser :combinator)
     (literal (advance parser)))
    ((parser-match parser :left-paren)
     (prog1 (parse-combinator-tree parser)
       (parser-consume parser :right-paren "Expect ')' after parenthesized expression.")))
    ((parser-check parser :identifier)
     (make-combinator-variable (literal (advance parser))))
    (t
     (parser-error parser "Expect either a combinator, an application or a variable."))))

(defun parse-combinator-tree (parser)
  (loop with term = (parse-combinator-atom parser)
        until (or (parser-check parser :eof)
                  (parser-check parser :right-paren)
                  (parser-check parser :semicolon))
        for next = (parse-combinator-atom parser)
        do (setf term (make-combinator-application term next))
        finally (return term)))

(defun parse-combinator-binding (parser)
  (let ((name (parser-consume parser :identifier "Expect combinator binding name."))
        (vars (loop until (parser-match parser :equal)
                    for name = (parser-consume parser :identifier "Expect variable name.")
                    collect (intern (string-upcase (literal name)))))
        (body (parse-combinator-tree parser)))
    (make-combinator-binding
     (literal name)
     (length vars)
     (compile nil (expand-operation vars body)))))

(defun parse-combinator-toplevel (parser)
  (with-slots (tokens current) parser
    (let ((end (position :semicolon tokens :start current :key #'token-type)))
      (if (find :equal tokens :start current :end end :key #'token-type)
          (parse-combinator-binding parser)
          (parse-combinator-tree parser)))))

(defun parse-combinator-file (parser)
  (loop with toplevels = (make-array 0 :adjustable t :fill-pointer t)
        until (parser-check parser :eof)
        do (vector-push-extend
            (prog1 (parse-combinator-toplevel parser)
              (parser-consume parser :semicolon "Expect ';' after toplevel item."))
            toplevels)
        finally (return toplevels)))

(defun parse-combinator-term (source)
  (let* ((scanner (make-combinator-scanner source))
         (tokens (scan-tokens scanner))
         (parser (make-combinator-parser tokens source)))
    (prog1 (parse-combinator-toplevel parser)
      (parser-consume parser :eof "Leftover input at the end."))))

(defclass combinator-binding ()
  ((name :initarg :name :reader name)
   (arity :initarg :arity :reader arity)
   (operation :initarg :operation :reader operation))
  (:default-initargs
   :name (error "Name required.")
   :arity (error "Arity required.")
   :operation (error "Operation required.")))

(defun make-combinator-binding (name arity operation)
  (make-instance 'combinator-binding
                 :name name
                 :arity arity
                 :operation operation))

(defun combinator-binding-p (object)
  (typep object 'combinator-binding))

(defmethod print-object ((object combinator-binding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name arity) object
      (format stream "~a/~d" name arity))))

(defun expand-operation (variables expr)
  "Compute the lambda form that implements the operation of a combinator
binding."
  (labels ((operation-form (term)
             (etypecase term
               (combinator-variable
                (or (find (variable->symbol term) variables) term))
               (combinator-application
                `(make-combinator-application
                  ,(operation-form (left term))
                  ,(operation-form (right term))))
               (combinator-term
                term))))
    (let ((arity (length variables))
          (stack-variable (gensym)))
      `(lambda (,stack-variable)
         (when (>= (length ,stack-variable) ,arity)
           (let ,(expand-bindings-for-stack-access variables stack-variable)
             (declare (ignorable ,@variables))
             (values ,(operation-form expr)
                     (nthcdr ,arity ,stack-variable))))))))

(defvar *combinator-bindings*)

(defmethod step-combinator-term :around ((term combinator-variable) (stack list))
  (if (boundp '*combinator-bindings*)
      (multiple-value-bind (binding found-p) (gethash (name term) *combinator-bindings*)
        (if found-p (funcall (operation binding) stack) (call-next-method)))
      (call-next-method)))

(defun run-combinator-program (file &optional (stream *standard-output*))
  (let* ((source (uiop:read-file-string file))
         (scanner (make-combinator-scanner source))
         (tokens (scan-tokens scanner))
         (parser (make-combinator-parser tokens source))
         (toplevels (parse-combinator-file parser))
         (terms (remove-if #'combinator-binding-p toplevels))
         (*combinator-bindings*
           (loop with table = (make-hash-table :test #'equal)
                 for binding across (remove-if-not #'combinator-binding-p toplevels)
                 do (setf (gethash (name binding) table) binding)
                 finally (return table))))
    (loop with last
          for term across terms
          do (setf last (reduce-term term))
             (fresh-line stream)
             (print-term last stream)
          finally (return last))))
