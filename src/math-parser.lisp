;;; file MATH-PARSER:src/math-parser.lisp -- main file about parser
;;;
;;; Code:

(in-package :math-parser)

(defstruct token
  type ;; :keyword :number :variable :property :comparison :domain
  value)

;;;================================================================================
;;;                                    GRAMMAR
;;;================================================================================
;;;
;;; The grammar describes the English/Greek phrases accepted by the parser.
;;;
;;; Notation:
;;;   :=  means "is defined as"
;;;   |   means "or"
;;;   { } means a placeholder for another grammar element
;;;   [ ] means an optional part
;;;
;;; SET-EXPRESSION
;;;   := THE SET OF {DOMAIN} NUMBERS {VARIABLE} [ARE {CONDITION}]
;;;
;;; DOMAIN
;;;   := REAL
;;;    | INTEGER
;;;    | NATURAL
;;;    | RATIONAL
;;;
;;; CONDITION
;;;   := PROPERTY
;;;    | COMPARISON
;;;    | CONDITION AND CONDITION
;;;    | CONDITION OR CONDITION
;;;
;;; PROPERTY
;;;   := EVEN
;;;    | ODD
;;;
;;; COMPARISON
;;;   := GREATER FROM {NUMBER}
;;;    | GREATER OR EQUAL FROM {NUMBER}
;;;    | EQUAL TO {NUMBER}
;;;    | LESS FROM {NUMBER}
;;;    | LESS OR EQUAL FROM {NUMBER}
;;;
;;; The condition is optional. Therefore both of the following are valid:
;;;
;;;   "the set of integer numbers x"
;;;   "the set of integer numbers x are greater from 10"
;;;
;;; When the condition is omitted, the SET-EXPRESSION has CONDITION = NIL.

(defparameter *multi-word-tokens*
  '(;; English
    (("GREATER" "OR" "EQUAL") . "GREATER-OR-EQUAL")
    (("LESS"    "OR" "EQUAL") . "LESS-OR-EQUAL")
    (("WHICH" "ARE") . "ARE")
    (("THE" "SET" "OF") . "THE-SET-OF")
   

    ;; Greek
    (("ΠΟΥ" "ΕΙΝΑΙ") . "ΕΙΝΑΙ")
    (("ΑΠΟ" "ΤΟ") . "ΑΠΟ")
    (("ΜΕΓΑΛΥΤΕΡΟΙ" "Η" "ΙΣΟΙ") . "GREATER-OR-EQUAL")
    (("ΜΙΚΡΟΤΕΡΟΙ"  "Η" "ΙΣΟΙ") . "LESS-OR-EQUAL")
    (("ΤΟ" "ΣΥΝΟΛΟ" "ΤΩΝ") . "THE-SET-OF")
   ))
;;;================================================================================
;;;                              GRAMMAR STRUCTS
;;;================================================================================

(defstruct set-expression
  "AST node representing a complete mathematical set expression.

DOMAIN is the mathematical domain of the set, for example :INTEGER or :REAL.
VARIABLE is the variable used to describe the members of the set.
CONDITION is an optional condition restricting the members of the set.
It is NIL when no condition is specified."
  domain
  variable
  condition)

(defstruct and-expression
  "AST node representing the logical conjunction of two conditions.

LEFT and RIGHT are condition AST nodes.
The resulting condition is true when both LEFT and RIGHT are true."
  left
  right)

(defstruct or-expression
  "AST node representing the logical disjunction of two conditions.

LEFT and RIGHT are condition AST nodes.
The resulting condition is true when either LEFT or RIGHT is true."
  left
  right)

(defstruct property-expression
  "AST node representing a mathematical property of the set variable.

PROPERTY identifies the property, for example :EVEN or :ODD."
  property)

(defstruct comparison-expression
  "AST node representing a comparison between the set variable and a number.

OPERATOR identifies the comparison, for example :GREATER or :LESS.
VALUE is the number used in the comparison."
  operator
  value)

;;;PARSERS
(defstruct parser
  tokens
  position)

(defun current-token (parser)
   "Return the token at the parser's current position.

Returns NIL when the parser has reached the end of the token list."
  (nth (parser-position parser)
       (parser-tokens parser)))

(defun advance-parser (parser)
   "Advance PARSER to the next token."
  (incf (parser-position parser)))

(defun expect-token (parser type)
  "Consume and return the current token if it has TYPE.

Signals an error when the current token is missing or has a
different type."
  (let ((token (current-token parser)))
    (unless (and token
                 (eq (token-type token) type))
      (error "Expected ~S, got ~S"
             type
             token))
    (advance-parser parser)
    token))

(defun expect-keyword (parser keyword)
  "Consume and return the current token if it is KEYWORD.

Signals an error when the current token is not the expected keyword."
  (let ((token (current-token parser)))
    (unless (and token
                 (eq (token-type token) :keyword)
                 (eq (token-value token) keyword))
      (error "Expected keyword ~S, got ~S"
             keyword
             token))
    (advance-parser parser)
    token))

(defun parse-domain (parser)
  "Parse and return the domain at PARSER's current position.

The expected token must have type :DOMAIN.

Returns the domain value, such as :REAL, :INTEGER, :NATURAL,
or :RATIONAL."
  (let ((token (current-token parser)))
    (unless (and token
                 (eq (token-type token) :domain))
      (error "Expected domain, got ~S" token))
    (advance-parser parser)
    (token-value token)))

(defun parse-condition (parser)
  "Parse a condition from PARSER and return its AST representation.

A condition may be a simple property or comparison, or a compound
condition connected by AND or OR.

Returns a PROPERTY-EXPRESSION, COMPARISON-EXPRESSION, AND-EXPRESSION,
or OR-EXPRESSION."

  (let ((left (parse-simple-condition parser)))
    (cond
      ;; AND
      ((and (current-token parser)
            (eq (token-type (current-token parser)) :keyword)
            (eq (token-value (current-token parser)) :and))
       (advance-parser parser)
       (make-and-expression
        :left left
        :right (parse-condition parser)))

      ;; OR
      ((and (current-token parser)
            (eq (token-type (current-token parser)) :keyword)
            (eq (token-value (current-token parser)) :or))
       (advance-parser parser)
       (make-or-expression
        :left left
        :right (parse-condition parser)))

      ;; no AND/OR
      (t
       left))))

(defun parse-simple-condition (parser)
  "Parse one non-compound condition from PARSER.

A simple condition is either:
  - a PROPERTY, such as EVEN or ODD
  - a COMPARISON, such as GREATER FROM 10

Returns the corresponding condition AST node.

Signals an error when the current token cannot begin a condition."
  (let ((token (current-token parser)))
    (unless token
      (error "Expected condition, got end of input"))

    (case (token-type token)

      (:property
       (advance-parser parser)
       (make-property-expression
        :property (token-value token)))

      (:comparison
       (advance-parser parser)
       (expect-keyword parser :from)
       (let ((value-token (current-token parser)))
         (unless value-token
           (error "Expected value after comparison"))

         (unless (eq (token-type value-token) :number)
           (error "Expected number after comparison, got ~S"
                  value-token))

         (advance-parser parser)

         (make-comparison-expression
          :operator (token-value token)
          :value (token-value value-token))))

      (otherwise
       (error "Unexpected token in condition: ~S"
              token)))))

(defun parse-variable (parser)
  "Parse and return the variable at PARSER's current position.

The current token must have type :VARIABLE.

Returns the internal variable representation, such as :X or :Y."
  (let ((token (current-token parser)))
    (unless (and token
                 (eq (token-type token) :variable))
      (error "Expected variable, got ~S" token))
    (advance-parser parser)
    (token-value token)))

(defun parse-set (parser)
  "Parse a complete SET-EXPRESSION from PARSER.

The expected structure is:

  THE SET OF DOMAIN NUMBERS VARIABLE [ARE CONDITION]

The ARE CONDITION part is optional. When it is absent, the resulting
SET-EXPRESSION has a NIL condition.

Returns a SET-EXPRESSION AST node.

Signals an error when the input does not conform to the grammar."
  (expect-keyword parser :the-set-of)

  (let ((domain (parse-domain parser)))

    (expect-keyword parser :numbers)

    (let ((variable (parse-variable parser)))

      (let ((condition
              (when (and (current-token parser)
                         (eq (token-type (current-token parser)) :keyword)
                         (eq (token-value (current-token parser)) :are))
                (advance-parser parser)
                (parse-condition parser))))

        (make-set-expression
         :variable variable
         :domain domain
         :condition condition)))))

(defun phrase->ast (phrase &key (mwt *multi-word-tokens*))
  "Parse PHRASE and return its abstract syntax tree.

PHRASE is a natural-language description of a mathematical set.
MWT specifies the multi-word-token table used by the lexer.

The returned AST is a SET-EXPRESSION."
  (let* ((tokens (lexer phrase :mwt mwt))
	 (parser (make-parser :tokens tokens :position 0)))
    (parse-set parser)))

(defun phrase->math-notation (phrase &key (mwt *multi-word-tokens*))
  "Convert a natural-language set description into mathematical notation.

PHRASE is first converted into an AST and then into a mathematical
set-builder expression.

For example:

  \"the set of integer numbers x\"
      => \"{x ∈ ℤ}\"

  \"the set of integer numbers x are greater from 10\"
      => \"{x ∈ ℤ | x > 10}\""
  (let ((ast (phrase->ast phrase :mwt mwt)))
    (set-expression->math ast)))

(defun phrase->predicate (phrase &key (mwt *multi-word-tokens*))
  "Convert a natural-language set description into a membership predicate.

The returned function accepts one argument X and returns true when X
belongs to the described set.

The predicate always enforces the set's DOMAIN. If the optional
condition is present, the condition is also enforced."
  (let ((ast (phrase->ast phrase :mwt mwt)))
    (set-expression->predicate ast)))

(defparameter *Keywords*
  '(("THE-SET-OF" . :THE-SET-OF) 
    ("ΑΡΙΘΜΩΝ" . :NUMBERS)   ("NUMBERS" . :NUMBERS)
    ("ΕΙΝΑΙ" . :ARE)         ("ARE" . :ARE)
    ("ΑΠΟ" . :FROM)          ("FROM" . :FROM)
    ("ΚΑΙ" . :AND)           ("AND" . :AND)
    ("OR" . :OR)             ("Η" . :OR))
  
  )

(defparameter *domains*
  '(("ΠΡΑΓΜΑΤΙΚΩΝ" . :REAL) ("REAL" . :REAL)      ; R
    ("ΑΚΕΡΑΙΩΝ" . :INTEGER) ("INTEGER" . :INTEGER); Z
    ("ΦΥΣΙΚΩΝ" . :NATURAL)  ("NATURAL" . :NATURAL); N
    ("RATIONAL" . :RATIONAL)("ΡΗΤΩΝ" . :RATIONAL)); Q
  )

(defparameter *properties*
  '(("ΠΕΡΙΤΤΟΙ" . :ODD)      ("ODD" . :ODD)
    ("ΑΡΤΙΟΙ" . :EVEN)       ("EVEN" . :EVEN)
    ("ΘΕΤΙΚΟΙ" .  :POSITIVE) ("POSITIVE" . :POSITIVE)
    ("ΑΡΝΗΤΙΚΟΙ" . :NEGATIVE)("NEGATIVE" . :NEGATIVE)))

(defparameter *comparisons*
  '(("ΜΕΓΑΛΥΤΕΡΟΙ" . :GREATER)   ("GREATER" . :GREATER)
    ("ΜΙΚΡΟΤΕΡΟΙ" . :LESS)       ("LESS" . :LESS)
    ("ΙΣΟΙ" . :EQUAL)            ("EQUAL" . :EQUAL)
    
    ;;don't need greek here, because it's changed from *multi-word-tokens*
                                 ("GREATER-OR-EQUAL" . :GREATER-OR-EQUAL)))

(defparameter *variables*
  '(("Χ" . :X) ("X" . :X)
    ("Ψ" . :Y)
    ("Y" . :Y) ("Y" . :Y)
    ("Ν" . :N) ("N" . :N)
    ("Ι" . :I) ("I" . :I)
    ("Κ" . :K) ("K" . :K)
    ("Λ" . :l) ("L" . :L)))


(defun levenshtein (s1 s2)
   "Return the Levenshtein distance between strings S1 and S2.

The distance is the minimum number of single-character insertions,
deletions, or substitutions required to transform S1 into S2."
  (let* ((len1 (length s1))
	 (len2 (length s2))
	 (dist (make-array (list (1+ len1) (1+ len2))
			   :initial-element 0)))
    (dotimes (i (1+ len1))
      (setf (aref dist i 0) i))
    (dotimes (j (1+ len2))
      (setf (aref dist 0 j) j))
    (dotimes (i len1)
      (dotimes (j len2)
	(setf (aref dist (1+ i) (1+ j))
	      (min
	       (1+ (aref dist i (1+ j)))     ;; deletion
	       (1+ (aref dist (1+ i) j))     ;; insertion
	       (+ (aref dist i j)
		  (if (char= (aref s1 i) (aref s2 j)) 0 1))))))
    (aref dist len1 len2)))

(defun exact-lookup (word table)
  "Find WORD exactly in TABLE.

TABLE is an association list whose keys are strings.
Comparison is case-sensitive with respect to the already-normalized
input.

Returns the matching association or NIL when WORD is not found."
  (assoc word table :test #'string=))

(defun fuzzy-lookup (word table &key (threshold 2))
  "Find the closest entry for WORD in TABLE.

The comparison uses Levenshtein distance after normalization.
A match is returned only when its distance is at most THRESHOLD.

Returns the matching association or NIL when no sufficiently close
match exists."
  (let* ((w (string->atonic word))
	 (best nil)
	 (best-dist 999))
    (dolist (pair table)
      (let* ((kw (string->atonic (car pair)))
	     (dist (levenshtein w kw)))
	(when (< dist best-dist)
	  (setf best-dist dist
		best pair))))
    (if (<= best-dist threshold)
	best
	nil)))

(defun classify-token (word)
  "Classify WORD and return the corresponding TOKEN.

The classifier recognizes variables, keywords, domains, properties,
comparisons, and numbers.

When an exact match is not found, fuzzy matching is attempted.
Unknown words are returned as tokens with type :UNKNOWN."
  (let ((var (exact-lookup word *variables*))
	(kw (exact-lookup word *Keywords*))
	(domain (exact-lookup word *domains*))
	(property  (exact-lookup word *properties*))
	(comparison  (exact-lookup word *comparisons*)))
    (cond
      ((ignore-errors (parse-integer word))
       (make-token :type :number :value (parse-integer word)))
      (var (make-token :type :variable :value (cdr var)))
      (kw (make-token :type :keyword :value (cdr kw)))
      (domain (make-token :type :domain :value (cdr domain)))
      (property (make-token :type :property :value (cdr property)))
      (comparison (make-token :type :comparison :value (cdr comparison)))
    
      (t (let ((fuzzy
		 (or (fuzzy-lookup word *variables*)
		     (fuzzy-lookup word *Keywords*)
		     (fuzzy-lookup word *domains*)
		     (fuzzy-lookup word *properties*)
		     (fuzzy-lookup word *comparisons*))))

	   (if fuzzy
	       (make-token
		:type (cond
			((member fuzzy *variables*)   :variable)
			((member fuzzy *Keywords*)    :keyword)
			((member fuzzy *domains*)     :domain)
			((member fuzzy *properties*)  :property)
			((member fuzzy *comparisons*) :comparison))
		:value (cdr fuzzy))
	       (make-token :type :unknown :value word)))))))      




(defun lexer (phrase &key (mwt *multi-word-tokens*))
 "Convert natural-language PHRASE into a list of TOKEN structures.

The phrase is split into words, normalized, multi-word tokens are
replaced, and each resulting word is classified.

MWT specifies the multi-word-token table to use."
  (let* ((list-of-words
           (split-by phrase))
         
         (list-of-possible-tokens
           (mapcar #'string->atonic list-of-words))
         (words
           (replace-multi-word-tokens
            list-of-possible-tokens :mwt mwt)))

    (mapcar #'classify-token words)))

(defun comparison->math (comparison variable)
 "Convert a COMPARISON-EXPRESSION into mathematical notation.

COMPARISON contains the comparison operator and numeric value.
VARIABLE is the variable being compared.

For example:

  (:GREATER, 10), X
      => \"X > 10\""
  (format nil
          "~a ~a ~a"
          variable
          (case (comparison-expression-operator comparison)
            (:greater ">")
	    (:greater-or-equal "≥")
	    (:less "<")
	    (:less-or-equal "≤")
            (:equal "=")
	    )
          (comparison-expression-value comparison)))

(defun condition->math (condition variable)
"Convert a condition AST node into mathematical notation.

VARIABLE is the variable to which the condition applies.

Supports comparison, property, and AND expressions.

Signals an error when CONDITION has an unsupported type."
  (typecase condition

    (comparison-expression
     (comparison->math condition variable))

    (property-expression
     (case (property-expression-property condition)
       (:odd  (format nil "~a ≡ 1 (mod 2)" variable))
       (:even (format nil "~a ≡ 0 (mod 2)" variable))
       (otherwise
        (error "Unknown property: ~S"
               (property-expression-property condition)))))

    (and-expression
     (format nil
             "~a ∧ ~a"
             (condition->math
              (and-expression-left condition)
              variable)
             (condition->math
              (and-expression-right condition)
              variable)))

    (or-expression
     (format nil
	     "~a ∨ ~a"
	     (condition->math
	      (or-expression-left condition)
	      variable)
	     (condition->math
	      (or-expression-right condition)
	      variable)))

    (otherwise
     (error "Unknown condition: ~S" condition))))

(defun set-expression->math (set)
   "Convert a SET-EXPRESSION AST into mathematical set-builder notation.

When SET has a condition, the result has the form:

  {x ∈ DOMAIN | CONDITION}

When the condition is NIL, the result has the form:

  {x ∈ DOMAIN}

For example:

  SET with DOMAIN :INTEGER and no condition
      => \"{x ∈ ℤ}\""
  (if (set-expression-condition set)
      (format nil
              "{~a ∈ ~a | ~a}"
              (string-downcase
               (symbol-name (set-expression-variable set)))
              (case (set-expression-domain set)
                (:real "ℝ")
                (:integer "ℤ")
                (:natural "ℕ")
                (:rational "ℚ")
                (:complex "ℂ"))
              (condition->math
               (set-expression-condition set)
               (string-downcase
                (symbol-name (set-expression-variable set)))))
      (format nil
              "{~a ∈ ~a}"
              (string-downcase
               (symbol-name (set-expression-variable set)))
              (case (set-expression-domain set)
                (:real "ℝ")
                (:integer "ℤ")
                (:natural "ℕ")
                (:rational "ℚ")
                (:complex "ℂ")))))

(defun condition->predicate (condition )
  "Convert a condition AST node into a predicate function.

The returned function accepts one argument and returns a generalized
boolean indicating whether the argument satisfies CONDITION.

Supports comparison, property, and AND expressions.

CONDITION must be a valid condition AST node; NIL is not a condition
and is handled by SET-EXPRESSION->PREDICATE."
  (typecase condition

    (comparison-expression
     (let ((operator (comparison-expression-operator condition))
	   (value    (comparison-expression-value condition)))
       (case operator
	 (:greater
	  (lambda (x)
	    (> x value)))

	 (:greater-or-equal
	  (lambda (x)
	    (>= x value)))
	 
	 (:less
	  (lambda (x)
	    (< x value)))

	 (:less-or-equal
	  (lambda (x)
	    (<= x value)))

	 (:equal
	  (lambda (x)
	    (= x value)))

	 (otherwise
	  (error "Unknown comparison operator: ~S"
		 operator)))))

    (property-expression
     (case (property-expression-property condition)
       (:odd
	(lambda (x)
	  (and (integerp x)
	       (= (mod x 2) 1))))

       (:even
	(lambda (x)
	  (and (integerp x)
	       (= (mod x 2) 0))))
       
       (otherwise
	(error "Unknown property: ~S"
	       (property-expression-property condition)))))
    
    (and-expression
     (let ((left  (condition->predicate
		   (and-expression-left condition)
		   ))
	   (right (condition->predicate
		   (and-expression-right condition)
		   )))
       (lambda (x)
	 (and (funcall left x)
	      (funcall right x)))))

    (or-expression
     (let ((left  (condition->predicate
		   (or-expression-left condition)))
	   (right (condition->predicate
		   (or-expression-right condition))))
       (lambda (x)
	 (or (funcall left x)
	     (funcall right x)))))
    
    (otherwise
     (error "Unknown condition: ~S" condition))))

(defun set-expression->predicate (expression)
  "Convert a SET-EXPRESSION into a membership predicate function.

The returned function accepts a value X and returns true when:

  1. X belongs to the set's DOMAIN, and
  2. X satisfies the set's CONDITION, when one is present.

If the SET-EXPRESSION has no condition, only the domain restriction
is tested.

For example, the expression

  {x ∈ ℤ}

produces a predicate equivalent to:

  (lambda (x)
    (integerp x))"
  (let ((condition
          (and (set-expression-condition expression)
               (condition->predicate
                (set-expression-condition expression)))))
    (lambda (x)
      (and
       (case (set-expression-domain expression)
         (:integer (integerp x))
         (:real    (realp x))
         (:natural (and (integerp x) (>= x 0)))
         (:rational (rationalp x))
         (:complex (complexp x)))
       (or (null condition)
           (funcall condition x))))))

;;; File MATH-PARSER:src/math-parser.lisp ends here
