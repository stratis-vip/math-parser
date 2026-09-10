;;; file MATH-PARSER:tests/parser-tests.lisp -- tests about parser
;;;
;;; Code:

(in-package :math-parser/tests)


;;;================================================================================
;;;                              TEST SETUP
;;;================================================================================

(defsuite parser-tests)
(in-suite parser-tests)


(defun create-phrase-stages (phrase)
  "Create the intermediate stages of parsing PHRASE.

Returns four values:

  1. the list of TOKENS produced by the lexer,
  2. the PARSER,
  3. the resulting AST,
  4. the original PHRASE.

The parser has already parsed the phrase when these values are returned,
so its position is normally at the end of the token list."
  (let* ((tokens (math-parser::lexer phrase :mwt math-parser::*multi-word-tokens*))
         (parser (math-parser::make-parser :tokens tokens :position 0))
         (ast (math-parser::parse-set parser)))
    (values tokens parser ast phrase)))


;;;================================================================================
;;;                              PARSER PRIMITIVES
;;;================================================================================

(test current-token
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore ast phrase))

    ;; After parse-set, parser is at the end.
    (check (= (length tokens)
              (math-parser::parser-position parser)))

    ;; Reset parser to the beginning.
    (setf (math-parser::parser-position parser) 0)

    ;; Position 0 contains THE-SET-OF.
    (check (= 0 (math-parser::parser-position parser)))
    (check (eq :keyword
               (math-parser::token-type
                (math-parser::current-token parser))))
    (check (eq :the-set-of
               (math-parser::token-value
                (math-parser::current-token parser))))

    ;; Advance to the domain.
    (math-parser::advance-parser parser)

    (check (= 1 (math-parser::parser-position parser)))
    (check (eq :domain
               (math-parser::token-type
                (math-parser::current-token parser))))
    (check (eq :integer
               (math-parser::token-value
                (math-parser::current-token parser))))

    ;; Move to the end of the token list.
    (setf (math-parser::parser-position parser)
          (length tokens))

    ;; current-token returns NIL at end of input.
    (check-not (math-parser::current-token parser))))


(test advance-parser
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore tokens ast phrase))

    (setf (math-parser::parser-position parser) 0)

    (check (= 0 (math-parser::parser-position parser)))

    (math-parser::advance-parser parser)

    (check (= 1 (math-parser::parser-position parser)))

    (math-parser::advance-parser parser)

    (check (= 2 (math-parser::parser-position parser)))))


(test expect-token
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore tokens ast phrase))

    (setf (math-parser::parser-position parser) 0)

    ;; Correct token type is accepted.
    (let ((token (math-parser::expect-token parser :keyword)))
      (check (eq :keyword
                 (math-parser::token-type token)))
      (check (eq :the-set-of
                 (math-parser::token-value token))))

    ;; expect-token consumes the token.
    (check (= 1 (math-parser::parser-position parser)))

    ;; Wrong token type raises an error.
    (raise-error
      (math-parser::expect-token parser :number)
      simple-error)))


(test expect-keyword
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore tokens ast phrase))

    (setf (math-parser::parser-position parser) 0)

    ;; Correct keyword is accepted.
    (let ((token (math-parser::expect-keyword parser :the-set-of)))
      (check (eq :keyword
                 (math-parser::token-type token)))
      (check (eq :the-set-of
                 (math-parser::token-value token))))

    ;; expect-keyword consumes the token.
    (check (= 1 (math-parser::parser-position parser)))

    ;; Wrong keyword raises an error.
    (raise-error
      (math-parser::expect-keyword parser :from)
      simple-error))


(test parser-end-of-input
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore ast phrase))

    ;; Put parser at the end.
    (setf (math-parser::parser-position parser)
          (length tokens))

    (check-not (math-parser::current-token parser))

    ;; Both expect functions must reject end-of-input.
    (raise-error
	(math-parser::expect-token parser :keyword)
	simple-error)
    
     (raise-error
      (math-parser::expect-keyword parser :the-set-of)
      simple-error)))


;;;================================================================================
;;;                              DOMAIN
;;;================================================================================

(test parse-domain
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore tokens ast phrase))

    (setf (math-parser::parser-position parser) 1)

    (check (eq :integer
               (math-parser::parse-domain parser)))

    (check (= 2
              (math-parser::parser-position parser)))))


;;;================================================================================
;;;                              VARIABLE
;;;================================================================================

(test parse-variable
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages "the set of integer numbers x")
    (declare (ignore tokens ast phrase))

    ;; X is token number 3.
    (setf (math-parser::parser-position parser) 3)

    (check (eq :x
               (math-parser::parse-variable parser)))

    (check (= 4
              (math-parser::parser-position parser)))))


;;;================================================================================
;;;                              GRAMMAR
;;;================================================================================

(test grammar-no-condition
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages
       "the set of integer numbers x")
    (declare (ignore parser))

    ;; Six tokens:
    ;;
    ;; THE-SET-OF INTEGER NUMBERS X
    ;; Depending on the lexer, the actual token count should be checked
    ;; against the produced token list.
    (check (= 4 (length tokens)))

    ;; Domain.
    (check (eq :integer
               (math-parser::set-expression-domain ast)))

    ;; Variable.
    (check (eq :x
               (math-parser::set-expression-variable ast)))

    ;; No condition.
    (check-not
     (math-parser::set-expression-condition ast))

    ;; Mathematical notation.
    (check
     (string=
      "{x ∈ ℤ}"
      (math-parser::set-expression->math ast)))

    ;; Public conversion gives the same result.
    (check
     (string=
      (math-parser::set-expression->math ast)
      (phrase->math-notation phrase)))

    ;; Membership predicate.
    (check
     (funcall
      (math-parser::set-expression->predicate ast)
      -2345))

    ;; A real number does not belong to INTEGER.
    (check-not
     (funcall
      (math-parser::set-expression->predicate ast)
      2.5))))


(test grammar-comparison
  (multiple-value-bind (tokens parser ast phrase)
      (create-phrase-stages
       "the set of real numbers x are greater or equal from 0")
    (declare (ignore parser tokens))

    ;; Domain.
    (check (eq :real
               (math-parser::set-expression-domain ast)))

    ;; Variable.
    (check (eq :x
               (math-parser::set-expression-variable ast)))

    ;; Condition exists.
    (check
     (typep
      (math-parser::set-expression-condition ast)
      'math-parser::comparison-expression))

    ;; Mathematical notation.
    (check
     (string=
      "{x ∈ ℝ | x ≥ 0}"
      (math-parser::set-expression->math ast)))

    ;; Public conversion.
    (check
     (string=
      (math-parser::set-expression->math ast)
      (phrase->math-notation phrase)))

    ;; Predicate.
    (check
     (funcall
      (math-parser::set-expression->predicate ast)
      0))

    ;; 5 satisfies x >= 0.
    (check
     (funcall
      (math-parser::set-expression->predicate ast)
      5))

    ;; -1 does not satisfy x >= 0.
    (check-not
     (funcall
      (math-parser::set-expression->predicate ast)
      -1))))


;;;================================================================================
;;;                              PROPERTY CONDITIONS
;;;================================================================================

(test grammar-property
  ;; EVEN
  (check
   (string=
    "{x ∈ ℤ | x ≡ 0 (mod 2)}"
    (phrase->math-notation
     "the set of integer numbers x are even")))

  (check
   (funcall
    (phrase->predicate
     "the set of integer numbers x are even")
    20))

  (check-not
   (funcall
    (phrase->predicate
     "the set of integer numbers x are even")
    21))

  ;; ODD
  (check
   (string=
    "{x ∈ ℤ | x ≡ 1 (mod 2)}"
    (phrase->math-notation
     "the set of integer numbers x are odd")))

  (check
   (funcall
    (phrase->predicate
     "the set of integer numbers x are odd")
    21))

  (check-not
   (funcall
    (phrase->predicate
     "the set of integer numbers x are odd")
    20)))


;;;================================================================================
;;;                              AND CONDITIONS
;;;================================================================================

(test grammar-and
  (let ((phrase
          "the set of natural numbers x are greater or equal from 0 and less from 100 and even"))

    ;; Mathematical notation.
    (check
     (string=
      "{x ∈ ℕ | x ≥ 0 ∧ x < 100 ∧ x ≡ 0 (mod 2)}"
      (phrase->math-notation phrase)))

    ;; 20 satisfies all three conditions.
    (check
     (funcall
      (phrase->predicate phrase)
      20))

    ;; 101 fails x < 100.
    (check-not
     (funcall
      (phrase->predicate phrase)
      101))

    ;; 21 fails EVEN.
    (check-not
     (funcall
      (phrase->predicate phrase)
      21))

    ;; Negative values fail the NATURAL domain.
    (check-not
     (funcall
      (phrase->predicate phrase)
      -2)))))


;;;================================================================================
;;;                              OR CONDITIONS
;;;================================================================================

(test grammar-or
  (let ((phrase
          "the set of integer numbers x are greater from 10 or less from 0"))

    ;; Mathematical notation.
    (check
     (string=
      "{x ∈ ℤ | x > 10 ∨ x < 0}"
      (phrase->math-notation phrase)))

    ;; 20 satisfies x > 10.
    (check
     (funcall
      (phrase->predicate phrase)
      20))

    ;; -5 satisfies x < 0.
    (check
     (funcall
      (phrase->predicate phrase)
      -5))

    ;; 5 satisfies neither condition.
    (check-not
     (funcall
      (phrase->predicate phrase)
      5))

    ;; 10 satisfies neither x > 10 nor x < 0.
    (check-not
     (funcall
      (phrase->predicate phrase)
      10))

    ;; 11 satisfies x > 10.
    (check
     (funcall
      (phrase->predicate phrase)
      11))))


;;;================================================================================
;;;                              WHICH ARE
;;;================================================================================

(test grammar-which-are
  (let ((phrase
          "the set of integer numbers x which are greater or equal from 0"))

    ;; WHICH ARE should be normalized to ARE by the lexer.
    (check
     (string=
      "{x ∈ ℤ | x ≥ 0}"
      (phrase->math-notation phrase)))

    ;; Predicate.
    (check
     (funcall
      (phrase->predicate phrase)
      10))

    (check-not
     (funcall
      (phrase->predicate phrase)
      -1))))


;;;================================================================================
;;;                         OPTIONAL CONDITION
;;;================================================================================

(test optional-condition
  ;; No ARE clause.
  (let ((ast
          (phrase->ast
           "the set of integer numbers x")))

    (check-not
     (math-parser::set-expression-condition ast))

    (check
     (string=
      "{x ∈ ℤ}"
      (phrase->math-notation
       "the set of integer numbers x")))

    ;; Any integer belongs to the set.
    (check
     (funcall
      (phrase->predicate
       "the set of integer numbers x")
      -2345))

    ;; Non-integer does not.
    (check-not
     (funcall
      (phrase->predicate
       "the set of integer numbers x")
      2.5))))


;;;================================================================================
;;;                         PUBLIC INTERFACE
;;;================================================================================

(test phrase-to-ast
  (let ((ast
          (phrase->ast
           "the set of integer numbers x are greater from 10")))

    (check (typep ast 'math-parser::set-expression))

    (check (eq :integer
               (math-parser::set-expression-domain ast)))

    (check (eq :x
               (math-parser::set-expression-variable ast)))

    (check
     (typep
      (math-parser::set-expression-condition ast)
      'math-parser::comparison-expression))))


(test phrase-to-math-notation
  (check
   (string=
    "{x ∈ ℤ}"
    (phrase->math-notation
     "the set of integer numbers x")))

  (check
   (string=
    "{x ∈ ℤ | x > 10}"
    (phrase->math-notation
     "the set of integer numbers x are greater from 10")))

  (check
   (string=
    "{x ∈ ℤ | x > 10 ∨ x < 0}"
    (phrase->math-notation
     "the set of integer numbers x are greater from 10 or less from 0"))))


(test phrase-to-predicate
  (let ((predicate
          (phrase->predicate
           "the set of integer numbers x are greater from 10")))

    (check (funcall predicate 20))
    (check-not (funcall predicate 10))
    (check-not (funcall predicate 5))
    (check-not (funcall predicate 10.5))))


;;;================================================================================
;;;                              ERROR CASES
;;;================================================================================

(test invalid-domain
  (raise-error
    (phrase->ast
     "the set of imaginary numbers x")
    simple-error))


(test missing-variable
  (raise-error
    (phrase->ast
     "the set of integer numbers")
      simple-error))


(test invalid-condition
   (raise-error
    (phrase->ast
     "the set of integer numbers x are")
       simple-error))


(test invalid-comparison-value
   (raise-error
    (phrase->ast
     "the set of integer numbers x are greater from abc")
    simple-error))


;;;================================================================================
;;;                              END OF FILE
;;;================================================================================
