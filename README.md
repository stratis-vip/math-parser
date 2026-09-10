# Math-Parser
A simple math parser that creates math-notation about set (for now)

# Overview 

A simple utility that converts english or greek description for sets to the math representation.
```lisp  
(phrase->math-notation "the set of real numbers x which  are even") ; => "{x ∈ ℝ | x ≡ 0 (mod 2)}"

(phrase->math-notation "το συνολο των ακεραίων αριθμών X που είναι μεγαλύτεροι από το 0") ;=> "{x ∈ ℤ | x > 0}"
```
# Usage 
## Installation 

You need to clone this repo to a position that common lisp recognize (usually to ~/common-lisp/). 
This package depends on

* [strings](https://github.com/stratis-vip/strings)
* [review for testing](https://github.com/stratis-vip/review)
	
After that you can load package with 
```lisp 
(asdf:load-system :math-parser)
(asdf:load-system :math-parser/tests)
```
## Functions 
### phrase->ast
```lisp
(defun phrase->ast (phrase &key (mwt *multi-word-tokens*))
  "Parse PHRASE and return its abstract syntax tree.

PHRASE is a natural-language description of a mathematical set.
MWT specifies the multi-word-token table used by the lexer.

The returned AST is a SET-EXPRESSION."
...)
```

### phrase->predicate 
```lisp
(defun phrase->predicate (phrase &key (mwt *multi-word-tokens*))
  "Convert a natural-language set description into a membership predicate.

The returned function accepts one argument X and returns true when X
belongs to the described set.

The predicate always enforces the set's DOMAIN. If the optional
condition is present, the condition is also enforced."
```

### phrase->math-notation 
```lisp
(defun phrase->math-notation (phrase &key (mwt *multi-word-tokens*))
  "Convert a natural-language set description into mathematical notation.

PHRASE is first converted into an AST and then into a mathematical
set-builder expression.

For example:

  \"the set of integer numbers x\"
      => \"{x ∈ ℤ}\"

  \"the set of integer numbers x are greater from 10\"
      => \"{x ∈ ℤ | x > 10}\""
	  ...)
```
