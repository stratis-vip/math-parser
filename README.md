# Math-Parser
A simple math parser that creates math-notation about set (for now)

# Overview 

A simple utility that converts english or greek description for sets to the math representation.
```lisp  
(phrase->math-notation "the set of real numbers x which  are even")
"{x ∈ ℝ | x ≡ 0 (mod 2)}"
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
