(defpackage :math-parser
  (:use :cl :strings )
  (:export
    :phrase->ast
   :phrase->math-notation
    :phrase->predicate))
