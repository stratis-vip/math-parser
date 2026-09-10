(defsystem :math-parser
  :pathname "src"

  :depends-on ("strings")
  
  :components
  ((:file "package")
   (:file "math-parser")))

(defsystem :math-parser/tests
  :pathname "tests"
  :depends-on ("strings" "review" "math-parser")

  :components
  ((:file "package")
   (:file "test-setup")
   (:file "parser-tests"))


  )
