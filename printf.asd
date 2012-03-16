;;;; printf.asd

(asdf:defsystem #:printf
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Implementation of C's printf variants"
  :depends-on (#:smug)
  :serial t
  :components ((:file "package")
               (:file "printf")))

(asdf:defsystem #:printf.test
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Implementation of C's printf variants"
  :depends-on (#:printf)
  :serial t
  :components ((:file "tests")))

(defmethod asdf:perform ((op test-op) (system (eql (find-system '#:printf))))
  (asdf:load-system '#:printf.test)
  (eval (read-from-string "(printf.tests:run-tests)")))