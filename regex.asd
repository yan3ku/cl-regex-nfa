;;;; regex.asd

(asdf:defsystem #:regex
  :description "Describe regex here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "regex")))

(asdf:defsystem #:regex/test
  :description "Describe regex here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:regex)
  :serial t
  :components ((:file "regex-test")))
