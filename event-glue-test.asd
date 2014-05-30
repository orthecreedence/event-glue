(asdf:defsystem event-glue-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "Tests for event-glue"
  :depends-on (:fiveam :event-glue)
  :components
  ((:module test
    :serial t
    :components
    ((:file "main")))))

