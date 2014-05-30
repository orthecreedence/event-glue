(asdf:defsystem event-glue
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "A simple framework for event-based architectures."
  :components
  ((:file "package")
   (:file "event" :depends-on ("package"))))

