(push (directory-namestring *load-truename*) asdf:*central-registry*)
(asdf:load-system "cl-react-demo")
(in-package #:server.core)
(start)
