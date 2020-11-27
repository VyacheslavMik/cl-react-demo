(in-package #:asdf-user)

(defsystem #:cl-react-demo
  :description "Demo project with react wrapper in parescript"
  :version "0.0.1"
  :author "Vyacheslav Mikushev <vyacheslav.mik.8@gmail.com>"
  :depends-on (#:hunchentoot
	       #:parenscript
	       #:cl-ppcre
	       #:cl-json
	       #:uuid
	       #:ironclad
	       #:cl-who
	       #:lass
	       #:clss)
  :components ((:file "packages")
	       (:file "style/core")
	       (:file "server/routing")
	       (:file "server/utils")
	       (:file "server/parenscript")
               (:file "server/core")))
