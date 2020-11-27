(in-package #:cl-user)

(defpackage #:server.utils
  (:use #:cl)
  (:export #:code-replace-symbol
	   #:replace-symbol-this
	   #:html-tag-p
	   #:make-keyword
	   #:starts-with-p
	   #:substring
	   #:make-uuid
	   #:hash-password
	   #:check-password
	   #:get-v
	   #:plist->alist))

(defpackage #:server.core
  (:use #:cl #:server.utils)
  (:export #:fun
	   #:regexp
	   #:lisp-raw
	   #:list-raw))

(defpackage #:client.utils
  (:use #:cl #:parenscript #:server.core)
  (:export #:str
	   #:query
	   #:classes
	   #:foreach
	   #:clone-object
	   #:event-handler
	   #:http-handler
	   #:set-cookie
	   #:get-cookie
	   #:printv
	   #:object-keys
	   #:join
	   #:empty-p
	   #:get->
	   #:style-prop
	   #:new-state
	   #:distinct
	   #:range
	   #:concat))

(defpackage #:client.core
  (:use #:cl
	#:parenscript
	#:client.utils
	#:server.core
	#:server.utils))
