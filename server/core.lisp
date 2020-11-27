(in-package #:server.core)

(setf json:*json-array-type* 'vector)

(defvar *http-acceptor*)
(defvar *sessions* ())
(defun start ()
  (stop)
  (setf *http-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start *http-acceptor*))

(defun stop ()
  (when (and (boundp '*http-acceptor*) *http-acceptor*)
    (hunchentoot:stop *http-acceptor* :soft t)
    (setf *http-acceptor* nil)))

(let ((output *standard-output*))
  (defun log-message (message &rest args)
    (apply #'format output (concatenate 'string "~%~%" message "~%~%") args)))

(defun authorized-user (authorization)
  (multiple-value-bind (foundp a) (cl-ppcre:scan-to-strings "^Bearer (.+)" authorization)
    (when foundp
      (let ((token (elt a 0)))
	(cdr (assoc token *sessions* :test #'string=))))))

(defparameter *empty-json* (make-hash-table))

(defun content-type (type)
  (ecase type
    (:html       "text/html")
    (:javascript "application/javascript")
    (:css        "text/css")
    (:json       "application/json")
    (:png        "image/png")))

(defun parse-query-string (qs)
  (let (body)
    (flet ((parse-param (s)
	     (destructuring-bind (k v) (cl-ppcre:split "=" s)
	       (push (cons (make-keyword k) v) body))))
      (dolist (param (cl-ppcre:split "&" qs))
	(parse-param param)))
    body))

(defun body (method query-string request route)
  (ecase method
    (:get
     (parse-query-string query-string))
    ((:post :patch)
     (values
      (if (cdr (assoc :binary route))
	  (hunchentoot:raw-post-data :request request :force-binary t)
	  (cl-json:decode-json-from-string
	   (hunchentoot:raw-post-data :request request :force-text t)))
      (parse-query-string query-string)))

    (:delete
     ())))

(defun access-deniedp (route user)
  (let ((roles (get-v route :roles)))
    (when roles
      (unless (and (member :any roles) user)
	(let* ((role (get-v user :role))
	       (role (and role (make-keyword role))))
	  (not (member role roles)))))))

(defun acceptor-dispatch-request (request)
  (destructuring-bind (uri &optional qs) (cl-ppcre:split "\\?" (hunchentoot:request-uri request))
    (let* ((method (hunchentoot:request-method request))
	   (route  (find-route method uri)))
      (if (null route)
	  (progn
	    (setf (hunchentoot:content-type*) "application/json")
	    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	    (cl-json:encode-json-to-string "Route not found"))
	  (let* ((params   (cdr (assoc :params route)))
		 (route    (cdr (assoc :route route)))
		 (type     (or (cdr (assoc :type route)) :json))
		 (function (cdr (assoc :function route)))
		 (user     (authorized-user
			    (hunchentoot:header-in :authorization request))))
	    (cond
	      ((access-deniedp route user)
	       (setf (hunchentoot:content-type*) "application/json")
	       (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
	       (cl-json:encode-json-to-string "Access denied"))

	      ((eql type :static-folder)
	       :call-next-method)

	      ((eql type :continue)
	       :call-next-method)

	      ((eql type :static-file)
	       (funcall function))

	      (t
	       (setf (hunchentoot:content-type*) (content-type type))
	       (multiple-value-bind (body aux) (body method qs request route)
		 (let* ((route (cons (cons :user user) route))
			(result (funcall function route params body aux)))
		   (if (eql type :json)
		       (if result
			   (cl-json:encode-json-to-string result)
			   (cl-json:encode-json-to-string *empty-json*))
		       result))))))))))

(defun notify-error (err request)
  (destructuring-bind (uri &optional qs) (cl-ppcre:split "\\?" (hunchentoot:request-uri request))
    (let ((method (hunchentoot:request-method request)))
      (let ((user (authorized-user
		   (hunchentoot:header-in :authorization request)))
	    (fmt-str "Process request error: ~a~%Uri: ~a~%Method: ~a~%Query string: ~a~%Body: ~a~%User: ~a"))
	(multiple-value-bind (body _) (body method qs request ())
	  (declare (ignore _))
	  (setf user (remove :password user :key #'car))
	  (sb-thread:make-thread
	   (lambda ()
	     (log-message
	      (format nil fmt-str err uri method qs body user)))
	   :name "notify-error"))))))

(defmethod hunchentoot:acceptor-dispatch-request :around ((acceptor hunchentoot:easy-acceptor) request)
  (handler-case
      (let ((res (acceptor-dispatch-request request)))
	(if (eql res :call-next-method)
	    (call-next-method)
	    res))
    (error (c)
      (notify-error c request)
      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
      ())))

(defmethod hunchentoot:acceptor-status-message :around ((acceptor hunchentoot:easy-acceptor) code
							&key &allow-other-keys)
  (if (= code hunchentoot:+http-internal-server-error+)
      (progn
	(setf (hunchentoot:content-type*) "application/json")
	(cl-json:encode-json-to-string "internal-server-error"))
      (call-next-method)))

(defroute (:get "/") ((:type . :html))
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "Бетельгейзе. Minecraft сервер.")

	    (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/pure/0.6.0/pure-min.css")
	    (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.25.2/codemirror.min.css")
	    (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.25.2/theme/monokai.min.css")
	    (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.11.0/styles/github.min.css")
	    (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")

	    (:link :href "/styles.css" :rel "stylesheet")
	    (:link :href "https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900&amp;subset=cyrillic,cyrillic-ext" :rel "stylesheet")

	    (:script :type "text/javascript" :src "https://unpkg.com/react@16/umd/react.development.js")
	    (:script :type "text/javascript" :src "https://unpkg.com/react-dom@16/umd/react-dom.development.js")
	    (:script :type "text/javascript" :src "/js/client.js"))
     (:body :onload (ps:ps (client.core::init)) (:div :id "root")))))

(defroute (:get "/js/client.js") ((:type . :javascript))
  (apply
   #'concatenate
   'string
   (ps:ps* ps:*ps-lisp-library*)
   (mapcar 
    (lambda (f)
      (parenscript:ps-compile-file f))
    '("client/utils.paren"
      "client/macro-utils.paren"
      "client/js-class.paren"
      "client/react.paren"
      "client/form.paren"
      "client/http.paren"

      "client/core.paren"))))

(defroute (:get "/styles.css") ((:type . :css))
  css)

(defroute (:get "/favicon.ico")
    ((:type . :continue)))
