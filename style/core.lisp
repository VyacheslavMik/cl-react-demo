(in-package #:server.core)

(defparameter css
  (lass:compile-and-write
   '(html
     :height "100%"
     :width "100%"
     :display "flex"

     (body
      :margin 0
      :font-family "Roboto,sans-serif"
      :font-weight "400"
      :background "#2a469420"
      :height "100%"
      :width "100%"

      ((:or div form)
       :display "flex"
       :font-size "18px")

      (.hidden :visibility "hidden")

      (.column :flex-direction "column")

      (.text-button
       :border "none"
       :background "transparent"
       :outline "none"
       :color "#ffffffbb"
       :padding 0)

      ((:and .text-button :hover)
       :color "#fff")

      (.page-container
       :margin "auto"
       :max-width "900px"
       :width "100%"
       :height "100%")

      (.page-content
       :overflow-y "auto"
       :background "#2a469470"
       :flex "1")

      (.post-page
       :flex "1"
       :padding "20px"

       ((:or form .post-form .content-field)
	:flex "1")

       (.title-field
	:max-width "430px")

       (.button-container
	:padding-top "20px"))

      (.register-request-page
       :flex "1"
       :padding "20px"

       (form
	:flex "1"

	(.auth
	 :flex "1"
	 :align-items "center"

	 (.form-field
	  :max-width "300px"
	  :margin-bottom "20px"))))

      (.check-email-page
       :flex "1"
       :padding "40px 20px 20px 20px"
       :color "#ffffffee"
       :justify-content "center")

      (.register-finish-page
       :flex "1"
       :padding "40px 20px 20px 20px"
       :color "#ffffffee"
       :justify-content "center")

      (.login-page
       :flex "1"
       :padding "20px"

       (form
	:flex "1"

	(.auth
	 :flex "1"
	 :align-items "center"

	 (h3
	  :color "#ffffffee"
	  :margin-bottom "20px")

	 (.form-field
	  :max-width "300px"
	  :margin-bottom "20px"))))

      (.topic-page
       :flex "1"
       :padding "20px"

       (.topic-title
	:text-align "center"
	:color "#ffffffee")

       (.topic-message
	:color "#ffffffee"
	:padding "20px 0"
	:border-bottom "1px solid #f5f2ef"

	((:or .topic-message-author .date-view)
	 :color "#ffffff90"
	 :font-size "14px")

	(.date-view
	 :margin-left "auto"))

       (.topic-pager
	:padding "20px 0"
	:justify-content "center"

	(.topic-goto-page-button
	 :padding "5px 10px")

	(span
	 :color "#ffffff90"
	 :cursor "default"
	 :padding "5px 10px")

	((:and .topic-goto-page-button .selected)
	 :color "#ffffff30"
	 :cursor "default"))

       (.new-message-form
	:flex "1"

	(.content-field
	 :flex "1"
	 :min-height "250px")

	(.button-container
	 :padding "20px 0")))

      (.topics-page
       :flex "1"
       :padding "20px"

       (.new-topic-button
	:margin-left "auto")

       (.topic-view
	:background "transparent"
	:border "none"
	:border-bottom "1px solid #f5f2ef"
	:outline "none"
	:padding "20px 0"

	(.topic-title
	 :color "#ffffffbb")

	(.topic-and-last-message
	 :align-items "flex-start")

	(.last-message-info
	 :color "#ffffffbb"
	 :font-size "14px")

	(.last-message-date
	 :font-size "14px"
	 :padding-left "3px")

	(.topic-info
	 :color "#ffffffbb"
	 :font-size "14px"
	 :margin-left "auto"
	 :align-items "flex-end"))

       ((:and .topic-view :hover)

	(.topic-title
	 :color "#fff"))

       ((:and .topic-view :last-of-type)
	:border "none")

       (h3
	:color "#fff"
	:margin "0 auto 10px auto")

       ((:or form .new-topic-form .message-field)
	:flex "1")

       (.title-field
	:max-width "430px")

       (.button-container
	:padding-top "20px"))

      (.field-divider :padding-top "20px")

      (.horizontal-divider :padding-left "20px")

      (.form-field :overflow "hidden")

      (.page-header
       :background "#2A4694"

       ((:or .page-header-item .page-header-user)
       	:padding "10px 20px")

       ((:and .page-header-item .selected)
	:color "#ffffff30"
	:cursor "default")

       (.page-header-user
	:cursor "default"
	:padding "10px 0"
	:color "#f5f2ef")

       (.divider :margin-left "auto"))

      (.register-request-page
       (.text-input :width "400px"))

      (.last-posts
       (.post :padding "0 20px 40px 20px"

	(h2 :color "#fff")

	(.divider :border "1px solid #f5f2ef")

	(.edit-button-container :padding-top "10px")))

      (.markdown
       :flex-direction "column"
       :width "100%")

      ((:or
	.markdown-input
	.text-input
	.textarea-input
	.select-input)
       :flex-direction "column"
       :flex "1"

       (.error-text
	:color "#7b5774bb"
	:cursor "default"
	:font-size "14px"
	:padding "3px 0")

       (.error-text
	:color "#7b5774bb"
	:cursor "default"
	:font-size "14px"
	:padding "3px 0")

       ((:and .error-text .focused)
	:color "#7b5774"))

      (.text-input
       (.input-container
	:background "#fff")

       (.label
	:padding "3px 0px"
	:color "#ffffffbb")

       ((:and .label .focused)
	:color "#fff")

       (input
	:padding "5px 10px"
	:outline "none"
	:border "none"
	:background "#2a469420"
	:flex "1"))

      (.markdown-input

       (.submit-button
	:background "transparent"
	:border "none"
	:outline "none")

       (.fa :color "#7b5774bb")

       ((:and .submit-button :hover)
	(.fa :color "#7b5774ff"))

       ((:and .submit-button .selected)
	:cursor "default"

	(.fa
	 :color "#7b577430"))

       (.markdown-input-header

	(.label
	 :padding "3px 0px"
	 :color "#ffffffbb")

	((:and .label .focused)
	 :color "#fff")

	(.markdown-input-controls
	 :background "white"
	 :margin-left "auto"

	 (.markdown-input-controls-container
	  :background "#2a469420"
	  :padding "3px 10px"

	  ((:and .file-uploader :hover)
	   (.fa :color "#7b5774ff"))

	  (.file-uploader
	   :position "relative"
	   :justify-content "center"
	   :cursor "pointer"

	   (label
	    :opacity 0
	    :width "100%"
	    :height "100%"
	    :position "absolute"
	    :top 0
	    :left 0
	    :cursor "pointer"

	    (input :display "none"))))))

       (.preview-container
	:background "#fff"
	:overflow-y "auto"
	:display "block"
	:flex "1"

	(.preview
	 :background "#2a469420"
	 :min-height "calc(100% - 20px)"
	 :padding "10px"
	 :border "none"

	 (.markdown
	  :color "#000")))

       (.markdown-text-input
	:flex-direction "column"
	:background "#fff"

	((:and textarea :focus)
	 :outline "none")

	(textarea
	 :resize "vertical"
	 :flex "1"
	 :padding "10px"
	 :background "#2a469420"
	 :border "none")))

      (div#root
       :height "100%"
       :width "100%"
       :margin 0
       :position "relative"

       (div#app
	:height "100%"
	:width "100%"
	:margin 0
	:position "relative"

	(div#modal
	 :position "absolute"
	 :width "100%"
	 :height "100%"
	 :background "rgba(102,102,102,0.7)"

	 (.modal-content
	  :margin "auto"
	  :background "white"
	  :padding "40px 80px"))))))))
