(in-package :bbeyi)

;;; HTTP(S) 
(setq *show-lisp-errors-p* t) ;; set this to show error files in /priv/errors

;; define server config
;;;; these are set in $HOME/.bashrc to be accessible in the sbcl repl 
(defvar *bbeyi-http-port* (parse-integer (uiop:getenv "BBEYI_HTTP_PORT")))
(defvar *bbeyi-https-port* (parse-integer (uiop:getenv "BBEYI_HTTPS_PORT")))
(defvar *bbeyi-ssl-cert* (uiop:getenv "BBEYI_SSL_CERT"))
(defvar *bbeyi-ssl-key* (uiop:getenv "BBEYI_SSL_KEY"))
(defvar *bbeyi-url* (uiop:getenv "BBEYI_HOST"))

;; WEBSOCKET SERVER AND FUNCTIONS
(defclass ws-endpoint (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this server") :reader :name :accessor name))
  (:default-initargs :client-class 'ws-user)
  )

(defclass ws-user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!") :accessor name)))

(defvar *ws-endpoints* (list (make-instance 'ws-endpoint :name "/ws")))

(defun find-ws-endpoint (request)
  (find (hunchentoot:script-name request) *ws-endpoints* :test #'string= :key #'name))

(pushnew 'find-ws-endpoint hunchensocket:*websocket-dispatch-table*)


;; we need to use easy-routes over websockets, so we will create children of both
(defclass ws-routes-acceptor (easy-routes-acceptor acceptor-websocket)
  ()
  (:documentation "a subclass of routes-acceptor and hunchentsocket"))

(defclass ws-routes-ssl-acceptor (easy-routes-ssl-acceptor websocket-ssl-acceptor)
  ()
  (:documentation "routes and websockets over ssl"))


;; redirect all traffic to https
(defclass http-to-https-acceptor (hunchentoot:acceptor) ())
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor http-to-https-acceptor) request)
  (hunchentoot:redirect (hunchentoot:request-uri request)
                        :protocol :https :port *bbeyi-https-port*))

(defvar *bbeyi-wss-acceptor* (make-instance 'ws-routes-ssl-acceptor :port *bbeyi-https-port*
								    :ssl-certificate-file *bbeyi-ssl-cert*
								    :ssl-privatekey-file *bbeyi-ssl-key*
								    :document-root (truename "~/common-lisp/bbeyi/priv/")
								    :error-template-directory (truename "~/common-lisp/bbeyi/priv/errors/")))

(defvar *bbeyi-http-acceptor* (make-instance 'http-to-https-acceptor :port *bbeyi-http-port*))

;; set logging to files
					;(setf (acceptor-message-log-destination *bbeyi-wss-acceptor*) (truename "~/common-lisp/bbeyi/logs/message.log"))
					;(setf (acceptor-access-log-destination *bbeyi-wss-acceptor*) (truename "~/common-lisp/bbeyi/logs/access.log"))
;; don't allow persistent connections
;; this is because the server was not responding to requests, with a 503, and the error logs were showing too many threads.
;; still investigation, but maybe the connections were sending a keep alive header.
(setf (acceptor-persistent-connections-p *bbeyi-http-acceptor*) nil)
(setf (acceptor-persistent-connections-p *bbeyi-wss-acceptor*) nil)

;; after reviewing the taskmaster section of the docs, either of two things happened, because i was having one active connections
;; 1). the connections persisted, I don't why that is, but i have stopped persistent connections.
;; 2). The taskmaster ran out of threads, or the max accept was exceeded by the active requests.
;; 3). this is the solution, stop persistent connections above, then increase the threads to 1000, and max accept to 1500.

(let ((http-taskmaster (slot-value *bbeyi-http-acceptor* 'taskmaster))
      (https-taskmaster (slot-value *bbeyi-wss-acceptor* 'taskmaster)))
  (setf (slot-value http-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value http-taskmaster 'hunchentoot::max-accept-count) 15000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-accept-count) 15000))

(defun start-server ()
  "Start the server"
  (stop-server)
  (start-kvrocks)
  (hunchentoot:start *bbeyi-http-acceptor*)
  (hunchentoot:start *bbeyi-wss-acceptor*)
  )

(defun stop-server ()
  "Stop the server"
  (when (started-p *bbeyi-http-acceptor*)
    (stop *bbeyi-http-acceptor*))
  (when (started-p *bbeyi-wss-acceptor*)
    (stop *bbeyi-wss-acceptor*))
  (handler-case (stop-kvrocks)
    (redis:redis-connection-error (err)
      (declare (ignore err)))))

(defun restart-server ()
  (stop-server)
  (start-server))

;; websocket methods to handle communication via websocket
(defmethod hunchensocket:client-connected ((endpoint ws-endpoint) ws-user))

(defmethod hunchensocket:text-message-received ((endpoint ws-endpoint) ws-user message-json)
  (let* ((message (jzon:parse message-json))
	 (message-type (gethash "type" message nil)))
    (trivia:match message-type
      ("auto-complete"
       (let* ((fragment (or (gethash "fragment" message) #()))
	      (auto-words (or (mapcar (lambda (str)
					(str:downcase (nlp:remove-punctuation str))) (get-autocomplete fragment)) #())))
	 (hunchensocket:send-text-message ws-user  (jzon:stringify auto-words)))))))


(defun ws-js-code ()
  "generate the code for websockets and handling of the back and forth between client and server"
  (parenscript:ps
    (setf *socket* (new (-web-socket "/ws")))
    (setf (chain *socket* onopen) (lambda () ((chain console log) "connected to server")))
    (setf (chain *socket* onmessage) (lambda (event) (display-suggestions ((chain -j-s-o-n parse) (chain event data)))))
    (setf (chain *socket* onclose) (lambda () ((chain console log) "socket closed!")))
    (setf (chain *socket* onerror) (lambda (err) ((chain console log) err)))
    ;; capture user input
    (defvar *input* (chain document (get-element-by-id "search-input")))
    (chain *input* (add-event-listener "input" (lambda ()
						 (let ((query (chain this value)))
						   (if (> (chain query length) 0)
						       (chain *socket* (send (chain -j-s-o-n (stringify (create fragment query
														type "auto-complete")))))
						       (clear-suggestions))))))
    (defun clear-suggestions ()
      (setf (chain document (get-element-by-id "suggestions") inner-h-t-m-l) ""))
    (defun display-suggestions (suggestions)
      (let ((suggestions-container ((chain document get-element-by-id) "suggestions")))
	(setf (chain suggestions-container inner-h-t-m-l) "")
	(chain suggestions (for-each (lambda (suggestion)
				       (let ((suggestion-div (chain document (create-element "div"))))
					 (setf (getprop suggestion-div 'class-name) "autocomplete-suggestion")
					 (setf (getprop suggestion-div 'text-content) suggestion)
					 (chain suggestion-div (add-event-listener "click" (lambda ()
											     (setf (getprop *input* 'value) suggestion)
											     (clear-suggestions))))
					 (chain suggestions-container (append-child suggestion-div))))))))))

(defun analytics-js ()
  "add google analytics and microsoft clarity"
  (with-html-output (*standard-output*)
    (htm (:script :async t :src "https://www.googletagmanager.com/gtag/js?id=G-FRPZLDV6PZ")
	 (:script "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'G-FRPZLDV6PZ');"))))

;; PAGES
(defroute index-page ("/" :method :get) ()
  (with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:link :rel "icon" :href "/static/icons/web/favicon.ico" :sizes "any")
      (:link :rel "apple-touch-icon" :href "/static/icons/web/apple-touch-icon.png")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title "Bbeyi")
      (:style (str (index-css)))
      (analytics-js))
     (:body
      (:header
       (:div :class "logo" (:a :href "/" "Bbeyi")))
      (:div :class "container"
            ;; Search bar
            (:div :class "search-bar"
		  (:form :action "/search" :method "get"
			 (:div :class "search-and-button"
			       (:input :name "q" :type "text" :placeholder "Search products..." :id "search-input")
			       (:button "Search"))
	  		 (:div :id "suggestions" :class "autocomplete-suggestions")))
            ;; Product grid
            (:div :class "grid"
		  ;; Example product 1
		  (let ((products (test-data)))
		    (dolist (product products)
		      (htm 
		       (:div :class "product-card"
			     (:img :class "product-image" :src (first product) :alt (str (second product)))
			     (:div :class "product-info"
				   (:h2 :class "product-title" (str (third product)))
				   (:p :class "product-price" (str (fourth product)))
				   (:p :class "product-site" (str (str:capitalize (fifth product)))))))))))
      ;; Footer outside the container
      (:footer
       ;; Links section
       (:div :class "footer-links"
             (:a :href "mailto:bbeyi@ninx.xyz" "Email Us") " | "
             (:a :href "tel:+256785842699" "Call Us") " | "
             (:a :href "/privacy" "Privacy"))
       ;; Separator line
       (:hr :class "footer-separator")
       ;; Copyright line
       (:p "© 2024 Bbeyi. All rights reserved."))
      ;; JavaScript (not included in this snippet)
      (:script (str (ws-js-code)))))))

(defun test-data ()
  "generates data for testing our application web pages"
  (let* ((keys (subseq (redis:red-keys "{product}:*") 0 5))
	 (data (mapcar #'redis:red-hgetall keys)))
    (mapcar (lambda (d)
	      (list (get-value-by-key "image-url" d)
		    (get-value-by-key "name" d)
		    (get-value-by-key "name" d)
		    (get-value-by-key "price" d)
		    (get-value-by-key "site" d)
		    (get-value-by-key "url" d)))
	    data)))

(defun search-data (query)
  "builds search data for a given query"
  (let* ((keys (get-search query))
	 (data (mapcar #'redis:red-hgetall keys)))
    (mapcar (lambda (d)
	      (list (get-value-by-key "image-url" d)
		    (get-value-by-key "name" d)
		    (get-value-by-key "name" d)
		    (get-value-by-key "price" d)
		    (get-value-by-key "site" d)
		    (get-value-by-key "url" d)))
	    data)))

(defun get-value-by-key (key lst)
  (let ((pos (position key lst :test #'equal)))
    (if pos
        (nth (1+ pos) lst)
        nil)))

(defun index-css ()
  (cl-css:css
   '((body :font-family "Arial, sans-serif" :margin "0" :padding "0" :background-color "#f4f4f4" :color "#333" :min-height "100vh" :position "relative")
     (header :background-color "#007BFF" :padding "10px 20px" :color "white" :display "flex" :align-items "center")
     ("header .logo" :font-weight "bold" :font-size "24px")
     (".logo a" :color "white" :text-decoration "none")
     (".logo a:hover" :color "#00ffcc") ;; Hover color
     (".container" :width "60%" :max-width "1200px" :margin "20px auto" :padding "0 20px" :text-align "center")
     (".search-bar" :margin-bottom "30px" :display "flex" :flex-direction "column" :position "relative")
     (".search-bar input[type='text']" :width "calc(100% - 40px)" :padding "10px" :font-size "16px" :border "1px solid #ccc" :border-radius "4px" :box-shadow "0 2px 5px rgba(0,0,0,0.1)")
     (".search-bar button" :padding "10px 20px" :border "1px solid #ccc" :border-radius "0 5px 5px 0" :background-color "#007BFF" :color "white" :cursor "pointer" :border-left "none")
     (".search-bar button:hover" :background-color "#0056b3")
     (".search-and-button" :width "100%" :display "flex" :flex-direction "row")
     (".autocomplete-suggestions" :border "1px solid #ccc" :max-height "150px" :overflow-y "auto" :position "absolute" :background-color "white" :z-index "1000" :width "calc(100% - 40px)" :left "3%" :box-shadow "0 2px 5px rgba(0,0,0,0.1)" :margin-top "5px" :top "100%")
     (".autocomplete-suggestion" :padding "10px" :cursor "pointer" :border-bottom "1px solid #ddd")
     (".autocomplete-suggestion:hover" :background-color "#f0f0f0")
     (".grid" :display "flex" :flex-wrap "wrap" :justify-content "center" :gap "20px")
     (".product-card" :background-color "white" :border-radius "5px" :overflow "hidden" :box-shadow "0 4px 8px rgba(0, 0, 0, 0.1)" :transition "box-shadow 0.3s ease" :width "100%" :max-width "300px")
     (".product-card:hover" :box-shadow "0 8px 16px rgba(0, 0, 0, 0.2)")
     (".product-image" :width "100%" :height "auto")
     (".product-info" :padding "15px" :text-align "center")
     (".product-title" :font-size "18px" :margin-bottom "10px")
     (".product-price" :font-size "16px" :color "#007BFF" :margin-bottom "10px")
     (".product-site" :font-size "14px" :color "#888")
     (footer :background-color "#007BFF" :color "white" :text-align "center" :padding "10px 20px" :position "relative" :width "100%" :box-sizing "border-box")
     ;; Footer links style
     (".footer-links" :text-align "center" :color "white" :margin-bottom "10px")
     ;; Footer links hover effect
     (".footer-links a" :color "white" :text-decoration "none")
     (".footer-links a:hover" :color "#00ffcc") ;; Hover color
     ;; Separator line style
     (".footer-separator" :border "0" :border-top "1px solid white" :margin "10px 0")
     ;; Pagination styles
     (".pagination" :display "flex" :justify-content "center" :align-items "center" :margin "20px 0")
     (".pagination a" :padding "10px 15px" :margin "0 5px" :border "1px solid #007BFF" :background-color "#007BFF" :color "white" :text-decoration "none" :border-radius "5px")
     (".pagination a:hover" :background-color "#0056b3")
     (".pagination .current-page" :padding "10px 15px" :margin "0 5px" :border "1px solid #333" :background-color "#333" :color "white" :border-radius "5px")
     ("@media screen and (max-width: 600px)"
      (".container" :width "94%")
      (".search-bar input[type='text']" :width "100%")
      (".product-image" :width "60%" :margin "0 auto")
      (".grid" :grid-template-columns "1fr")
      (".product-title" :font-size "16px")
      (".product-price" :font-size "14px")
      (".product-site" :font-size "12px")))))


(defun prepare-url (url)
  "remove punctuation, make lowercase, replace space with + for readability"
  (str:replace-all " " "+" (nlp:remove-punctuation (str:downcase url))))


(defroute search-page ("/search" :method :get) (q page)
  (let* ((products (search-data q))
         (total-products (length products))
         (products-per-page 20)
         (total-pages (ceiling (/ total-products products-per-page)))
         (current-page (parse-integer (or page "1")))
         (start-index (* (1- current-page) products-per-page))
         (end-index (min total-products (* current-page products-per-page)))
         (paged-products (subseq products start-index end-index))
         (start-page (max 1 (- current-page 4)))
         (end-page (min total-pages (+ current-page 4))))
    (with-html-output-to-string (*standard-output*)
      (:html
       (:head
        (:meta :charset "utf-8")
	(:link :rel "icon" :href "/static/icons/web/favicon.ico" :sizes "any")
        (:link :rel "apple-touch-icon" :href "/static/icons/web/apple-touch-icon.png")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:title (str (format nil "~a | Bbeyi" q)))
        (:style (str (index-css)))
	(analytics-js))
       (:body
        (:header
         (:div :class "logo" (:a :href "/" "Bbeyi")))
        (:div :class "container"
              ;; Search bar
              (:div :class "search-bar"
                    (:form :action "/search" :method "get"
                           (:div :class "search-and-button"
                                 (:input :name "q" :type "text" :placeholder "Search products..." :id "search-input" :value (str q))
                                 (:button "Search"))
                           (:div :id "suggestions" :class "autocomplete-suggestions")))
              ;; Product grid
              (:div :class "grid"
                    (dolist (product paged-products)
                      (htm 
                       (:div :class "product-card"
                             (:img :class "product-image" :src (first product) :alt (str (second product)))
                             (:div :class "product-info"
                                   (:h2 :class "product-title" (str (third product)))
                                   (:p :class "product-price" (str (fourth product)))
                                   (:p :class "product-site" (str (str:capitalize (fifth product))))))))
                    ;; Pagination
                    (:div :class "pagination"
                          ;; Back button
                          (when (> current-page 1)
                            (htm (:a :href (format nil "/search?q=~a&page=~a" (prepare-url q) (- current-page 1)) "Back")))
                          ;; Page numbers
                          (loop for i from start-page to end-page do
                            (if (= i current-page)
                                (htm (:span :class "current-page" (str i)))
                                (htm (:a :href (format nil "/search?q=~a&page=~a" (prepare-url q) i) (str i)))))
                          ;; Next button
                          (when (< current-page total-pages)
                            (htm (:a :href (format nil "/search?q=~a&page=~a" (prepare-url q) (+ current-page 1)) "Next"))))))
        ;; Footer outside the container
        (:footer
         ;; Links section
         (:div :class "footer-links"
               (:a :href "mailto:bbeyi@ninx.xyz" "Email Us") " | "
               (:a :href "tel:+256785842699" "Call Us") " | "
               (:a :href "/privacy" "Privacy"))
         ;; Separator line
         (:hr :class "footer-separator")
         ;; Copyright line
         (:p "© 2024 Bbeyi. All rights reserved."))
        ;; JavaScript (not included in this snippet)
        (:script (str (ws-js-code))))))))

