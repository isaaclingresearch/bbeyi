(in-package :redis)

(def-cmd ZUNION (num &rest keys) :anything
  "Return the union between the Zsets stored at key1, key2, ..., keyN.")

(in-package :bbeyi)

(defun start-kvrocks ()
  (sb-ext:run-program (namestring (truename "~/.bin/kvrocks"))
		      (list "-c" (namestring (truename "~/common-lisp/bbeyi/conf/kvrocks.conf")))
		      :wait nil)
  (sleep 3); allow some time for the program to start
  (handler-case (connect-kvrocks)
    (error (err)
      (print err))))

(defun stop-kvrocks ()
  (redis:red-shutdown)
  (sleep 3)
  (redis:disconnect))

(defun connect-kvrocks ()
  "connect to the server"
  (redis:connect :port 6666 :auth (uiop:getenv "KVROCKS_DEFAULT_PASSWORD")))

(defmacro with-kvrocks-txn ((&key namespace-token) &body body)
  "when given a namespace, switch to that namespace, else run the commands in a 'redis transaction'"
  `(progn
     (redis:red-multi)
     (when ,namespace-token
       (redis:red-auth ,namespace-token))
     ,@body
     (when ,namespace-token
       (redis:red-auth ,(uiop:getenv "KVROCKS_DEFAULT_PASSWORD")))
     (redis:red-exec)))

(defun make-date ()
  "return date as YYYY-MM-DD"
  (car (str:split "T" (format nil "~a" (local-time:today)))))

;;;;; data functions

(defun save-data (product-id name description price location site image-url url status condition)
  "when saving product data, we need an id to be used to update our saved product, more so the status for jiji.

all data os saved in {products} 
fragment indexes are created from name and description and saved in {index} for full text search"
  (let ((id (to-string (make-v7))))
    (redis:red-hset id "product-id" product-id)
    (redis:red-hset id "name" name)
    (redis:red-hset id "description" description)
    (redis:red-hset id "price" price)
    (redis:red-hset id "location" location)
    (redis:red-hset id "site" site)
    (redis:red-hset id "image-url" image-url)
    (redis:red-hset id "url" url)
    (redis:red-hset id "status" status)
    (redis:red-hset id "condition" condition)))
