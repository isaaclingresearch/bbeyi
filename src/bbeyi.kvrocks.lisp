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

;; data functions
#|
we have to store data in the following ways.
1. the original data so that in case we need to reformat the data, we can do it later on.
2. indexes for the data: 
   i.  the first index is for auto complete. auto complete index will return the product name on querying it.
   ii. the second index is the for ids, you want to match text to ids, such that when a user queries, you can return all products that match
       string fragment

the keys are
1. {index}:{search} for which the zset will contain ids
2. {index}:{autocomplete} for which the zset will contain names					;
it is important that the we use {index} to put all of them in the same slot to be able to use multikey functions ;
|#


(defun save-data (product-id name description price location site image-url url status condition)
  "when saving product data, we need an id to be used to update our saved product, more so the status for jiji.

all data os saved in {products} 
fragment indexes are created from name and description and saved in {index} for full text search"
  (let ((id (format nil "{product}:~a" product-id)))
    (if (redis:red-exists id)
	(format t "Product: ~a already saved ~%" name)
	(progn
	  (redis:red-hset id "product-id" product-id)
	  (redis:red-hset id "name" name)
	  (redis:red-hset id "description" description)
	  (redis:red-hset id "price" price)
	  (redis:red-hset id "location" location)
	  (redis:red-hset id "site" site)
	  (redis:red-hset id "image-url" image-url)
	  (redis:red-hset id "url" url)
	  (redis:red-hset id "status" status)
	  (redis:red-hset id "condition" condition)
	  (create-index id name)
	  (format t "Saved Product: ~a ~%" name)))))

(defun create-index (id name)
  "start with an id and name
tokenize the name. then for each token, save its fragments against both the id and name in two different sets.
forexample: tokens is saved in tok, toke, token, tokens fragments"
  (let ((tokens (nlp:tokenize name)))
    (dolist (token tokens)
      (when (>= (length token) 1)
	(save-to-index id name token)))))

(defun save-to-index (id name token &key (pos 1))
  #|given a word, start at length 1 then save the word fragments to {index}{name} and {index}{autocomplete}, 
   we use sorted sets, such that we can track the words appearing most in the dataset.|#
  (unless (> pos (length token))
    (let ((subtoken (str:substring 0 pos token)))
      (redis:red-zincrby (format nil "{index}{autocomplete}:~a" subtoken) 1 name)
      (redis:red-zincrby (format nil "{index}{search}:~a" subtoken) 1 id))
    (save-to-index id name token :pos (1+ pos))))

(defun get-autocomplete (txt)
  "given a fragment, get all product names for which it is part, return only 10 of the most frequent"
  (let* ((tokens (nlp:tokenize txt))
	 (names (mapcar (lambda (token)
			  (to-alist
			   (redis:red-zrange (format nil "{index}{autocomplete}:~a" (string-downcase token)) 0 -1 :withscores)))
			tokens)))
    (unless (equal '(nil) names)
      (let ((combined-alist (combine-alists names)))
	(if (<= (length combined-alist) 10)
	    (mapcar #'car combined-alist)
	    (mapcar #'car (subseq combined-alist 0 10)))))))

(defun get-search (txt)
  "given a fragment, get all product ids of it"
  (let* ((tokens (nlp:tokenize txt))
	 (names (mapcar (lambda (token)
			  (to-alist
			   (redis:red-zrange (format nil "{index}{search}:~a" (string-downcase token)) 0 -1 :withscores)))
			tokens)))
    (unless (equal '(nil) names)
      (mapcar #'car (combine-alists names)))))

(defun to-alist (lst)
  "Convert a list of elements into an alist assuming alternating key-value pairs."
  (loop for (key value) on lst by #'cddr
        collect (cons key value)))

(defun combine-alists (alist-list)
  "Combine a list of alists, summing integer values for each key."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (alist alist-list)
      (dolist (pair alist)
        (let* ((key (car pair))
               (value (parse-integer (cdr pair)))
               (current (gethash key result 0)))
          (setf (gethash key result) (+ current value)))))
    ;; Convert hash table to alist
    (let ((combined-alist nil))
      (maphash (lambda (key value)
                 (push (cons key value) combined-alist))
               result)
      (sort combined-alist (lambda (a b) (> (cdr a) (cdr b)))))))
