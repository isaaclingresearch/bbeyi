(defpackage :bbeyi.crawlers
  (:use :cl :drakma :com.inuoe.jzon :cl-hash-util :plump)
  (:nicknames :bcrawlers)
  (:documentation "This package for crawling data from sites")
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:shadow plump:parse)
  (:export :jiji))

(defpackage :bbeyi
  (:use :cl :redis :easy-routes :hunchentoot :hunchensocket :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :com.inuoe.jzon :parenscript :local-time)
  (:shadow easy-routes:redirect hunchentoot:reply parenscript:@ parenscript:stringify parenscript:% redis:close-connection cl-who:fmt redis:tell str:match)
  (:documentation "The main package of the bbeyi application.")
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :start-server :restart-server :start-kvrocks))
