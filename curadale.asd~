(defsystem "curadale"
  :author "Ninx technology limited"
  :description "a simple friendly patient education application."
  :depends-on (:cl-redis :str :com.inuoe.jzon :hunchentoot :hunchensocket :drakma :easy-routes :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :flexi-streams :parenscript :local-time)
  :components ((:module "src"
		:components ((:file "packages")
			     (:file "curadale.nlp")
			     (:file "curadale.sexp")
			     (:file "curadale.apis")
			     (:file "curadale.kvrocks")
			     (:file "curadale" :depends-on ("curadale.apis" "curadale.nlp"))
			     (:file "curadale.app")))
	       )
  :build-operation "program-op" ;; leave as is
  :build-pathname "curadale"
  :entry-point "curadale:start-server"
  )
