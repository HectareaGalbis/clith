
(asdf:defsystem "clith"
  :depends-on ("adp" "alexandria")
  :components ((:file "package")
	       (:file "clith")
	       (:file "definitions")))


(asdf:defsystem "clith/docs"
  :depends-on ("adp" "alexandria")
  :components ((:file "package")
	       (:file "clith")
	       (:file "definitions")
	       (:file "README")))
