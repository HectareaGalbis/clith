
(asdf:defsystem "clith"
  :depends-on ("adp" "alexandria")
  :components ((:file "package")
	       (:file "clith")))


(asdf:defsystem "clith/docs"
  :depends-on ("adp" "alexandria")
  :components ((:file "package")
	       (:file "clith")
	       (:file "README")))
