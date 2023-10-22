
(defsystem "clith"
  :depends-on ("alexandria")
  :components ((:file "package")
	       (:file "clith")))


(defsystem "clith/docs"
  :defsystem-depends-on ("adp-github")
  :build-operation "adp-github-op"
  :depends-on ("clith")
  :components ((:scribble "reference")
               (:scribble "README")))
