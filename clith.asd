
(defsystem "clith"
  :author "Héctor Galbis Sanchis"
  :description "Common Lisp wITH macro. A general WITH macro."
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:file "package")
	       (:file "clith")))


;; In order to generate the documentation, uncomment
;; the following subsystem and eval (asdf:make "clith/docs")

;; (defsystem "clith/docs"
;;   :defsystem-depends-on ("adp-github")
;;   :build-operation "adp-github-op"
;;   :depends-on ("clith")
;;   :components ((:scribble "reference")
;;                (:scribble "README")))
