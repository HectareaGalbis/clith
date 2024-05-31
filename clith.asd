
(defsystem "clith"
  :author "Héctor Galbis Sanchis"
  :description "Common Lisp wITH macro. A general WITH macro."
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
	                     (:file "clith")
                             (:file "definitions")))))


;; (defsystem "clith/docs"
;;   :defsystem-depends-on ("adp-github")
;;   :build-operation "adp-github-op"
;;   :depends-on ("clith")
;;   :components ((:module "scribble"
;;                 :components ((:scribble "reference")
;;                              (:scribble "README")))))
