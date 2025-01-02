
(defsystem "clith"
  :author "Héctor Galbis Sanchis"
  :description "Common Lisp wITH macro. A general WITH macro."
  :license "MIT"
  :depends-on ("alexandria" "expanders")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
	                     (:file "clith")
                             (:file "definitions")))))


;; (defsystem "clith/docs"
;;   :defsystem-depends-on ("adp-github")
;;   :class :adp-github
;;   :depends-on ("clith")
;;   :components ((:module "scribble"
;;                 :components ((:file "package")
;;                              (:scribble "reference")
;;                              (:scribble "README")))))
