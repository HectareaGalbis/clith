
(defsystem "clith"
  :author "Héctor Galbis Sanchis"
  :description "Common Lisp wITH macro. A general WITH macro."
  :license "MIT"
  :depends-on ("alexandria" "expanders")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
	                     (:file "clith")
                             (:file "definitions"))))
  :in-order-to ((test-op (test-op "clith/test"))))

(defsystem "clith/test"
  :depends-on ("clith")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "harness")
                             (:file "tests"))))
  :perform (test-op (op c)
             (unless (uiop:symbol-call '#:clith/test '#:run-tests)
               (error "clith tests failed"))))


;; (defsystem "clith/docs"
;;   :defsystem-depends-on ("adp-github")
;;   :class :adp-github
;;   :depends-on ("clith")
;;   :components ((:module "scribble"
;;                 :components ((:file "package")
;;                              (:scribble "README")))))
