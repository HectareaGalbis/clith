
(in-package :clith)


(adp:in-file #P"README")


(adp:header "Common Lisp wITH")

(adp:text "Welcome to Clith!")

(adp:text "This library defines the macro " @f(with) ". It is like the 'with expression' in Python but better. It allows you to create some objects, bind them to some variables, evaluate some expressions using that variables, and lastly the objects are destroyed automatically. Even more, you can bind functions like LABELS does and nest expressions like UIOP:NEST.")

(adp:mini-table-of-contents)


(adp:subheader "Installation")

(adp:text "This library is available at Ultralisp. If you don't have it already, you can add it evaluating this:")

(adp:verbatim-code-block "Lisp"
  "(ql-dist:install-dist \"http://dist.ultralisp.org/\"
			 :prompt nil)")

(adp:text "After this you can install Clith using Quicklisp:")

(adp:verbatim-code-block "Lisp"
  "(ql:quickload :clith)")


(adp:subheader "Documentation")

(adp:itemize (adp:item @h(api-reference-header)))


(adp:subheader "A brief guide")

(adp:text "The simplest way to use " @f(with) " is like using " @l(let) " or " @l(multiple-value-bind) ":")

(adp:code-example
  (with ((x 5)
	 ((q r) (floor 45 32)))
    (+ x q r)))

(adp:text "But you can also bind functions:")

(adp:code-example
  (with ((hello (name)
                (format t "~%Hello ~a!" name)))
    (hello "there")))

(adp:text "Or nest other expressions. This can be useful when using other with- macros:")

(adp:code-block ()
  (with ((with-open-file (file "~/my-file.txt")))
    (print (read file))))

(adp:text "And, of course, you can mix it all up.")

(adp:code-example
  (with ((n 10)
         (add2 (a b)
               (+ a b))
         ((with-output-to-string (str))))
    (format str "The result is ~a" (add2 10 n))))


(adp:text "The bound variables are destroyed automatically at the end of WITH. More precisely, the generic function " @f(destroyer) " is called for almost all bound variables. Variables bound by a LET-like binding clause will be destroyed always. However, only the first bound variable within a MULTIPLE-VALUE-BIND-like form will be destroyed.")

(adp:code-block ()
  (with ((n 10)
         ((a b) (values 1 2)))
    (print n)))

(adp:text "Expands to:")

(adp:code-block ()
  (let* ((n 10))
    (unwind-protect
         (multiple-value-bind (a b)
             (values 1 2)
           (unwind-protect (progn (print n)) (destroyer a)))
      (progn (destroyer n)))))

(adp:text "Observe that only N and A are destroyed.")

(adp:text "The function " @f(destroyer) " is already defined for stream objects. In fact, this is the implementation of the method you can find in the source code.")

(adp:code-block ()
  (defmethod destroyer ((obj stream))
    "Closes a stream."
    (close obj)))
