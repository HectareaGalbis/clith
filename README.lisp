
(in-package :clith)


(adp:in-file #P"README")


(adp:header "Common Lisp wITH")

(adp:text "Welcome to Clith!")

(adp:text "This library defines the macro " @f(with) ". It is like the 'with expression' in Python but better. It allows you to create some objects, bind them to some variables, evaluate some expressions using that variables, and lastly the objects are destroyed automatically.")

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


(adp:subheader "A brief introduction")

(adp:text "Let's see how we can create a 'with macro'. A well known example is the macro " @l(with-open-file) ". Let's define our own macro called " @f(with-my-open-file) ". It will do the same as " @l(with-open-file) " but it prints a message when the stream is about
to be closed.")

(adp:code-block (open-file-example)
  open-file-example)

(adp:text "The first argument must be a symbol denoting the suffix of our new macro. The next arguments are the constructor and the destructor. Finally we can add a docstring to our macro.")

(adp:text "The defined macro is the next one:")


(adp:code-tag (open-file-example)
  (defwith my-open-file #'open (lambda (&rest args)
				 (print "Closing the stream")
				 (apply #'close args))
    "Same as OPEN-FILE"))

(adp:text "Now we can use that macro to perform our reading or writing operations:")

(adp:code-example
  (with-my-open-file file-stream (#P"~/example.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format file-stream "Hello Clith!")))
