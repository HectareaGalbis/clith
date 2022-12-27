
(in-package :clith)


(adp:in-file #P"README")


(adp:header "Common Lisp wITH")

(adp:text "Welcome to Clith!")

(adp:text "This library defines the macro " @f(with) ". It is like the 'with expression' in Python but better. It allows you to create some objects, bind them to some variables, evaluate some expressions using that variables, and lastly the objects are destroyed automatically.")

(adp:text "A well known example is the macro " @l(with-open-file) ". Let's define our own macro called " @f(with-my-open-file) ". It will do the same as " @l(with-open-file) " but it prints a message when the stream is about
to be closed.")

(adp:code-example
  (defwith my-open-file #'open (lambda (&rest args)
				 (print "Closing the stream")
				 (apply #'close args))
    "Same as OPEN-FILE"))

(adp:text "The first argument must be a symbol denoting the suffix of our new macro. The next arguments are the constructor and the destructor. Finally we can add a docstring to our macro.")

(adp:text "Now we can use that macro to perform our reading or writing operations:")

(adp:code-example
  (with-my-open-file file-stream (#P"~/example.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format file-stream "Hello Clith!")))
