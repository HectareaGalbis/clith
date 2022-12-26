
(in-package :clith)


(adp:in-file #P"README")


(adp:header "Common Lisp wITH")

(adp:text "Welcome to Clith!")

(adp:text "This library defines the macro WITH. It is like the 'with expression' in Python but better. It allows you to create some objects, bind them to some variables, evaluate some expressions using that variables, and lastly the objects are destroyed automatically.")

(adp:text "A well known example is opening a file, performing some writing or reading, and closing the file. To do this using the macro " @f(with) " we must define a with-destructor using " @f(define-with-destructor) ".")

(adp:code-example
 (define-with-destructor stream (some-stream)
   (print "Closing a stream")
   (close some-stream)))

(adp:text "Now we can use the " @f(with) " macro.")

(adp:code-example
 (with ((file-stream (open "~/some-file.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)))
   (print "Hello clith!" file-stream)))
