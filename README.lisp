
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

(adp:text "The macro " @f(with) " is used to destroy automatically the created objects after using them. This is intended mainly for using with the Common Lisp Foreign Function Interface (CFFI). The C language coninuously allocates and deallocates memory so a WITH macro can be very helpful.")


(adp:subsubheader "A simple example")

(adp:text "Suppose you have the following C functions:")

(adp:verbatim-code-block "C"
  "window* createWindow(char* name);

void destroyWindow(window* w);")

(adp:text "You write the following bindings:")

(adp:verbatim-code-block "CommonLisp"
  "(cffi:defcfun \"createWindow\" :pointer
  (name :string))

(cffi:defcfun \"destroyWindow\" :void
  (w :pointer))")

(adp:text "And you make the following wrapping:")

(adp:code-block ()

  (defun create-window (name)
    (createWindow name))

  (defun destroy-window (w)
    (destroyWindow w)))

(adp:text "The usual way to work with this functions is:")

(adp:code-block (let-window)

  let-window)

(adp:code-tag (let-window)
  @'((let ((window (create-window "A window")))
       (adp:code-comment
	   "Doing some stuff with the window"
	 (print-something window))
       (adp:code-comment
	   "Closing the window"
	 (destroy-window w)))))

(adp:text "You can forget about closing the window, so we should use the " @f(with) " macro. First, we need to define a 'with constructor name' using " @f(defwith) ".")

(adp:code-block ()

  (defwith 'create-window #'create-window #'destroy-window))

(adp:text "The first argument must be a symbol denoting the 'with constructor name'. The following two arguments are the constructor and destructor used to create and destroy the object (in this case the window) when using the " @f(with) " macro.")

(adp:text "Now we can use " @f(with) ":")

(adp:code-block (with-window)

  with-window)

(adp:code-tag (with-window)
  @'((with ((window (create-window "A window")))
       (adp:code-comment
	   "Doing some stuff with the window"
	 (print-something window)))))


(adp:subsubheader "A more realistic example")

(adp:text "Surely a binding function like CREATE-WINDOW could receive and/or return multiple values. Also, some of these values must be used also in the destructor. For example, consider the following C functions and their respective bindings and wrappings:")

(adp:verbatim-code-block "C"
  "// The new window is set to the pointer whose address is stored in w.
// The window is created by the factory.
int createWindow(char* name, WindowFactory* factory, Window** w);

// The window must be destroyed by the same factory it was created.
void destroyWindow(window* w, WindowFactory* factory);")

(adp:verbatim-code-block "CommonLisp"
  "(cffi:defcfun \"createWindow\" :int
  (name :string) (factory :pointer) (w :pointer))

(cffi:defcfun \"destroyWindow\" :void
  (w :pointer) (factory :pointer))")

(adp:verbatim-code-block "CommonLisp"
  "(defun create-window (name factory)
  (cffi:with-foreign-object (pWindow :pointer)
    (let ((result (createWindow name factory pWindow)))
      (values (cffi:mem-ref pWindow :pointer)
              result))))

(defun destroy-window (w factory)
  (destroyWindow w factory))")


(adp:text "Now suppose we have the pointer to a factory stored in the parameter *factory*. The usual way to work with a window could be:")

(adp:code-block (complex-let-window)
  complex-let-window)

(adp:code-tag (complex-let-window)
  @'((multiple-value-bind (window result) (create-window "A window" *factory*)
       (adp:code-comment
	   "Checking if creation was succesful"
	 (unless (equal result 'ok)
	   (error "Window creation failed!")))
       (adp:code-comment
	   "Doing some stuff with the window"
	 (print-something window))
       (adp:code-comment
	   "Closing the window"
	 (destroy-window window *factory*)))))

(adp:text "Let's make a 'with constructor name'. The destructor must receive the window and the " @c("*factory*") ". In order to achieve that, the constructor must return all the values the destructor needs.")

(adp:code-block (complex-defwith)
  complex-defwith)

(adp:code-tag (complex-defwith)
  @'((defwith 'create-window

      (adp:code-comment
	  "We make a constructor that uses CREATE-WINDOW"
	(lambda (name factory)
	  (multiple-value-bind (window result) (create-window name factory)
	    (adp:code-comment
		"We return the same as the constructor plus the values the destructor needs."
	      (values window result factory)))))

      (adp:code-comment
	  "The destructor must receive all the values returned by the constructor."
	(lambda (window result factory)
	  (declare (ignore result))
	  (destroy-window window factory))))))

(adp:text "Now we can use the " @f(with) " macro as in the previous simple example. Also, " @f(with) " support multiple bindings as if using " @l(multiple-value-bind) ":")

(adp:code-block (complex-with-window)

  complex-with-window)

(adp:code-tag (complex-with-window)
  @'((with (((window result) (create-window "A window" *factory*)))
       (adp:code-comment
	   "Checking if creation was succesful"
	 (unless (equal result 'ok)
	   (error "Window creation failed!")))
       (adp:code-comment
	   "Doing some stuff with the window"
	 (print-something window)))))

(adp:text "Much better!")

(adp:text "Finally, I must say that " @f(with) " already defines some 'with constructor names' like values, open, make-string-input-stream, etc. Here is a last example:")

(adp:code-block ()
  (defwith 'debug-with
      (lambda (value)
	(format "Constructor ~s" value)
	value)
    (lambda (value)
      (format "Destructor ~s" value)))

  (with (((debug-with 1))
	 ((debug-with 2))
	 ((*special-var1* *special-var2*) (values "A string" 1234))
	 ((window1 result) (create-window "A window" *factory*))
	 (window2 (create-window "Another window" *another-factory*)))
    (doing (a lot of (stuff)))
    (values "Hello Clith!" *special-var2*)))

(adp:verbatim-code-block "CommonLisp"
  "Constructor 1
Constructor 2
Destructor 2
Destructor 1
\"Hello Clith!\"
1234")

(adp:text "Consider reading the " @h(api-reference-header) " for more information about how these macros work.")
