
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


(adp:subheader "A brief guide")

(adp:text "The simplest way to use " @f(with) " is like using " @l(let) " or " @l(multiple-value-bind) ":")

(adp:code-example
  (with ((x 5)
	 ((q r) (floor 45 32)))
    (+ x q r)))

(adp:text "However, the macro " @f(with) " allows you to destroy automatically the created objects after using these objects. This is intended mainly for using with the Common Lisp Foreign Function Interface (CFFI). The C language coninuously allocates and deallocates memory so a WITH macro can be very helpful.")


(adp:subsubheader "With and Defwith macros")

(adp:text "Suppose you have the following C functions:")

(adp:verbatim-code-block "C"
  "window* createWindow(char* name);

void destroyWindow(window* w);")

(adp:text "You write the following bindings:")

(adp:verbatim-code-block "Lisp"
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

  (defwith create-window #'create-window #'destroy-window))

(adp:text "The first argument must be a symbol denoting the 'with constructor name'. The following two arguments are the constructor and destructor used to create and destroy the object (in this case the window) when using the " @f(with) " macro.")

(adp:text "Now we can use " @f(with) ":")

(adp:code-block (with-window)

  with-window)

(adp:code-tag (with-window)
  @'((with ((window (create-window "A window")))
       (adp:code-comment
	   "Doing some stuff with the window"
	 (print-something window)))))


(adp:subsubheader "More about Defwith")

(adp:text "Surely a binding function like CREATE-WINDOW could receive and/or return multiple values. Also, some of these values must be used also in the destructor. For example, consider the following C functions and their respective bindings and wrappings:")

(adp:verbatim-code-block "C"
  "// The new window is set to the pointer whose address is stored in w.
// The window is created by the factory.
int createWindow(char* name, WindowFactory* factory, Window** w);

// The window must be destroyed by the same factory it was created.
void destroyWindow(window* w, WindowFactory* factory);")

(adp:verbatim-code-block "Lisp"
  "(cffi:defcfun \"createWindow\" :int
  (name :string) (factory :pointer) (w :pointer))

(cffi:defcfun \"destroyWindow\" :void
  (w :pointer) (factory :pointer))")

(adp:verbatim-code-block "Lisp"
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
  @'((defwith create-window

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

(adp:text "Now we can use the " @f(with) " macro as in the previous simple example. Also, as I said at the
beginning," @f(with) " support multiple bindings as if using " @l(multiple-value-bind) ":")

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

(adp:text "Finally, Clith already defines some 'with constructor names' like 'open', 'make-string-input-stream', etc.")

(adp:text "Here is an example where you can see what the " @f(with) " macro can do (yet there is more):")

(adp:code-block ()
  (defwith debug-with
      (lambda (value)
	(format "Constructor ~s" value)
	value)
    (lambda (value)
      (format "Destructor ~s" value)))

  (with (((debug-with 1))
	 ((debug-with 2))
	 (*special-var1* "A string")
	 (*special-var2* 1234)
	 ((window1 result) (create-window "A window" *factory*))
	 (window2 (create-window "Another window" *another-factory*)))
    (doing (a lot of (stuff)))
    (values "Hello Clith!" *special-var2*)))

(adp:verbatim-code-block "Text"
  "Constructor 1
Constructor 2
Destructor 2
Destructor 1
\"Hello Clith!\"
1234")


(adp:subsubheader "Define-with-expander macro")

(adp:text "Sometimes a constructor and a destructor is not enough. Maybe you want to enclose the body forms of
the " @f(with) " macro within a closure or a specific macro. Let's see how we can achieve this using the last macro of
this library: " @f(define-with-expander) ".")

(adp:text "Suppose you have a macro defined like this:")

(adp:code-example
  (defmacro with-special-bindings (&body body)
    `(let ((*animal* "Dog") (*speed* 8))
       (declare (special *animal* *speed*))
       ,@body)))

(adp:text "You can use this macro as expected:")

(adp:code-example
  (with-special-bindings
      (format t "The ~a goes at ~sKm/h speed."
	      *animal* *speed*)))

(adp:text "However you want to use this in the " @f(with) " macro. The solution goes to define a 'with expander'.")

(adp:code-example
  (define-with-expander special-bindings (&body body)
    `(let ((*animal* "Dog") (*speed* 8))
       (declare (special *animal* *speed*))
       ,@body)))

(adp:text "Done! Note that this macro works the same as " @l(defmacro) " (almost). Now we can use this in the
" @f(with) " macro:")

(adp:code-example
  (with (((special-bindings)))
    (format t "The ~a goes at ~sKm/h speed."
	    *animal* *speed*)))

(adp:text "Nice!")



