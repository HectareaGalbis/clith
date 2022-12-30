<h1 id="header:ADP:HEADERTAG0">Common Lisp wITH</h1>

Welcome to Clith\!

This library defines the macro <a href="/docs/api.md#function:CLITH:WITH">clith:with</a>\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using that variables\, and lastly the objects are destroyed automatically\.

* <a href="/README.md#header:ADP:HEADERTAG0">Common Lisp wITH</a>
  * <a href="/README.md#header:ADP:HEADERTAG1">Installation</a>
  * <a href="/README.md#header:ADP:HEADERTAG2">Documentation</a>
  * <a href="/README.md#header:ADP:HEADERTAG3">A brief guide</a>
    * <a href="/README.md#header:ADP:HEADERTAG4">With and Defwith macros</a>
    * <a href="/README.md#header:ADP:HEADERTAG5">More about Defwith</a>
    * <a href="/README.md#header:ADP:HEADERTAG6">Define-with-expander macro</a>

<h2 id="header:ADP:HEADERTAG1">Installation</h2>

This library is available at Ultralisp\. If you don\'t have it already\, you can add it evaluating this\:

`````Lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
			 :prompt nil)
`````

After this you can install Clith using Quicklisp\:

`````Lisp
(ql:quickload :clith)
`````

<h2 id="header:ADP:HEADERTAG2">Documentation</h2>

* <a href="/docs/api.md#header:CLITH:API-REFERENCE-HEADER">Clith API reference</a>

<h2 id="header:ADP:HEADERTAG3">A brief guide</h2>

The simplest way to use <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> is like using [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) or [multiple\-value\-bind](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

```Lisp
(clith:with ((x 5) ((q r) (floor 45 32)))
  (+ x q r))

19
```

However\, the macro <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> allows you to destroy automatically the created objects after using these objects\. This is intended mainly for using with the Common Lisp Foreign Function Interface \(CFFI\)\. The C language coninuously allocates and deallocates memory so a WITH macro can be very helpful\.

<h3 id="header:ADP:HEADERTAG4">With and Defwith macros</h3>

Suppose you have the following C functions\:

`````C
window* createWindow(char* name);

void destroyWindow(window* w);
`````

You write the following bindings\:

`````Lisp
(cffi:defcfun "createWindow" :pointer
  (name :string))

(cffi:defcfun "destroyWindow" :void
  (w :pointer))
`````

And you make the following wrapping\:

`````Lisp
(defun create-window (name) (createwindow name))

(defun destroy-window (w) (destroywindow w))
`````

The usual way to work with this functions is\:

`````Lisp
(let ((window (create-window "A window")))
  ;; Doing some stuff with the window
  (print-something window)
  ;; Closing the window
  (destroy-window w))
`````

You can forget about closing the window\, so we should use the <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro\. First\, we need to define a \'with constructor name\' using <a href="/docs/api.md#function:CLITH:DEFWITH">clith:defwith</a>\.

`````Lisp
(clith:defwith create-window #'create-window #'destroy-window)
`````

The first argument must be a symbol denoting the \'with constructor name\'\. The following two arguments are the constructor and destructor used to create and destroy the object \(in this case the window\) when using the <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro\.

Now we can use <a href="/docs/api.md#function:CLITH:WITH">clith:with</a>\:

`````Lisp
(clith:with ((window (create-window "A window")))
  ;; Doing some stuff with the window
  (print-something window))
`````

<h3 id="header:ADP:HEADERTAG5">More about Defwith</h3>

Surely a binding function like CREATE\-WINDOW could receive and\/or return multiple values\. Also\, some of these values must be used also in the destructor\. For example\, consider the following C functions and their respective bindings and wrappings\:

`````C
// The new window is set to the pointer whose address is stored in w.
// The window is created by the factory.
int createWindow(char* name, WindowFactory* factory, Window** w);

// The window must be destroyed by the same factory it was created.
void destroyWindow(window* w, WindowFactory* factory);
`````

`````Lisp
(cffi:defcfun "createWindow" :int
  (name :string) (factory :pointer) (w :pointer))

(cffi:defcfun "destroyWindow" :void
  (w :pointer) (factory :pointer))
`````

`````Lisp
(defun create-window (name factory)
  (cffi:with-foreign-object (pWindow :pointer)
    (let ((result (createWindow name factory pWindow)))
      (values (cffi:mem-ref pWindow :pointer)
              result))))

(defun destroy-window (w factory)
  (destroyWindow w factory))
`````

Now suppose we have the pointer to a factory stored in the parameter \*factory\*\. The usual way to work with a window could be\:

`````Lisp
(multiple-value-bind (window result)
    (create-window "A window" *factory*)
  ;; Checking if creation was succesful
  (unless (equal result 'ok) (error "Window creation failed!"))
  ;; Doing some stuff with the window
  (print-something window)
  ;; Closing the window
  (destroy-window window *factory*))
`````

Let\'s make a \'with constructor name\'\. The destructor must receive the window and the ``` *factory* ```\. In order to achieve that\, the constructor must return all the values the destructor needs\.

`````Lisp
(clith:defwith create-window
               ;; We make a constructor that uses CREATE-WINDOW
               (lambda (name factory)
                 (multiple-value-bind (window result)
                     (create-window name factory)
                   ;; We return the same as the constructor plus the values the destructor needs.
                   (values window result factory)))
               ;; The destructor must receive all the values returned by the constructor.
               (lambda (window result factory)
                 (declare (ignore result))
                 (destroy-window window factory)))
`````

Now we can use the <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro as in the previous simple example\. Also\, as I said at the
beginning\,<a href="/docs/api.md#function:CLITH:WITH">clith:with</a> support multiple bindings as if using [multiple\-value\-bind](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

`````Lisp
(clith:with (((window result) (create-window "A window" *factory*)))
  ;; Checking if creation was succesful
  (unless (equal result 'ok) (error "Window creation failed!"))
  ;; Doing some stuff with the window
  (print-something window))
`````

Much better\!

Finally\, Clith already defines some \'with constructor names\' like \'open\'\, \'make\-string\-input\-stream\'\, etc\.

Here is an example where you can see what the <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro can do \(yet there is more\)\:

`````Lisp
(clith:defwith debug-with
               (lambda (value) (format "Constructor ~s" value) value)
               (lambda (value) (format "Destructor ~s" value)))

(clith:with (((debug-with 1)) ((debug-with 2)) (*special-var1* "A string")
             (*special-var2* 1234)
             ((window1 result) (create-window "A window" *factory*))
             (window2 (create-window "Another window" *another-factory*)))
  (doing (a lot of (stuff)))
  (values "Hello Clith!" *special-var2*))
`````

`````Text
Constructor 1
Constructor 2
Destructor 2
Destructor 1
"Hello Clith!"
1234
`````

<h3 id="header:ADP:HEADERTAG6">Define-with-expander macro</h3>

Sometimes a constructor and a destructor is not enough\. Maybe you want to enclose the body forms of
the <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro within a closure or a specific macro\. Let\'s see how we can achieve this using the last macro of
this library\: <a href="/docs/api.md#function:CLITH:DEFINE-WITH-EXPANDER">clith:define-with-expander</a>\.

Suppose you have a macro defined like this\:

```Lisp
(defmacro with-special-bindings (&body body)
  `(let ((*animal* "Dog") (*speed* 8))
     (declare (special *animal* *speed*))
     ,@body))

clith::with-special-bindings
```

You can use this macro as expected\:

```Lisp
(with-special-bindings
  (format t "The ~a goes at ~sKm/h speed." *animal* *speed*))
The Dog goes at 8Km/h speed.
nil
```

However you want to use this in the <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro\. The solution goes to define a \'with expander\'\.

```Lisp
(clith:define-with-expander special-bindings
    (&body body)
  `(let ((*animal* "Dog") (*speed* 8))
     (declare (special *animal* *speed*))
     ,@body))

clith::special-bindings
```

Done\! Note that this macro works the same as [defmacro](http://www.lispworks.com/reference/HyperSpec/Body/m_defmac.htm) \(almost\)\. Now we can use this in the
<a href="/docs/api.md#function:CLITH:WITH">clith:with</a> macro\:

```Lisp
(clith:with (((special-bindings)))
  (format t "The ~a goes at ~sKm/h speed." *animal* *speed*))
The Dog goes at 8Km/h speed.
nil
```

Nice\!

