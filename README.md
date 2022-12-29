<h1 id="header:ADP:HEADERTAG0">Common Lisp wITH</h1>

Welcome to Clith\!

This library defines the macro <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a>\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using that variables\, and lastly the objects are destroyed automatically\.

* <a href="/README.md#header:ADP:HEADERTAG0">Common Lisp wITH</a>
  * <a href="/README.md#header:ADP:HEADERTAG1">Installation</a>
  * <a href="/README.md#header:ADP:HEADERTAG2">Documentation</a>
  * <a href="/README.md#header:ADP:HEADERTAG3">A brief guide</a>
    * <a href="/README.md#header:ADP:HEADERTAG4">A simple example</a>
    * <a href="/README.md#header:ADP:HEADERTAG5">A more realistic example</a>

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

The simplest way to use <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> is like using [LET](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) or [MULTIPLE\-VALUE\-BIND](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

```Lisp
(CLITH:WITH ((X 5) ((Q R) (FLOOR 45 32)))
  (+ X Q R))

19
```

However\, the macro <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> allows you to destroy automatically the created objects after using these objects\. This is intended mainly for using with the Common Lisp Foreign Function Interface \(CFFI\)\. The C language coninuously allocates and deallocates memory so a WITH macro can be very helpful\.

<h3 id="header:ADP:HEADERTAG4">A simple example</h3>

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
(DEFUN CREATE-WINDOW (NAME) (CREATEWINDOW NAME))

(DEFUN DESTROY-WINDOW (W) (DESTROYWINDOW W))
`````

The usual way to work with this functions is\:

`````Lisp
(LET ((WINDOW (CREATE-WINDOW "A window")))
  ;; Doing some stuff with the window
  (PRINT-SOMETHING WINDOW)
  ;; Closing the window
  (DESTROY-WINDOW W))
`````

You can forget about closing the window\, so we should use the <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> macro\. First\, we need to define a \'with constructor name\' using <a href="/docs/api.md#function:CLITH:DEFWITH">CLITH:DEFWITH</a>\.

`````Lisp
(CLITH:DEFWITH 'CREATE-WINDOW #'CREATE-WINDOW #'DESTROY-WINDOW)
`````

The first argument must be a symbol denoting the \'with constructor name\'\. The following two arguments are the constructor and destructor used to create and destroy the object \(in this case the window\) when using the <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> macro\.

Now we can use <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a>\:

`````Lisp
(CLITH:WITH ((WINDOW (CREATE-WINDOW "A window")))
  ;; Doing some stuff with the window
  (PRINT-SOMETHING WINDOW))
`````

<h3 id="header:ADP:HEADERTAG5">A more realistic example</h3>

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
(MULTIPLE-VALUE-BIND (WINDOW RESULT)
    (CREATE-WINDOW "A window" *FACTORY*)
  ;; Checking if creation was succesful
  (UNLESS (EQUAL RESULT 'OK) (ERROR "Window creation failed!"))
  ;; Doing some stuff with the window
  (PRINT-SOMETHING WINDOW)
  ;; Closing the window
  (DESTROY-WINDOW WINDOW *FACTORY*))
`````

Let\'s make a \'with constructor name\'\. The destructor must receive the window and the ``` *factory* ```\. In order to achieve that\, the constructor must return all the values the destructor needs\.

`````Lisp
(CLITH:DEFWITH 'CREATE-WINDOW
               ;; We make a constructor that uses CREATE-WINDOW
               (LAMBDA (NAME FACTORY)
                 (MULTIPLE-VALUE-BIND (WINDOW RESULT)
                     (CREATE-WINDOW NAME FACTORY)
                   ;; We return the same as the constructor plus the values the destructor needs.
                   (VALUES WINDOW RESULT FACTORY)))
               ;; The destructor must receive all the values returned by the constructor.
               (LAMBDA (WINDOW RESULT FACTORY)
                 (DECLARE (IGNORE RESULT))
                 (DESTROY-WINDOW WINDOW FACTORY)))
`````

Now we can use the <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> macro as in the previous simple example\. Also\, as I said at the
beginning\,<a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> support multiple bindings as if using [MULTIPLE\-VALUE\-BIND](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

`````Lisp
(CLITH:WITH (((WINDOW RESULT) (CREATE-WINDOW "A window" *FACTORY*)))
  ;; Checking if creation was succesful
  (UNLESS (EQUAL RESULT 'OK) (ERROR "Window creation failed!"))
  ;; Doing some stuff with the window
  (PRINT-SOMETHING WINDOW))
`````

Much better\!

Finally\, Clith already defines some \'with constructor names\' like \'open\'\, \'make\-string\-input\-stream\'\, etc\. Here is a last example\:

`````Lisp
(CLITH:DEFWITH 'DEBUG-WITH
               (LAMBDA (VALUE) (FORMAT "Constructor ~s" VALUE) VALUE)
               (LAMBDA (VALUE) (FORMAT "Destructor ~s" VALUE)))

(CLITH:WITH (((DEBUG-WITH 1)) ((DEBUG-WITH 2)) (*SPECIAL-VAR1* "A string")
             (*SPECIAL-VAR2* 1234)
             ((WINDOW1 RESULT) (CREATE-WINDOW "A window" *FACTORY*))
             (WINDOW2 (CREATE-WINDOW "Another window" *ANOTHER-FACTORY*)))
  (DOING (A LOT OF (STUFF)))
  (VALUES "Hello Clith!" *SPECIAL-VAR2*))
`````

`````Text
Constructor 1
Constructor 2
Destructor 2
Destructor 1
"Hello Clith!"
1234
`````

Consider reading the <a href="/docs/api.md#header:CLITH:API-REFERENCE-HEADER">Clith API reference</a> for more information about how these macros work\.

