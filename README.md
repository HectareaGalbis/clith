

<a id="TITLE:CLITH-DOCS:TAG64"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH)\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\. Once the process is complete\, the objects are automatically destroyed\.

* [Installation](/README.md#TITLE:CLITH-DOCS:TAG65)
* [Reference](/README.md#TITLE:CLITH-DOCS:TAG66)
* [Getting started](/README.md#TITLE:CLITH-DOCS:TAG67)
* [Defining a WITH expansion](/README.md#TITLE:CLITH-DOCS:TAG68)
* [Expansion\'s documentation](/README.md#TITLE:CLITH-DOCS:TAG69)
* [Declarations](/README.md#TITLE:CLITH-DOCS:TAG70)


<a id="TITLE:CLITH-DOCS:TAG65"></a>
## Installation

* Manual\:

`````sh
cd ~/common-lisp
git clone https://github.com/Hectarea1996/clith.git
`````
* Quicklisp\:

`````common-lisp
(ql:quickload "clith")
`````

<a id="TITLE:CLITH-DOCS:TAG66"></a>
## Reference

* [Reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:REFERENCE)


<a id="TITLE:CLITH-DOCS:TAG67"></a>
## Getting started

The macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) uses ```WITH expansions``` in a similar way to ```setf```\. These expansions control how the macro  [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) is expanded\.

`````common-lisp
(let (some-stream)

  (with ((the-stream (open "~/test.txt")))
    (setf some-stream the-stream)
    (format t "Stream opened? ~s~%" (open-stream-p some-stream)))

  (format t "Stream opened after? ~s" (open-stream-p some-stream)))
`````
`````text
;; Output
Stream opened? T
Stream opened after? NIL
`````
`````common-lisp
;; Returns
NIL
`````

Every Common Lisp function that creates an object that should be closed\/destroyed at the end has a ```WITH expansion``` defined by ```CLITH```\. For example\, functions like [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm) or [make\-two\-way\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_two.htm) have a ```WITH expansion```\. See all the functions in the [reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-SYMBOLS)\.

Also\, we can check if a symbol denotes a ```WITH expansion``` using [clith\:withp](/docs/scribble/reference.md#FUNCTION:CLITH:WITHP)\:

`````common-lisp
(withp 'open)
`````
`````common-lisp
;; Returns
T
`````

<a id="TITLE:CLITH-DOCS:TAG68"></a>
## Defining a WITH expansion

In order to extend the macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) we need to define a ```WITH expansion```\. To do so\, we use [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH:DEFWITH)\.

Suppose we have ```(MAKE-WINDOW TITLE)``` and ```(DESTROY-WINDOW WINDOW)```\. We want to control the expansion of WITH in order to use both functions\. Let\'s define the WITH expansion\:

`````common-lisp
(defwith make-window ((window) (title) body)
  "Makes a window that will be destroyed after the end of WITH."
  (let ((window-var (gensym)))
    `(let ((,window-var (make-window ,title)))
       (let ((,window ,window-var))
         (unwind-protect
             (progn ,@body)
           (destroy-window ,window-var))))))
`````
`````common-lisp
;; Returns
MAKE-WINDOW
`````

This is a common implementation of a \'with\-\' macro\. Note that we specified ```(window)``` to specify that only one variable is wanted\.

Now we can use our expansion\:

`````
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
`````

After the evaluation of the body\, ```my-window``` will be destroyed by ```destroy-window```\.

<a id="TITLE:CLITH-DOCS:TAG69"></a>
## Expansion\'s documentation

The macro [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH:DEFWITH) accepts a docstring that can be retrieved with the function ```documentation```\. Check out again the definition of the expansion of ```make-window``` above\. Note that we wrote a docstring\.

`````common-lisp
(documentation 'make-window 'with)
`````
`````common-lisp
;; Returns
"Makes a window that will be destroyed after the end of WITH."
`````

We can also ```setf``` the docstring\:

`````common-lisp
(setf (documentation 'make-window 'with) "Another docstring!")
(documentation 'make-window 'with)
`````
`````common-lisp
;; Returns
"Another docstring!"
`````


<a id="TITLE:CLITH-DOCS:TAG70"></a>
## Declarations

The macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) accepts declarations\. These declarations are moved to the correct place at expansion time\. For example\, imagine we want to open two windows\, but only one variable will be used\. The other one must be ignored\:

`````common-lisp
(with ((w1 (make-window "Window 1"))
       (w2 (make-window "Window 2")))
  (declare (ignore w1))
  (print "Hello world!")
)
`````

Let\'s see the expanded code\:

`````common-lisp
(macroexpand-1 '(with ((w1 (make-window "Window 1"))
                       (w2 (make-window "Window 2")))
                  (declare (ignore w1))
                  (print "Hello world!")))
`````
`````common-lisp
;; Returns
(LET ((#:G382 (MAKE-WINDOW "Window 1")))
  (LET ((W1 #:G382))
    (UNWIND-PROTECT
        (PROGN
         (DECLARE (IGNORE W1))
         (LET ((#:G380 (MAKE-WINDOW "Window 2")))
           (LET ((W2 #:G380))
             (UNWIND-PROTECT (PROGN (PRINT "Hello world!"))
               (DESTROY-WINDOW #:G380)))))
      (DESTROY-WINDOW #:G382))))
T
`````

Observe that the declaration is in the right place\. Every symbol that can be bound is a candidate for a declaration\. If more that one candidate is found \(same symbol appearing more than once\) the last one is selected\.