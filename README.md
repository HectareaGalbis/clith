

<a id="TITLE:CLITH-DOCS:TAG5"></a>
# Common Lisp wITH

Welcome to Clith\!

* [Introduction](/README.md#TITLE:CLITH-DOCS:TAG6)
* [Installation](/README.md#TITLE:CLITH-DOCS:TAG7)
* [Reference](/README.md#TITLE:CLITH-DOCS:TAG8)
* [Getting started](/README.md#TITLE:CLITH-DOCS:TAG9)
* [Defining a WITH expansion](/README.md#TITLE:CLITH-DOCS:TAG10)
  * [Simple example\: MAKE\-WINDOW](/README.md#TITLE:CLITH-DOCS:TAG11)
  * [No need to return a value\: INIT\-SUBSYSTEM](/README.md#TITLE:CLITH-DOCS:TAG12)
  * [Extended syntax\: GENSYMS](/README.md#TITLE:CLITH-DOCS:TAG13)
* [Documentation](/README.md#TITLE:CLITH-DOCS:TAG14)
* [Declarations](/README.md#TITLE:CLITH-DOCS:TAG15)


<a id="TITLE:CLITH-DOCS:TAG6"></a>
## Introduction

This library defines the macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH)\. This macro binds symbols to objects that can be finalized automatically in a personalized way\.

`````common-lisp
(with ((file (open "~/file.txt" :direction :output)))
  (print "Hello Clith!" file))
`````

[clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) is powerful enough to support almost every regular ```WITH-``` macro\:

`````common-lisp
(defwith slots (vars (object) body)
  `(with-slots ,vars ,object
     ,@body))

(defstruct 3d-vector x y z)

(with ((p (make-3d-vector :x 1 :y 2 :z 3))
       ((z (up y) x) (slots p)))
  (+ x up z))
`````
`````common-lisp
;; Returns
6
`````

It supports declarations\:

`````common-lisp
(with (((x y z) (values 1 2 3))
       ((a b c) (values 'a 'b 'c)))
  (declare (ignore a y c))
  (values x b z))
`````
`````common-lisp
;; Returns
1
B
3
`````

And it detects macros and symbol\-macros\:

`````common-lisp
(symbol-macrolet ((my-file (open "~/file.txt")))
  (with ((f my-file))
    (read f)))
`````
`````common-lisp
;; Returns
"Hello Clith!"
`````

<a id="TITLE:CLITH-DOCS:TAG7"></a>
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

<a id="TITLE:CLITH-DOCS:TAG8"></a>
## Reference

* [Reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:REFERENCE)


<a id="TITLE:CLITH-DOCS:TAG9"></a>
## Getting started

[clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) can be used as [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) or [multiple\-value\-bind](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

`````common-lisp
(with (x
       (y 3)
       ((q r) (floor 4 5)))
  (values x y q r))
`````
`````common-lisp
;; Returns
NIL
3
0
4
`````

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

Every Common Lisp function that creates an object that should be closed\/destroyed has a ```WITH expansion``` defined by ```CLITH```\. For example\, functions like [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm) or [make\-two\-way\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_two.htm) have a ```WITH expansion```\. See all the functions in the [reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-SYMBOLS)\.

Also\, we can check if a symbol denotes a ```WITH expansion``` using [clith\:withp](/docs/scribble/reference.md#FUNCTION:CLITH:WITHP)\:

`````common-lisp
(withp 'open)
`````
`````common-lisp
;; Returns
T
`````

<a id="TITLE:CLITH-DOCS:TAG10"></a>
## Defining a WITH expansion

<a id="TITLE:CLITH-DOCS:TAG11"></a>
### Simple example\: MAKE\-WINDOW

In order to extend the macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) we need to define a ```WITH expansion```\. To do so\, we use [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH:DEFWITH)\.

Suppose we have ```(MAKE-WINDOW TITLE)``` and ```(DESTROY-WINDOW WINDOW)```\. We want to control the expansion of WITH in order to use both functions\. Let\'s define the WITH expansion\:

`````common-lisp
(defwith make-window ((window) (title) body)
  "Makes a window that will be destroyed after the end of WITH."
  (let ((window-var (gensym)))
    `(let ((,window-var (make-window ,title)))
       (unwind-protect
           (let ((,window ,window-var))
             ,@body)
         (destroy-window ,window-var)))))
`````
`````common-lisp
;; Returns
MAKE-WINDOW
`````

This is a common implementation of a ```WITH-``` macro\. Note that we specified ```(window)``` to specify that only one variable is wanted\.

Now we can use our expansion\:

`````
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
`````

After the evaluation of the body\, ```my-window``` will be destroyed by ```destroy-window```\.

<a id="TITLE:CLITH-DOCS:TAG12"></a>
### No need to return a value\: INIT\-SUBSYSTEM

There are ```WITH-``` macros that doesn\'t return anything\. They just initialize something that should be finalized at the end\. Imagine that we have the functions ```INIT-SUBSYSTEM``` and ```FINALIZE-SUBSYSTEM```\. Let\'s define a ```WITH expansion``` that calls to ```FINALIZE-SUBSYSTEM```\:

`````common-lisp
(defwith init-subsystem (() () body) ; <- No variables to bind and no arguments.
  "Initialize the subsystem and finalize it at the end of WITH."
  `(progn
     (init-subsystem)
     (unwind-protect
         (progn ,@body)
       (finalize-subsystem))))
`````

Now we don\'t need to worry about finalizing the subsystem\:

`````common-lisp
(with (((init-subsystem)))
  ...)
`````

<a id="TITLE:CLITH-DOCS:TAG13"></a>
### Extended syntax\: GENSYMS

Some ```WITH-``` macros like [with\-slots](http://www.lispworks.com/reference/HyperSpec/Body/m_w_slts.htm) allow to specify some options to variables\. Let\'s try to make a ```WITH``` expansion that works like ```alexandria:with-gensyms```\. Each variable should optionally accept the prefix for the fresh generated symbol\.

We want to achieve something like this\:

`````common-lisp
(with ((sym1 (gensyms))                  ; <- Regular syntax
       ((sym2 (sym3 "FOO")) (gensyms)))  ; <- Extended syntax for SYM3
  ...)
`````

In order to do this\, we are using [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\:

`````common-lisp
(defwith gensyms (vars () body)
  (let* ((list-vars (mapcar #'alexandria:ensure-list vars))
         (sym-vars (mapcar #'car list-vars))
         (prefixes (mapcar #'cdr list-vars))
         (let-bindings (mapcar (lambda (sym-var prefix)
                                 `(,sym-var (gensym ,(if prefix (car prefix) (symbol-name sym-var)))))
                               sym-vars prefixes)))
    `(let ,let-bindings
       ,@body)))
`````
`````common-lisp
;; Returns
GENSYMS
`````

Each element in ```VARS``` can be a symbol or a list\. That\'s the reason we are using ```alexandria:ensure-list```\. ```LIST-VARS``` will contain lists where the first element is the symbol to bound and can have a second element\, the prefix\. We store then the symbols in ```SYM-VARS``` and the prefixes in ```PREFIXES```\. Note that if a prefix is not specified\, then the corresponding element in ```PREFIXES``` will be ```NIL```\. If some ```PREFIX``` is ```NIL```\, we use the name of the respective ```SYM-VAR```\. Finally\, we create the ```LET-BINDING``` and use it in the final form\.

Let\'s try it out\:

`````common-lisp
(with ((x (gensyms))
       ((y z) (gensyms))
       (((a "CUSTOM-A") (b "CUSTOM-B") c) (gensyms)))
  (values (list x y z a b c)))
`````
`````common-lisp
;; Returns
(#:X329 #:Y330 #:Z331 #:CUSTOM-A332 #:CUSTOM-B333 #:C334)
`````

<a id="TITLE:CLITH-DOCS:TAG14"></a>
## Documentation

The macro [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH:DEFWITH) accepts a docstring that can be retrieved with the function [documentation](http://www.lispworks.com/reference/HyperSpec/Body/f_docume.htm)\. Check out again the definition of the expansion of ```make-window``` above\. Note that we wrote a docstring\.

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


<a id="TITLE:CLITH-DOCS:TAG15"></a>
## Declarations

The macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) accepts declarations\. These declarations are moved to the correct place at expansion time\. For example\, imagine we want to open two windows\, but the variables can be ignored\:

`````common-lisp
(with ((w1 (make-window "Window 1"))
       (w2 (make-window "Window 2")))
  (declare (ignorable w1 w2))
  (print "Hello world!"))
`````

Let\'s see the expanded code\:

`````common-lisp
(macroexpand-1 '(with ((w1 (make-window "Window 1"))
                       (w2 (make-window "Window 2")))
                  (declare (ignorable w1 w2))
                  (print "Hello world!")))
`````
`````common-lisp
;; Returns
(LET ((#:G346 (MAKE-WINDOW "Window 1")))
  (UNWIND-PROTECT
      (LET ((W1 #:G346))
        (DECLARE (IGNORABLE W1))
        (LET ((#:G343 (MAKE-WINDOW "Window 2")))
          (UNWIND-PROTECT
              (LET ((W2 #:G343))
                (DECLARE (IGNORABLE W2))
                (PRINT "Hello world!"))
            (DESTROY-WINDOW #:G343))))
    (DESTROY-WINDOW #:G346)))
T
`````

Observe that the declarations are in the right place\. Every symbol that can be bound is a candidate for a declaration\. If more that one candidate is found \(same symbol appearing more than once\) the last one is selected\.