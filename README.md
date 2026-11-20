

<a id="TITLE:ADP-GITHUB-WRITING:TAG1"></a>
# Common Lisp wITH

Welcome to Clith\!

* [Introduction](/README.md#TITLE:ADP-GITHUB-WRITING:TAG2)
* [Installation](/README.md#TITLE:ADP-GITHUB-WRITING:TAG3)
* [Getting started](/README.md#TITLE:ADP-GITHUB-WRITING:TAG4)
* [Defining a WITH expansion](/README.md#TITLE:ADP-GITHUB-WRITING:TAG5)
  * [Simple example\: MAKE\-WINDOW](/README.md#TITLE:ADP-GITHUB-WRITING:TAG6)
  * [No need to return a value\: INIT\-SUBSYSTEM](/README.md#TITLE:ADP-GITHUB-WRITING:TAG7)
  * [Extended syntax\: GENSYMS](/README.md#TITLE:ADP-GITHUB-WRITING:TAG8)
* [Documentation](/README.md#TITLE:ADP-GITHUB-WRITING:TAG9)
* [Declarations](/README.md#TITLE:ADP-GITHUB-WRITING:TAG10)
* [Built\-in WITH expansions](/README.md#TITLE:CLITH-DOCS:CL-SYMBOLS)
* [Reference](/README.md#TITLE:ADP-GITHUB-WRITING:TAG11)


<a id="TITLE:ADP-GITHUB-WRITING:TAG2"></a>
## Introduction

This library defines the macro [clith\:with](/README.md#FUNCTION:CLITH:WITH)\.

This macro aims to encapsulate every kind of ```WITH-``` macro into one\.

`````common-lisp
(with ((file (open "~/file.txt" :direction :output)))
  (print "Hello Clith!" file))
`````

[clith\:with](/README.md#FUNCTION:CLITH:WITH) is powerful enough to support almost every ```WITH-``` macro\:

`````common-lisp
(defwith slots (vars body object)
  `(with-slots ,vars ,object
     ,@body))

(defstruct 3d-vector x y z)

(let ((p (make-3d-vector :x 1 :y 2 :z 3)))
  (with (((z (up y) x) (slots p)))
    (+ x up z)))
`````
`````common-lisp
;; Returns
6
`````

It supports declarations\:

`````common-lisp
(let ((p (make-3d-vector :x 1 :y 2 :z 3)))
  (with (((x y z) (slots p)))
    (declare (ignore x z))
    (values y)))
`````
`````common-lisp
;; Returns
2
`````

And it detects macros and symbol\-macros\:

`````common-lisp
(symbol-macrolet ((my-file (open "~/file.txt")))
  (with ((f my-file))
    (read f)))
`````
`````common-lisp
;; Returns
"Hola mundo"
`````

<a id="TITLE:ADP-GITHUB-WRITING:TAG3"></a>
## Installation

* Manual\:

`````sh
cd ~/common-lisp
git clone https://github.com/HectareaGalbis/clith.git
`````
* Quicklisp\:

`````common-lisp
(ql:quickload "clith")
`````

<a id="TITLE:ADP-GITHUB-WRITING:TAG4"></a>
## Getting started

The macro [clith\:with](/README.md#FUNCTION:CLITH:WITH) uses ```WITH expansions``` in a similar way to ```setf```\. These expansions control how this macro is expanded\.

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

Every Common Lisp function that creates an object that should be closed\/destroyed has a ```WITH expansion``` defined by ```CLITH```\. For example\, functions like [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm) or [make\-two\-way\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_two.htm) have a ```WITH expansion```\. See all the functions in the [reference](/README.md#TITLE:CLITH-DOCS:CL-SYMBOLS)\.

Also\, we can check if a symbol denotes a ```WITH expansion``` using [clith\:withp](/README.md#FUNCTION:CLITH:WITHP)\:

`````common-lisp
(withp 'open)
`````
`````common-lisp
;; Returns
T
`````

<a id="TITLE:ADP-GITHUB-WRITING:TAG5"></a>
## Defining a WITH expansion

<a id="TITLE:ADP-GITHUB-WRITING:TAG6"></a>
### Simple example\: MAKE\-WINDOW

In order to extend the macro [clith\:with](/README.md#FUNCTION:CLITH:WITH) we need to define a ```WITH expansion```\. To do so\, we use [clith\:defwith](/README.md#FUNCTION:CLITH:DEFWITH)\.

Suppose we have ```(MAKE-WINDOW TITLE)``` and ```(DESTROY-WINDOW WINDOW)```\. We want to control the expansion of [clith\:with](/README.md#FUNCTION:CLITH:WITH) in order to use both functions\. Let\'s define the WITH expansion\:

`````common-lisp
(defwith make-window ((window) body title)
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

<a id="TITLE:ADP-GITHUB-WRITING:TAG7"></a>
### No need to return a value\: INIT\-SUBSYSTEM

There are ```WITH-``` macros that doesn\'t return anything\. They just initialize something that should be finalized at the end\. Imagine that we have the functions ```INIT-SUBSYSTEM``` and ```FINALIZE-SUBSYSTEM```\. Let\'s define a ```WITH expansion``` that calls to ```FINALIZE-SUBSYSTEM```\:

`````common-lisp
(defwith init-subsystem (() body) ; <- No variables to bind and no arguments.
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

<a id="TITLE:ADP-GITHUB-WRITING:TAG8"></a>
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
(defwith gensyms (vars body)
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
(#:X339 #:Y340 #:Z341 #:CUSTOM-A342 #:CUSTOM-B343 #:C344)
`````

<a id="TITLE:ADP-GITHUB-WRITING:TAG9"></a>
## Documentation

The macro [clith\:defwith](/README.md#FUNCTION:CLITH:DEFWITH) accepts a docstring that can be retrieved with the function [documentation](http://www.lispworks.com/reference/HyperSpec/Body/f_docume.htm)\. Check out again the definition of the expansion of ```make-window``` above\. Note that we wrote a docstring\.

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


<a id="TITLE:ADP-GITHUB-WRITING:TAG10"></a>
## Declarations

The macro [clith\:with](/README.md#FUNCTION:CLITH:WITH) accepts declarations\. These declarations are moved to the correct place at expansion time\. For example\, imagine we want to open two windows\, but the variables can be ignored\:

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
(LET ((#:G359 (MAKE-WINDOW "Window 1")))
  (UNWIND-PROTECT
      (LET ((W1 #:G359))
        (DECLARE (IGNORABLE W1))
        (LET ((#:G358 (MAKE-WINDOW "Window 2")))
          (UNWIND-PROTECT
              (LET ((W2 #:G358))
                (DECLARE (IGNORABLE W2))
                (PRINT "Hello world!"))
            (DESTROY-WINDOW #:G358))))
    (DESTROY-WINDOW #:G359)))
T
`````

Observe that the declarations are in the right place\. Every symbol that can be bound is a candidate for a declaration\. If more that one candidate is found \(same symbol appearing more than once\) the last one is selected\.


<a id="TITLE:CLITH-DOCS:CL-SYMBOLS"></a>
## Built\-in WITH expansions

The following Common Lisp functions have a ```WITH expansion```\:

* ```make-broadcast-stream```
* ```make-concatenated-stream```
* ```make-echo-stream```
* ```make-string-input-stream```
* ```make-string-output-stream```
* ```make-synonym-stream```
* ```make-two-way-stream```
* ```open```


<a id="TITLE:ADP-GITHUB-WRITING:TAG11"></a>
## Reference

<a id="FUNCTION:CLITH:DEFWITH"></a>
<a id="FUNCTION:ADP-GITHUB-WRITING:TAG12"></a>
#### Macro: clith\:defwith \(name args \&body body\)

`````text
Define a WITH expansion. A WITH expansion controls how the macro WITH is expanded. This macro has
the following syntax:

  (DEFWITH name ([&whole whole-var] [&environment env-var] vars with-body . args)
    [docstring] declaration* body*)

  name              ::= symbol
  args              ::= rest of a macro-lambda-list
  body              ::= form

When using (NAME ARGS*) inside the macro WITH, it will expand to the value returned by DEFWITH.
The lambda list must indicate at least 2 required arguments, followed by the arguments ARGS:
  1. The list of variables to bound. Each element of the list can have the form {var | (var var-option*)} where
     var is a symbol and var-option can be any form.
  2. The body of the WITH macro.
Keep in mind that the body can start with declarations. These are the declarations written in WITH that
apply to the variables to bound, so the expansion should place the body where declarations are allowed.

If &whole is used, WHOLE-VAR is bound to the form written by the user, i.e. (NAME ARGS*), without the
variables and the body. If that form was obtained by expanding a macro or symbol-macro, WHOLE-VAR
is bound to the expanded form. If &environment is used, ENV-VAR is bound to the environment of WITH.

As an example, let's define the with expansion MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

  (defwith my-file ((stream) body filespec &rest options)
    "Open a file."
    `(with-open-file (,stream ,filespec ,@options)
       ,@body))

In this example, as VARS is always a list, we can use destructuring to retrieve directly the variable to bound.
Also, we are assuming here that no additional options are passed with the variable.

Now, using WITH:

  (with ((file (my-file "~/file.txt" :direction :output)))
    (print "Hey!" file))

Finally, note that we put a docstring when we defined MY-FILE. We can retrieve it with DOCUMENTATION:

  (documentation 'my-file 'with)  ;; --> "Open a file."
`````

<a id="FUNCTION:CLITH:WITH"></a>
<a id="FUNCTION:ADP-GITHUB-WRITING:TAG13"></a>
#### Macro: clith\:with \(bindings \&body body\)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] form)
  vars             ::= symbol | (var-with-options*)
  var-with-options ::= symbol | (symbol var-option*)
  var-option       ::= form

WITH accepts a list of binding clauses. Each binding clause can be a symbol or a list. Depending on
this, the behaeviour of WITH is slightly different:

  - A list with one element. That element must be a WITH expansion. The expansion is expanded
     according to DEFWITH. In this case, the WITH expansion will receive NIL as the list of variables to bound.

      (with (((init-video-system)))  ; Possible expansion that should finalize the video system at the end
        ;; Doing video stuff
        )

  - A list with two elements: The first element must be a symbol or a list of symbols with
    or without options. The second element must be a WITH expansion:

      (with ((my-file (open "~/my-file.txt")))  ; Expanded to WITH-OPEN-FILE
        ...)

Each variable in a binding clause can have options. These options should be used inside DEFWITH
 to control the expansion with better precision:

      (defwith slots (vars (object) body)
        `(with-slots ,vars ,object
           ,@body))

      (defstruct 3d-vector x y z)

      (with ((v (make-3d-vector :x 1 :y 2 :z 3))
             ((x (up y) z) (slots v)))
        (+ x up z))

Macros and symbol-macros are treated specially. If a macro or symbol-macro is used, they
will be expanded with MACROEXPAND-1 and its result must be a WITH expansion.
`````

<a id="FUNCTION:CLITH:WITHP"></a>
<a id="FUNCTION:ADP-GITHUB-WRITING:TAG14"></a>
#### Function: clith\:withp \(sym\)

`````text
Checks wether a symbol denotes a WITH expansion.
`````
