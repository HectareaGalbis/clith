

<a id="TITLE:CLITH-DOCS:TAG6"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH)\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\.

* [Installation](/README.md#TITLE:CLITH-DOCS:TAG7)
* [Reference](/README.md#TITLE:CLITH-DOCS:TAG8)
* [Getting started](/README.md#TITLE:CLITH-DOCS:TAG9)
* [Defining a WITH expansion](/README.md#TITLE:CLITH-DOCS:TAG10)
* [Expansion\'s documentation](/README.md#TITLE:CLITH-DOCS:TAG11)
* [Declarations](/README.md#TITLE:CLITH-DOCS:TAG12)


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

The macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) uses ```WITH expansions``` in a similarly way to ```setf```\. These expansions control how the macro  [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) is expanded\.

Every common lisp function that creates an object that should be closed at the end has a ```WITH expansion```\. For example\, functions like [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm) or [make\-two\-way\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_two.htm) have a ```WITH expansion```\. See all the function in the [reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-SYMBOLS)\.

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

On the other hand\, clith defines new symbols for each common lisp ```with-``` macro\. Examples are ```package-iterator``` or ```slots```\. See all the macros in the [reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-MACROS)\.

`````common-lisp
(defstruct vec2
  x
  y)

(let ((start (make-vec2 :x 5 :y 10)))
  (with (((x y) (slots start)))
    (+ x y)))
`````
`````common-lisp
;; Returns
15
`````

The macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) can accept optional arguments for each slot\:

`````common-lisp
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (+ x1 y1 x2 y2)))
`````
`````common-lisp
;; Returns
8
`````

Here\, the slot ```x1``` is receiving the argument ```x```\. Each ```WITH expansion``` is responsible to manage these arguments\.

Lastly\, we can check if a symbol denotes a ```WITH expansion``` using [clith\:withp](/docs/scribble/reference.md#FUNCTION:CLITH:WITHP)\:

`````common-lisp
(withp 'slots)
`````
`````common-lisp
;; Returns
T
`````

<a id="TITLE:CLITH-DOCS:TAG10"></a>
## Defining a WITH expansion

In order to extend the macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) we need to define a ```WITH expansion```\. To do so\, we use [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH:DEFWITH)\.

Suppose we have ```(MAKE-WINDOW TITLE)``` and ```(DESTROY-WINDOW WINDOW)```\. We want to control the expansion of WITH in order to use both functions\. Let\'s define the WITH expansion\:

`````common-lisp
(defwith make-window ((window) (title) &body body)
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

Now we can use our expansion in WITH\:

`````
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
`````

After the evaluation of with\'s body\, ```my-window``` will be destroyed by ```destroy-window```\.

<a id="TITLE:CLITH-DOCS:TAG11"></a>
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


<a id="TITLE:CLITH-DOCS:TAG12"></a>
## Declarations

The macro [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) accepts declarations\. These declarations are moved to the correct place at expansion time\. For example\, consider again the example with the points\, but this time\, we want to ignore two arguments\:

`````common-lisp
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (declare (ignore y1 x2))
    (+ x1 y2)))
`````
`````common-lisp
;; Returns
1
`````

Let\'s see the expanded code\:

`````common-lisp
(macroexpand-1 '(with ((((x1 x) (y1 y)) (slots start))
                       (((x2 x) (y2 y)) (slots end)))
                  (declare (ignore y1 x2))
                  (+ x1 y2)))
`````
`````common-lisp
;; Returns
(WITH-SLOTS ((X1 X) (Y1 Y))
    START
  (DECLARE (IGNORE Y1))
  (WITH-SLOTS ((X2 X) (Y2 Y))
      END
    (DECLARE (IGNORE X2))
    (+ X1 Y2)))
T
`````

Observe that every declaration is in the right place\. But how this work\?

[clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) assumes that symbols to be bound will be in certain places\. Each symbol in the declaration is searched over all the places that can contain a symbol to be bound\. It is searched from bottom to top\. When a symbol is found\, a declaration of that symbol is created there\.

If you want to know exactly where these places are\, check out the syntax of the [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH:WITH) macro\:

`````text
(WITH (binding*) declaration* form*)

binding          ::= ([vars] form)
vars             ::= var | (var-with-options*)
var-with-options ::= var | (var var-option*)
var-option       ::= form
`````

```var``` are those places where a declaration can be placed\.