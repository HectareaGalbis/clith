

<a id="header-adp-github-headertag549"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/scribble/reference.md#function-clith-with)\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\.

* [Common Lisp wITH](/README.md#header-adp-github-headertag549)
  * [Installation](/README.md#header-adp-github-headertag550)
  * [Reference](/README.md#header-adp-github-headertag551)
  * [Getting started](/README.md#header-adp-github-headertag552)
  * [Defining a WITH expansion](/README.md#header-adp-github-headertag564)
  * [Expansion\'s documentation](/README.md#header-adp-github-headertag569)
  * [Declarations](/README.md#header-adp-github-headertag578)
  * [Built\-in WITH expansions](/README.md#header-adp-github-headertag589)


<a id="header-adp-github-headertag550"></a>
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

<a id="header-adp-github-headertag551"></a>
## Reference

* [Reference](/docs/scribble/reference.md#header-clith-docs-reference)


<a id="header-adp-github-headertag552"></a>
## Getting started

The macro [clith\:with](/docs/scribble/reference.md#function-clith-with) uses ``` WITH expansions ``` similarly ``` setf ``` uses ``` setf expansions ```\. These expansions control how the macro  [clith\:with](/docs/scribble/reference.md#function-clith-with) is expanded\.

We can define a ``` WITH expansion ``` using [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith)\. As an example\, let\'s define a ``` WITH expansion ``` named ``` slots ``` that should expand to ``` with-slots ```\.

`````common-lisp
(defwith slots (vars (instance) &body body)
  `(with-slots ,vars ,instance
     ,@body))
`````
`````common-lisp
slots
`````

We can check if a symbol denotes a ``` WITH expansion ``` using [clith\:withp](/docs/scribble/reference.md#function-clith-withp)\:

`````common-lisp
(withp 'slots)
`````
`````common-lisp
t
`````

Now we can use the new expansion like this\:

`````common-lisp
(defstruct vec2
  x
  y)

(let ((start (make-vec2 :x 5 :y 10)))
  (with (((x y) (slots start)))
    (+ x y)))
`````
`````common-lisp
15
`````

The macro [clith\:with](/docs/scribble/reference.md#function-clith-with) also accepts options for each variable we want to bind\. In the above example\, what happens if we have two points\?

`````
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with (((x y) (slots origin))
         ((x y) (slots end)))   ;; <-- Name collision!!
    (+ x y x y)))
`````

We should specify\, as if using ``` with-slots ```\, that we want to reference the slot ``` x ``` or ``` y ``` using another symbol\.

`````common-lisp
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (+ x1 y1 x2 y2)))
`````
`````common-lisp
8
`````

This works because of how we defined our expansion ``` slots ```\. The details can be found in the reference\: [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith)\.

<a id="header-adp-github-headertag564"></a>
## Defining a WITH expansion

In order to extend the macro [clith\:with](/docs/scribble/reference.md#function-clith-with) we need to define a ``` WITH expansion ```\. To do so\, we use [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith)\.

Suppose we have ``` (MAKE-WINDOW TITLE) ``` and ``` (DESTROY-WINDOW WINDOW) ```\. We want to control the expansion of WITH in order to use both functions\. Let\'s define the WITH expansion\:

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
make-window
`````

This is a common implementation of a \'with\-\' macro\. Note that we specified ``` (window) ``` to specify that only one variable is wanted\.

Now we can use our expansion in WITH\:

`````
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
`````

After the body of [clith\:with](/docs/scribble/reference.md#function-clith-with) is evaluated\, ``` my-window ``` will be destroyed by ``` destroy-window ```\.

<a id="header-adp-github-headertag569"></a>
## Expansion\'s documentation

The macro [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith) accepts a docstring that can be retrieved with the function ``` documentation ```\. Check out again the definition of the expansion of ``` make-window ``` above\. Note that we wrote a docstring\.

`````common-lisp
(documentation 'make-window 'with)
`````
`````common-lisp
"Makes a window that will be destroyed after the end of WITH."
`````

We can also ``` setf ``` the docstring\:

`````common-lisp
(setf (documentation 'make-window 'with) "Another docstring!")
(documentation 'make-window 'with)
`````
`````common-lisp
"Another docstring!"
`````


<a id="header-adp-github-headertag578"></a>
## Declarations

The macro [clith\:with](/docs/scribble/reference.md#function-clith-with) accepts declarations\. These declarations are moved to the correct place at expansion time\. For example\, consider again the example with the points\, but this time\, we want to ignore two arguments\:

`````common-lisp
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (declare (ignore y1 x2))
    (+ x1 y2)))
`````
`````common-lisp
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
(with-slots ((x1 x) (y1 y))
    start
  (declare (ignore y1))
  (with-slots ((x2 x) (y2 y))
      end
    (declare (ignore x2))
    (+ x1 y2)))

t
`````

Observe that every declaration is in the right place\. But how this work\?

[clith\:with](/docs/scribble/reference.md#function-clith-with) assumes that variables to be bound will be in certain places\. Each variable in the declaration is searched over all the places that can contain a variable to be bound\. It is searched from bottom to top\. When a variable is found\, a declaration of that variable is created there\.

If you want to know exactly where these places are\, check out the syntax of the [clith\:with](/docs/scribble/reference.md#function-clith-with) macro\:

`````text
(WITH (binding*) declaration* form*)

binding          ::= ([vars] form)
vars             ::= var | (var-with-options*)
var-with-options ::= var | (var var-option*)
var-option       ::= form
`````

``` var ``` are those places where a declaration can be placed\.

<a id="header-adp-github-headertag589"></a>
## Built\-in WITH expansions

Clith doesn\'t provide any built\-in expansions\. However\, you can check out the project [clith\-std](https://github.com/HectareaGalbis/clith-std)\.