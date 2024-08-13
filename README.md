

<a id="header-adp-github-headertag382"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/scribble/reference.md#function-clith-with)\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\.

* [Common Lisp wITH](/README.md#header-adp-github-headertag382)
  * [Installation](/README.md#header-adp-github-headertag383)
  * [Reference](/README.md#header-adp-github-headertag384)
  * [Basic usage](/README.md#header-adp-github-headertag385)
  * [Defining a WITH expander](/README.md#header-adp-github-headertag390)
  * [Expander\'s documentation](/README.md#header-adp-github-headertag395)
  * [Declarations](/README.md#header-adp-github-headertag404)
  * [Built\-in WITH expanders](/README.md#header-adp-github-headertag409)


<a id="header-adp-github-headertag383"></a>
## Installation

* Manual\:

`````sh
cd ~/common-lisp
git clone https://github.com/Hectarea1996/clith.git
`````
* Quicklisp \(Ultralisp\)\:

`````common-lisp
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
(ql:quickload "clith")
`````

<a id="header-adp-github-headertag384"></a>
## Reference

* [Reference](/docs/scribble/reference.md#header-clith-docs-reference)


<a id="header-adp-github-headertag385"></a>
## Basic usage

The macro [clith\:with](/docs/scribble/reference.md#function-clith-with) uses ``` WITH expanders ``` simarly ``` setf ``` uses ``` setf expanders ```\. These expanders controls how the macro  [clith\:with](/docs/scribble/reference.md#function-clith-with) is expanded\.

Let\'s take a look at the built\-in ``` slots ``` expander\. As the name suggest\, it will expand into a ``` with-slots ``` expression\:

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

<a id="header-adp-github-headertag390"></a>
## Defining a WITH expander

In order to extend the macro [clith\:with](/docs/scribble/reference.md#function-clith-with) we need to define a ``` WITH expander ```\. To do so\, we use [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith)\.

Suppose we have ``` (MAKE-WINDOW TITLE) ``` and ``` (DESTROY-WINDOW WINDOW) ```\. We want to control the expansion of WITH in order to use both functions\. Let\'s define the WITH expander\:

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

This is a common implementation of a \'with\-\' macro\. Note that we specified ``` (window) ``` to specify that only
one variable is wanted\.

Now we can use our expander in WITH\:

`````
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
`````

After the body of [clith\:with](/docs/scribble/reference.md#function-clith-with) is evaluated\, ``` my-window ``` will be destroyed by ``` destroy-window ```\.

<a id="header-adp-github-headertag395"></a>
## Expander\'s documentation

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


<a id="header-adp-github-headertag404"></a>
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

``` var ``` are those places where a declaration can refer to\.

<a id="header-adp-github-headertag409"></a>
## Built\-in WITH expanders

The next symbols from the package ``` CL ``` has a built\-in expander\:

* ``` make-broadcast-stream ```
* ``` make-concatenated-stream ```
* ``` make-echo-stream ```
* ``` make-string-input-stream ```
* ``` make-string-output-stream ```
* ``` make-two-way-stream ```
* ``` open ```


Additionally\, every macro from the package ``` CL ``` whose name starts with ``` with- ``` has its own expander\. We\'ve already seen an example using the expander ``` slots ```\.

Since we cannot define new symbols in the package ``` CL ```\, these expanders are defined in a special way\. [clith\:with](/docs/scribble/reference.md#function-clith-with) will recognize all the symbols \(for any package\) whose name is equal to the name of the expander\.

The complete list is\:

<table>
<tr>
<td>CL Standard macro</td>
<td>WITH expander</td>
</tr>
<tr>
<td>with-accesors</td>
<td>accesors</td>
</tr>
<tr>
<td>with-compilation-unit</td>
<td>compilation-unit</td>
</tr>
<tr>
<td>with-condition-restarts</td>
<td>condition-restarts</td>
</tr>
<tr>
<td>with-hash-table-iterator</td>
<td>hash-table-iterator</td>
</tr>
<tr>
<td>with-input-from-string</td>
<td>input-from-string</td>
</tr>
<tr>
<td>with-open-file</td>
<td>open-file</td>
</tr>
<tr>
<td>with-open-stream</td>
<td>open-stream</td>
</tr>
<tr>
<td>with-output-to-string</td>
<td>output-to-string</td>
</tr>
<tr>
<td>with-package-iterator</td>
<td>package-iterator</td>
</tr>
<tr>
<td>with-simple-restart</td>
<td>simple-restart</td>
</tr>
<tr>
<td>with-slots</td>
<td>slots</td>
</tr>
<tr>
<td>with-standard-io-syntax</td>
<td>standard-io-syntax</td>
</tr>
</table>