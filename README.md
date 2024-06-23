

<a id="header-adp-github-headertag342"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/scribble/reference.md#function-clith-with)\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\.

* [Common Lisp wITH](/README.md#header-adp-github-headertag342)
  * [Installation](/README.md#header-adp-github-headertag343)
  * [Documentation](/README.md#header-adp-github-headertag344)
  * [Basic usage](/README.md#header-adp-github-headertag345)
  * [Customizing expansion](/README.md#header-adp-github-headertag352)
  * [Built\-in WITH expanders](/README.md#header-adp-github-headertag359)


<a id="header-adp-github-headertag343"></a>
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

<a id="header-adp-github-headertag344"></a>
## Documentation

* [Reference](/docs/scribble/reference.md#header-adp-github-reference)


<a id="header-adp-github-headertag345"></a>
## Basic usage

The simplest way to use [clith\:with](/docs/scribble/reference.md#function-clith-with) is like using LET or MULTIPLE\-VALUE\-BIND\:

`````common-lisp
(clith:with ((x 5)
             ((q r) (floor 45 32)))
  (+ x q r))
`````
`````common-lisp
19
`````


But also we can open a file that will be destroyed automatically when exiting the body of [clith\:with](/docs/scribble/reference.md#function-clith-with)\:

`````common-lisp
(clith:with ((file (open-file "~/test.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)))
  (print "Hey!" file))
`````
`````common-lisp
"Hey!"
`````

And the content of the file should be ``` "Hey!" ```\.

Or we can even take the values of an object\:

`````common-lisp
(defclass point ()
  ((x :initarg :x)
   (y :initarg :y)))

(clith:with ((start (make-instance 'point :x 5 :y 10))
             (end   (make-instance 'point :x -3 :y 21))
             ((x y)           (slots start))
             (((x2 x) (y2 y)) (slots end)))
  (+ (* x x2) (* y y2)))
`````
`````common-lisp
195
`````

<a id="header-adp-github-headertag352"></a>
## Customizing expansion

When using certain binding form\, we can control how [clith\:with](/docs/scribble/reference.md#function-clith-with) is expanded\. In order to do this we must use [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith)\.

As a simple example\, let\'s define the with expander ``` specials ```\. It must bind some variables dynamically \(via ``` (declare (special var)) ```\)\.

First\, take a look at the [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith) macro\:

`````
(clith:defwith specials (vars (&rest vals) &body body)
  ...)
`````

The macro [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith) must receive 2 arguments\: The variables used in the [clith\:with](/docs/scribble/reference.md#function-clith-with) binding and the body of the macro\. Lastly\, we receive the actual arguments of ``` specials ```\.

The rest of the definition is as follows\:

`````common-lisp
(clith:defwith specials (vars (&rest vals) &body body)
  `(multiple-value-bind ,vars (values ,@vals)
     (declare (special ,@vars))
     ,@body))
`````
`````common-lisp
specials
`````

It is like a regular macro\. First we bind the variables\, then we declare them as special and lastly we evaluate the body forms\.

Let\'s use it\:

`````common-lisp
(defun add-special-x-y ()
  (declare (special x y))
  (+ x y))

(clith:with (;; ...
             ((x y) (specials 5 10))
             ;; ...
             )
  ;; ...
  (add-special-x-y))
`````
`````common-lisp
15
`````

In this example\, ``` (x y) ``` is bound to ``` vars ``` in [clith\:defwith](/docs/scribble/reference.md#function-clith-defwith)\, ``` ((add-special-x-y)) ``` to ``` body ``` and ``` (5 10) ``` to ``` vals ```\.

Let\'s see another example\:

`````common-lisp
(clith:with (;; ...
             (z (specials 5 10))
             ;; ...
             )
  (print z))
`````
`````text

5 
`````
`````common-lisp
5
`````

Note that now we have ``` z ``` instead of ``` (z) ```\. Both cases are valid\. CLITH makes sure that ``` vars ``` is always bound to a list of variables\. If the user doesn\'t indicate any variable\, then ``` NIL ``` is bound to ``` vars ```\.

<a id="header-adp-github-headertag359"></a>
## Built\-in WITH expanders

Every macro from the package ``` common-lisp ``` whose name starts with ``` with- ``` has its own expander\. We\'ve already seen an example using the expander of ``` with-open-file ```\. The complete list is\:

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