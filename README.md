

<a id="header-adp-github-headertag674"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/scribble/reference.md#function-clith-with)\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\.

* [Common Lisp wITH](/docs/scribble/README.md#header-adp-github-headertag674)
  * [Installation](/docs/scribble/README.md#header-adp-github-headertag675)
  * [Documentation](/docs/scribble/README.md#header-adp-github-headertag676)
  * [Basic usage](/docs/scribble/README.md#header-adp-github-headertag677)
  * [Customizing expansion](/docs/scribble/README.md#header-adp-github-headertag682)
    * [Built\-in WITH expanders](/docs/scribble/README.md#header-adp-github-headertag689)


<a id="header-adp-github-headertag675"></a>
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


<a id="header-adp-github-headertag676"></a>
## Documentation

* [Reference](/docs/scribble/reference.md#header-adp-github-reference)


<a id="header-adp-github-headertag677"></a>
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


<a id="header-adp-github-headertag682"></a>
## Customizing expansion

There are some cases that [clith\:with](/docs/scribble/reference.md#function-clith-with) cannot resolve on its own\. Expanders are designed to solve this\. When using certain binding form\, we can control how [clith\:with](/docs/scribble/reference.md#function-clith-with) is expanded\. In order to do this we must use [clith\:define\-with\-expander](/docs/scribble/reference.md#function-clith-define-with-expander)\.

As a simple example\, let\'s define the with expander ``` specials ```\. It must bind some variables dynamically \(via ``` (declare (special var)) ```\)\.

First\, take a look at the [clith\:define\-with\-expander](/docs/scribble/reference.md#function-clith-define-with-expander) macro\:

`````
(clith:define-with-expander specials (vars body &rest vals)
  ...)
`````

The macro [clith\:define\-with\-expander](/docs/scribble/reference.md#function-clith-define-with-expander) must receive 2 arguments\: The variables used in the [clith\:with](/docs/scribble/reference.md#function-clith-with) binding and the body of the macro\. Lastly\, we receive the actual arguments of ``` specials ```\.

The rest of the definition is as follows\:

`````common-lisp
(clith:define-with-expander specials (vars body &rest vals)
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

In this example\, ``` (x y) ``` is bound to ``` vars ``` in [clith\:define\-with\-expander](/docs/scribble/reference.md#function-clith-define-with-expander)\, ``` ((add-special-x-y)) ``` to ``` body ``` and ``` (5 10) ``` to ``` vals ```\.

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

Note that now we have ``` z ``` instead of ``` (z) ```\. Both cases are valid\. CLITH makes sure that ``` vars ``` is alwais bound to a list of variables\. If the user doesn\'t indicate any variable\, then ``` NIL ``` is bound to ``` vars ```\.

<a id="header-adp-github-headertag689"></a>
### Built\-in WITH expanders

Every macro from the package ``` common-lisp ``` whose name starts with ``` with- ``` has its own expander\. We\'ve already seen an example using the expander of ``` with-open-file ```\. The complete list is\:

* ``` with-accesors ``` \-\> ``` accesors ```
* ``` with-compilation-unit ``` \-\> ``` compilation-unit ```
* ``` with-condition-restarts ``` \-\> ``` condition-restarts ```
* ``` with-hash-table-iterator ``` \-\> ``` hash-table-iterator ```
* ``` with-input-from-string ``` \-\> ``` input-from-string ```
* ``` with-open-file ``` \-\> ``` open-file ```
* ``` with-open-stream ``` \-\> ``` open-stream ```
* ``` with-output-to-string ``` \-\> ``` output-to-string ```
* ``` with-package-iterator ``` \-\> ``` package-iterator ```
* ``` with-simple-restart ``` \-\> ``` simple-restart ```
* ``` with-slots ``` \-\> ``` slots ```
* ``` with-standard-io-syntax ``` \-\> ``` standard-io-syntax ```
