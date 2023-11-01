<a id="header-adp-github-headertag694"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/reference.md#function-clith-with)\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\. Even more\, you can bind functions like LABELS does and nest expressions like UIOP\:NEST\.

* [Common Lisp wITH](/README.md#header-adp-github-headertag694)
  * [Documentation](/README.md#header-adp-github-headertag695)
  * [A brief guide](/README.md#header-adp-github-headertag696)


<a id="header-adp-github-headertag695"></a>
## Documentation

* [Reference](/docs/reference.md#header-adp-github-reference)


<a id="header-adp-github-headertag696"></a>
## A brief guide

The simplest way to use [clith\:with](/docs/reference.md#function-clith-with) is like using LET or MULTIPLE\-VALUE\-BIND\:

`````common-lisp
(clith:with ((x 5)
             ((q r) (floor 45 32)))
  (+ x q r))
`````
`````common-lisp
19
`````

But you can also bind functions\:

`````common-lisp
(clith:with ((hello (name)
               (format t "~%Hello ~a!" name)))
  (hello "there"))
`````
`````text

Hello there!
`````
`````common-lisp
nil
`````

Or nest other expressions\. This can be useful when using other with\- macros\:

`````common-lisp
(clith:with ((with-open-file (file "~/my-file.txt")))
  (print (read file)))
`````

And\, of course\, you can mix it all up\.

`````common-lisp
(clith:with ((n 10)
             (add2 (a b)
               (+ a b))
             ((with-output-to-string (str))))
  (format str "The result is ~a" (add2 10 n)))
`````
`````common-lisp
"The result is 20"
`````

The bound variables are destroyed automatically at the end of [clith\:with](/docs/reference.md#function-clith-with)\. More precisely\, the generic function [clith\:destroyer](/docs/reference.md#function-clith-destroyer) is called for almost all bound variables\. Variables bound by a LET\-like binding clause will be destroyed always\. However\, only the first bound variable within a MULTIPLE\-VALUE\-BIND\-like form will be destroyed\.

`````common-lisp
(clith:with ((n 10)
             ((a b) (values 1 2)))
  (print n))
`````

Expands to\:

`````common-lisp
(let* ((n 10))
  (unwind-protect
      (multiple-value-bind (a b) (values 1 2)
        (unwind-protect
            (progn (print n))
          (destroyer a)))
    (progn
      (destroyer n))))
`````

Observe that only ``` N ``` and ``` A ``` are destroyed\.

The function [clith\:destroyer](/docs/reference.md#function-clith-destroyer) is already defined for stream objects\. In fact\, this is the implementation of the method you can find in the source code\.

`````common-lisp
(defmethod destroyer ((obj stream))
  "Closes a stream."
  (close obj))
`````