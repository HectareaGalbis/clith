<h1 id="header:ADP:HEADERTAG3">Common Lisp wITH</h1>

Welcome to Clith\!

This library defines the macro <a href="/docs/api.md#function:CLITH:WITH">clith:with</a>\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using that variables\, and lastly the objects are destroyed automatically\. Even more\, you can bind functions like LABELS does and nest expressions like UIOP\:NEST\.

* <a href="/README.md#header:ADP:HEADERTAG3">Common Lisp wITH</a>
  * <a href="/README.md#header:ADP:HEADERTAG4">Installation</a>
  * <a href="/README.md#header:ADP:HEADERTAG5">Documentation</a>
  * <a href="/README.md#header:ADP:HEADERTAG6">A brief guide</a>

<h2 id="header:ADP:HEADERTAG4">Installation</h2>

This library is available at Ultralisp\. If you don\'t have it already\, you can add it evaluating this\:

`````Lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
			 :prompt nil)
`````

After this you can install Clith using Quicklisp\:

`````Lisp
(ql:quickload :clith)
`````

<h2 id="header:ADP:HEADERTAG5">Documentation</h2>

* <a href="/docs/api.md#header:CLITH:API-REFERENCE-HEADER">Clith API reference</a>

<h2 id="header:ADP:HEADERTAG6">A brief guide</h2>

The simplest way to use <a href="/docs/api.md#function:CLITH:WITH">clith:with</a> is like using [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) or [multiple\-value\-bind](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

```Lisp
(clith:with ((x 5) ((q r) (floor 45 32)))
  (+ x q r))

19
```

But you can also bind functions\:

```Lisp
(clith:with ((hello (name) (format t "~%Hello ~a!" name)))
  (hello "there"))

Hello there!
nil
```

Or nest other expressions\. This can be useful when using other with\- macros\:

`````Lisp
(clith:with ((with-open-file (file "~/my-file.txt")))
  (print (read file)))
`````

And\, of course\, you can mix it all up\.

```Lisp
(clith:with ((n 10) (add2 (a b) (+ a b)) ((with-output-to-string (str))))
  (format str "The result is ~a" (add2 10 n)))

"The result is 20"
```

The bound variables are destroyed automatically at the end of WITH\. More precisely\, the generic function <a href="/docs/api.md#function:CLITH:DESTROYER">clith:destroyer</a> is called for almost all bound variables\. Variables bound by a LET\-like binding clause will be destroyed always\. However\, only the first bound variable within a MULTIPLE\-VALUE\-BIND\-like form will be destroyed\.

`````Lisp
(clith:with ((n 10) ((a b) (values 1 2)))
  (print n))
`````

Expands to\:

`````Lisp
(let* ((n 10))
  (unwind-protect
      (multiple-value-bind (a b)
          (values 1 2)
        (unwind-protect (progn (print n)) (clith:destroyer a)))
    (progn (clith:destroyer n))))
`````

Observe that only N and A are destroyed\.

The function <a href="/docs/api.md#function:CLITH:DESTROYER">clith:destroyer</a> is already defined for stream objects\. In fact\, this is the implementation of the method you can find in the source code\.

`````Lisp
(defmethod clith:destroyer ((obj stream)) "Closes a stream." (close obj))
`````

