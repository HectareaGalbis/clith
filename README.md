<h1 id="header:ADP:HEADERTAG3">Common Lisp wITH</h1>

Welcome to Clith\!

This library defines the macro <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a>\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using that variables\, and lastly the objects are destroyed automatically\. Even more\, you can bind functions like LABELS does and nest expressions like UIOP\:NEST\.

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

The simplest way to use <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> is like using [LET](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) or [MULTIPLE\-VALUE\-BIND](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\:

```Lisp
(CLITH:WITH ((X 5) ((Q R) (FLOOR 45 32)))
  (+ X Q R))

19
```

But you can also bind functions\:

```Lisp
(CLITH:WITH ((HELLO (NAME) (FORMAT T "~%Hello ~a!" NAME)))
  (HELLO "there"))

Hello there!
NIL
```

Or nest other expressions\. This can be useful when using other with\- macros\:

`````Lisp
(CLITH:WITH ((WITH-OPEN-FILE (FILE "~/my-file.txt")))
  (PRINT (READ FILE)))
`````

And\, of course\, you can mix it all up\.

```Lisp
(CLITH:WITH ((N 10) (ADD2 (A B) (+ A B)) ((WITH-OUTPUT-TO-STRING (STR))))
  (FORMAT STR "The result is ~a" (ADD2 10 N)))

"The result is 20"
```

The bound variables are destroyed automatically at the end of WITH\. More precisely\, the generic function <a href="/docs/api.md#function:CLITH:DESTROYER">CLITH::DESTROYER</a> is called for almost all bound variables\. Variables bound by a LET\-like binding clause will be destroyed always\. However\, only the first bound variable within a MULTIPLE\-VALUE\-BIND\-like form will be destroyed\.

`````Lisp
(CLITH:WITH ((N 10) ((A B) (VALUES 1 2)))
  (PRINT N))
`````

Expands to\:

`````Lisp
(LET* ((N 10))
  (UNWIND-PROTECT
      (MULTIPLE-VALUE-BIND (A B)
          (VALUES 1 2)
        (UNWIND-PROTECT (PROGN (PRINT N)) (DESTROYER A)))
    (PROGN (DESTROYER N))))
`````

Observe that only N and A are destroyed\.

The function <a href="/docs/api.md#function:CLITH:DESTROYER">CLITH::DESTROYER</a> is already defined for stream objects\. In fact\, this is the implementation of the method you can find in the source code\.

`````Lisp
(DEFMETHOD DESTROYER ((OBJ STREAM)) "Closes a stream." (CLOSE OBJ))
`````

