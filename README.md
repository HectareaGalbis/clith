<h1 id="header:ADP:HEADERTAG0">Common Lisp wITH</h1>

Welcome to Clith\!

This library defines the macro <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a>\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using that variables\, and lastly the objects are destroyed automatically\.

* <a href="/README.md#header:ADP:HEADERTAG0">Common Lisp wITH</a>
  * <a href="/README.md#header:ADP:HEADERTAG1">Installation</a>
  * <a href="/README.md#header:ADP:HEADERTAG2">Documentation</a>
  * <a href="/README.md#header:ADP:HEADERTAG3">A brief introduction</a>

<h2 id="header:ADP:HEADERTAG1">Installation</h2>

This library is available at Ultralisp\. If you don\'t have it already\, you can add it evaluating this\:

`````Lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
			 :prompt nil)
`````

After this you can install Clith using Quicklisp\:

`````Lisp
(ql:quickload :clith)
`````

<h2 id="header:ADP:HEADERTAG2">Documentation</h2>

You can read the <a href="/docs/api.md#header:CLITH:API-REFERENCE-HEADER">Clith API reference</a> and the <a href="/docs/guide.md#header:CLITH:CLITH-GUIDE-HEADER">The Clith guide</a>\.

<h2 id="header:ADP:HEADERTAG3">A brief introduction</h2>

Let\'s see how we can create a \'with macro\'\. A well known example is the macro [WITH\-OPEN\-FILE](http://www.lispworks.com/reference/HyperSpec/Body/m_w_open.htm)\. Let\'s define our own macro called <a href="/README.md#function:CLITH:WITH-MY-OPEN-FILE">CLITH::WITH-MY-OPEN-FILE</a>\. It will do the same as [WITH\-OPEN\-FILE](http://www.lispworks.com/reference/HyperSpec/Body/m_w_open.htm) but it prints a message when the stream is about
to be closed\.

`````Lisp
(CLITH:DEFWITH MY-OPEN-FILE #'OPEN
               (LAMBDA (&REST ARGS)
                 (PRINT "Closing the stream")
                 (APPLY #'CLOSE ARGS))
               "Same as OPEN-FILE")
`````

The first argument must be a symbol denoting the suffix of our new macro\. The next arguments are the constructor and the destructor\. Finally we can add a docstring to our macro\.

The defined macro is the next one\:

<h4 id="function:CLITH:WITH-MY-OPEN-FILE">Macro: WITH-MY-OPEN-FILE</h4>

```Lisp
(defmacro WITH-MY-OPEN-FILE (VAR ARGS &BODY BODY)
  ...)
```

````
Same as OPEN-FILE
````

Now we can use that macro to perform our reading or writing operations\:

```Lisp
(WITH-MY-OPEN-FILE FILE-STREAM
    (#P"~/example.txt" :DIRECTION :OUTPUT :IF-DOES-NOT-EXIST :CREATE :IF-EXISTS
     :SUPERSEDE)
  (FORMAT FILE-STREAM "Hello Clith!"))

"Closing the stream" 
NIL
```

