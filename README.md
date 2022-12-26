<h1 id="header:ADP:HEADERTAG1">Common Lisp wITH</h1>

Welcome to Clith\!

This library defines the macro <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a>\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using that variables\, and lastly the objects are destroyed automatically\.

A well known example is opening a file\, performing some writing or reading\, and closing the file\. To do this using the macro <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> we must define a \'with\-destructor\' using <a href="/docs/api.md#function:CLITH:DEFINE-WITH-DESTRUCTOR">CLITH:DEFINE-WITH-DESTRUCTOR</a>\.

```Lisp
(CLITH:DEFINE-WITH-DESTRUCTOR STREAM
    (SOME-STREAM)
  (PRINT "Closing a stream")
  (CLOSE SOME-STREAM))

#<STANDARD-METHOD CLITH:MAKE-WITH-DESTRUCTOR (STREAM) {10026DF7A3}>
```

Now we can use the <a href="/docs/api.md#function:CLITH:WITH">CLITH:WITH</a> macro\.

```Lisp
(CLITH:WITH ((FILE-STREAM
              (OPEN "~/some-file.txt" :DIRECTION :OUTPUT :IF-DOES-NOT-EXIST
                    :CREATE :IF-EXISTS :SUPERSEDE)))
  (FORMAT FILE-STREAM "Hello clith!"))

"Closing a stream" 
NIL
```

