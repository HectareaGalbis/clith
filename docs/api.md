<h1 id="header:CLITH:API-REFERENCE-HEADER">Clith API reference</h1>

<h2 id="header:ADP:HEADERTAG0">Defwith</h2>

<h4 id="function:CLITH:DEFWITH">Macro: defwith</h4>

```Lisp
(defmacro clith:defwith (constructor-name constructor destructor)
  ...)
```

````
CONSTRUCTOR-NAME must be a symbol. CONSTRUCTOR and DESTRUCTOR must be forms that evaluate to a function.
DEFWITH defines a way to destruct an object returned by the function CONSTRUCTOR within the WITH macro. The
DESTRUCTOR must receive the same number of values that CONSTRUCTOR returns.

If CONSTRUCTOR-NAME has already a constructor and a destructor, they are replaced by CONSTRUCTOR and
DESTRUCTOR.
````

<h4 id="function:CLITH:WITH-CONSTRUCTOR-P">Function: with-constructor-p</h4>

```Lisp
(defun clith:with-constructor-p (constructor-name)
  ...)
```

````
Checks if the symbol CONSTRUCTOR-NAME is 'with constructor'.
````

<h4 id="function:CLITH:SYMBOL-WITH-CONSTRUCTOR">Function: symbol-with-constructor</h4>

```Lisp
(defun clith:symbol-with-constructor (constructor-name)
  ...)
```

````
Retrieves the constructor asociated with the symbol CONSTRUCTOR-NAME.
Raises an error if CONSTRUCTOR-NAME is not a symbol or is not a 'with constructor'.
````

<h4 id="function:CLITH:SYMBOL-WITH-DESTRUCTOR">Function: symbol-with-destructor</h4>

```Lisp
(defun clith:symbol-with-destructor (constructor-name)
  ...)
```

````
Retrieves the destructor asociated with the symbol CONSTRUCTOR-NAME.
Raises an error if CONSTRUCTOR-NAME is not a symbol or is not a 'with constructor'.
````

<h2 id="header:ADP:HEADERTAG1">Define-with-expander</h2>

<h4 id="function:CLITH:DEFINE-WITH-EXPANDER">Macro: define-with-expander</h4>

```Lisp
(defmacro clith:define-with-expander (expander-name destructuring-lambda-list
                                      &body body)
  ...)
```

````
Defines an expander for the WITH macro called EXPANDER-NAME. The DESTRUCTURING-LAMBDA-LIST must receive at
least two arguments. The first one represents a VARS list used within the WITH macro. The second one represents
the body forms of a WITH macro. The expander must return the form that the WITH macro will expand to.
````

<h4 id="function:CLITH:WITH-EXPANDER-P">Function: with-expander-p</h4>

```Lisp
(defun clith:with-expander-p (expander-name)
  ...)
```

````
Checks if the symbol EXPANDER-NAME is a 'with expander'.
````

<h2 id="header:ADP:HEADERTAG2">With</h2>

<h4 id="function:CLITH:WITH">Macro: with</h4>

```Lisp
(defmacro clith:with (bindings &body body)
  ...)
```

````
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] { constructor-form | expander-form })
  vars             ::= symbol | (symbol*)
  constructor-form ::= (constructor-name arg*)
  expander-form    ::= (expander-name arg*)
  constructor-name ::= symbol
  expander-name    ::= symbol
  arg              ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms. 
Each constructor-name must be a symbol that was used in DEFWITH. The same goes to expander-name that must be a
symbol used in DEFINE-WITH-EXPANDER. 

Each constructor is called with the values placed after the constructor-name. The returned values are used to
bind the vars. vars are bound as if using MULTIPLE-VALUE-BIND. I.e. if there are more vars than values
returned, extra values of NIL are given to the remaining vars. If there are more values than vars, the excess
values are discarded. After these variables are bound, the forms are evaluated. At the end, each destructor is
called with the values returned by each constructor respectively.

Each expander is expanded like a macro does. They will receive the args plus the body forms of the WITH macro.
Its expansion is what the WITH macro will expand to.
````

<h2 id="header:ADP:HEADERTAG3">Predefined 'with constructors'</h2>

* [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm)
* [make\-broadcast\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_bro.htm)
* [make\-concatenated\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_con.htm)
* [make\-echo\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_ech.htm)
* [make\-string\-input\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_s_1.htm)
* [make\-string\-output\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_s_2.htm)
* [make\-synonym\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_syn.htm)
* [make\-two\-way\-stream](http://www.lispworks.com/reference/HyperSpec/Body/f_mk_two.htm)

<h2 id="header:ADP:HEADERTAG4">Predefined 'with expanders'</h2>

<h3 id="header:ADP:HEADERTAG5">In</h3>

It expects a list\. The binding symbol represents each element of the list\.

```Lisp
(clith:with ((x (clith:in '(1 2 3 4))))
  (print x))

1 
2 
3 
4 
nil
```

<h3 id="header:ADP:HEADERTAG6">Across</h3>

Same IN\, but it expects a vector\.

```Lisp
(clith:with ((x (clith:across #(1 2 3 4))))
  (print x))

1 
2 
3 
4 
nil
```

