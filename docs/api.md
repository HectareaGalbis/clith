<h1 id="header:CLITH:API-REFERENCE-HEADER">Clith API reference</h1>

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

If this form is at top-level, effects will take place at compile time.
````

<h4 id="function:CLITH:DEFINE-WITH-EXPANDER">Macro: define-with-expander</h4>

```Lisp
(defmacro clith:define-with-expander (expander-name destructuring-lambda-list
                                      &body body)
  ...)
```

````
Defines an expander for the WITH macro called EXPANDER-NAME. The DESTRUCTURING-LAMBDA-LIST must receive a
&body or &rest argument. That argument will be the body forms used within the WITH macro. The expander must
return the form that the WITH macro will expand to.
````

<h4 id="function:CLITH:WITH">Macro: with</h4>

```Lisp
(defmacro clith:with (bindings &body body)
  ...)
```

````
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] constructor-form) | (expander-form)
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

