<h1 id="header:CLITH:API-REFERENCE-HEADER">Clith API reference</h1>

<h4 id="function:CLITH:DEFWITH">Macro: DEFWITH</h4>

```Lisp
(DEFMACRO CLITH:DEFWITH (CONSTRUCTOR-NAME CONSTRUCTOR DESTRUCTOR)
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

<h4 id="function:CLITH:DEFINE-WITH-EXPANDER">Macro: DEFINE-WITH-EXPANDER</h4>

```Lisp
(DEFMACRO CLITH:DEFINE-WITH-EXPANDER (EXPANDER-NAME DESTRUCTURING-LAMBDA-LIST
                                      &BODY BODY)
  ...)
```

````
Defines an expander for the WITH macro called EXPANDER-NAME. The DESTRUCTURING-LAMBDA-LIST must receive a
&body or &rest argument. That argument will be the body forms used within the WITH macro. The expander must
return the form that the WITH macro will expand to.
````

<h4 id="function:CLITH:WITH">Macro: WITH</h4>

```Lisp
(DEFMACRO CLITH:WITH (BINDINGS &BODY BODY)
  ...)
```

````
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] constructor-form)
  vars             ::= symbol | (symbol*)
  constructor-form ::= (constructor-name arg*)
  constructor-name ::= symbol
  arg              ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms. 
Each constructor-name must be a symbol that was used in DEFWITH. 

Firstly, each constructor is called with the values placed after the constructor-name. The returned values are
used to bind the vars. vars are bound as if using MULTIPLE-VALUE-BIND. I.e. if there are more vars than values
returned, extra values of NIL are given to the remaining vars. If there are more values than vars, the excess
values are discarded. After these variables are bound, the forms are evaluated. Finally, each destructor is
called with the values returned by each constructor respectively.
````

