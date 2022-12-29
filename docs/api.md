<h1 id="header:CLITH:API-REFERENCE-HEADER">Clith API reference</h1>

<h4 id="function:CLITH:DEFWITH">Function: defwith</h4>

```Lisp
(defun clith:defwith (constructor-name constructor destructor)
  ...)
```

````
CONSTRUCTOR-NAME must be a symbol. CONSTRUCTOR and DESTRUCTOR must be functions. DEFWITH Defines a
constructor-name for the macro WITH. This will enable the use of a form with the syntax

  (constructor-name arg*)

within the WITH macro. arg* denotes the arguments that CONSTRUCTOR must receive. The DESTRUCTOR must receive the
same number of values that CONSTRUCTOR returns.

If CONSTRUCTOR-NAME had already associated a constructor and a destructor, they are replaced by CONSTRUCTOR and
DESTRUCTOR.

If this form is at top-level, effects will take place at compile time.
````

<h4 id="function:CLITH:WITH">Macro: with</h4>

```Lisp
(defmacro clith:with (bindings &body body)
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

