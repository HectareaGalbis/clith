<h1 id="header:CLITH:API-REFERENCE-HEADER">Clith API reference</h1>

<h4 id="function:CLITH:DEFWITH">Macro: DEFWITH</h4>

```Lisp
(defmacro CLITH:DEFWITH (SUFFIX CONSTRUCTOR DESTRUCTOR &OPTIONAL DOCSTRING)
  ...)
```

````
This macro has the following syntax:

  (DEFWITH suffix constructor destructor &optional docstring)

  suffix      ::= An interned symbol
  constructor ::= A form that evaluates to a function
  destructor  ::= A form that evaluates to a function
  docstring   ::= NIL | string

Defines a new macro called WITH-suffix with DOCSTRING as documentation string. This new macro has the following
syntax:

  (WITH-suffix vars args decalration* body-expr*)

  vars      ::= symbol | (symbol*)
  args      ::= (form*)
  body-expr ::= form

This new macro binds the symbols in vars with the values returned by constructor. The constructor is called with
the arguments in args. Then the body-exprs are evaluated. Finally, the destructor is called. The destructor will
receive all the values returned by the constructor. 

The binding of vars work the same as MULTIPLE-VALUE-BIND. If there are more vars than values returned, extra
values of NIL are given to the remaining vars. If there are more values than vars, the excess values are
discarded.
````

<h4 id="function:CLITH:WITH">Macro: WITH</h4>

```Lisp
(defmacro CLITH:WITH (BINDINGS &BODY BODY)
  ...)
```

````
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding            ::= (vars suffix-constructor)
  vars               ::= symbol | (symbol*)
  suffix-constructor ::= (suffix arg*)
  suffix             ::= symbol
  arg                ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms.

Each suffix must be a symbol that was used in DEFWITH to define a 'with macro'. 

WITH expands to nested WITH-suffix forms.
````

