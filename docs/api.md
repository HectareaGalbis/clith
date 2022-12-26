<h1 id="header:ADP:HEADERTAG0">Clith API reference</h1>

<h4 id="function:CLITH:MAKE-WITH-DESTRUCTOR">Generic function: MAKE-WITH-DESTRUCTOR</h4>

```Lisp
(defgeneric CLITH:MAKE-WITH-DESTRUCTOR (OBJECT)
  ...)
```

<h4 id="function:CLITH:DEFINE-WITH-DESTRUCTOR">Macro: DEFINE-WITH-DESTRUCTOR</h4>

```Lisp
(defmacro CLITH:DEFINE-WITH-DESTRUCTOR (CLASS LAMBDA-LIST &BODY BODY)
  ...)
```

````
Syntactic sugar for defining a method for MAKE-WITH-DESTRUCTOR. The fist argument is the class of the object
to be destroyed.
````

<h4 id="function:CLITH:WITH">Macro: WITH</h4>

```Lisp
(defmacro CLITH:WITH (BINDINGS &BODY BODY)
  ...)
```

````
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding     ::= (var-or-vars constructor-form)
  var-or-vars ::= symbol | (symbol+)
  constructor-form ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms. Each
constructor-form must return at least one object. The first object returned must be of a class type that has
specialized the MAKE-WITH-DESTRUCTOR method. After evaluating the body forms, each value returned by the
constructor-form is received by the destructor created by MAKE-WITH-DESTRUCTOR.

var-or-vars indicates the symbols to be bound with the values returned by constructor-form. It works like 
MULTIPLE-VALUE-BIND, i.e., if there are more symbols than values returned, extra values of NIL are given to the
remaining symbols. If there are more values than symbols, the excess values are discarded.
````

