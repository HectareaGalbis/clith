<a id="TITLE:CLITH-DOCS:REFERENCE"></a>
# Reference

* [API reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:TAG60)
* [Built\-in WITH expansions](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-SYMBOLS)



<a id="TITLE:CLITH-DOCS:TAG60"></a>
## API reference

* [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG62)
* [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG61)
* [clith\:withp](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG63)


<a id="FUNCTION:CLITH:DEFWITH"></a>
<a id="FUNCTION:CLITH-DOCS:TAG62"></a>
#### Macro: clith\:defwith \(name \(vars args with\-body\) \&body body\)

`````text
Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. This macro has
the following syntax:

  (DEFWITH name (vars args with-body) declaration* body*)

  name             ::= symbol
  vars             ::= symbol | (symbol*)
  args             ::= destructuring-lambda-list
  with-body        ::= symbol
  declaration      ::= declaration-form | docstring
  body             ::= form

The symbol NAME will be available to use inside WITH expanding to the value returned by BODY.
The variables to be bound are passed through VARS (VARS will always be a list) and the arguments passed
to NAME are bound to ARGS. Finally, WITH-BODY is bound to the body of the WITH macro. Note that WITH-BODY
can contain declarations.

As an example, let's define the with expansion MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

  (defwith my-file (vars (filespec &rest options) body)
    "Open a file."
    (with-gensyms (stream)
      `(with-open-file (,stream ,filespec ,@options)
         (multiple-value-bind ,vars ,stream
           ,@body))))

As VARS is always a list, we can use MULTIPLE-VALUE-BIND in case additional variables are passed.

Now, using WITH:

  (with ((file (my-file "~/file.txt" :direction :output)))
    (print "Hey!" file))

Finally, note that we put a docstring when we defined MY-FILE. We can retrieve it with DOCUMENTATION:

  (documentation 'my-file 'with)  ;; --> "Open a file."
`````

<a id="FUNCTION:CLITH:WITH"></a>
<a id="FUNCTION:CLITH-DOCS:TAG61"></a>
#### Macro: clith\:with \(bindings \&body body\)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] (with-expansion args*))
  vars             ::= symbol | (symbol*)
  with-expansion   ::= symbol
  args             ::= form

WITH accepts a list of binding clauses. Each binding clause must be a list. The variables are optional, so we can have as clauses lists with one or two elements:

  - A list with one element: That element is a form that must be a WITH expansion defined with DEFWITH.
    In this case, the WITH expansion will receive NIL as the list of variables to be bound.
      
      (with (((foo arg))) ; <- expanded using the expansion of FOO.
        ...)

  - A list with two elements: The first element must be a symbol or a list of symbols.
    The second element is a form that must be a WITH expansion.

      (with (((var1 var2) (bar arg1 arg2)))  ; <- VAR1 and VAR2 are bound with the values from
                                                  the WITH expansion BAR.
        ...)

In order to define a WITH expansion you must use DEFWITH.
`````

<a id="FUNCTION:CLITH:WITHP"></a>
<a id="FUNCTION:CLITH-DOCS:TAG63"></a>
#### Function: clith\:withp \(sym\)

`````text
Checks wether a symbol denotes a WITH expansion.
`````


<a id="TITLE:CLITH-DOCS:CL-SYMBOLS"></a>
## Built\-in WITH expansions

The following Common Lisp functions have a ```WITH expansion```\:

* ```make-broadcast-stream```
* ```make-concatenated-stream```
* ```make-echo-stream```
* ```make-string-input-stream```
* ```make-string-output-stream```
* ```make-synonym-stream```
* ```make-two-way-stream```
* ```open```
