<a id="TITLE:CLITH-DOCS:REFERENCE"></a>
# Reference

* [API reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:TAG1)
* [WITH expansions](/docs/scribble/reference.md#TITLE:CLITH-DOCS:TAG5)
  * [Common Lisp symbols](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-SYMBOLS)
  * [Common Lisp WITH\- macros](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-MACROS)



<a id="TITLE:CLITH-DOCS:TAG1"></a>
## API reference

* [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG4)
* [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG2)
* [clith\:withp](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG3)


<a id="FUNCTION:CLITH:DEFWITH"></a>
<a id="FUNCTION:CLITH-DOCS:TAG4"></a>
#### Macro: clith\:defwith \(name \(vars args \&rest with\-body\) \&body body\)

`````text
Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. This macro has
the following syntax:

  (DEFWITH name (vars args with-body*) declaration* body*)

  name             ::= symbol
  vars             ::= (var-with-options*)
  var-with-options ::= symbol | (symbol option*)
  option           ::= form
  args             ::= destructuring-lambda-list
  with-body        ::= form
  declaration      ::= declaration-form | docstring
  body             ::= form

The symbol NAME will be available to use inside WITH performing a custom expansion defined by DEFWITH.
The variables to be bound are passed through VARS (VARS will always be a list) and the arguments passed
to NAME are bound to ARGS. Finally, WITH-BODY is bound to the body of the WITH macro. Note that WITH-BODY
can contain declarations.

As an example, let's define the with expansion MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

  (defwith my-file (vars (filespec &rest options) &body body)
    "Open a file."
    (with-gensyms (stream)
      `(with-open-file (,stream ,filespec ,@options)
         (multiple-value-bind ,vars ,stream
           ,@body))))

As VARS is always a list, we can use MULTIPLE-VALUE-BIND in case additional variables are passed.
Also, we are assuming here that no additional options are passed with the variables to be bound.

Now, using WITH:

  (with ((file (my-file "~/file.txt" :direction :output)))
    (print "Hey!" file))

Finally, note that we put a docstring in MY-FILE. We can retrieve it with DOCUMENTATION:

  (documentation 'my-file 'with)  ;; --> "Open a file."
`````

<a id="FUNCTION:CLITH:WITH"></a>
<a id="FUNCTION:CLITH-DOCS:TAG2"></a>
#### Macro: clith\:with \(bindings \&body body\)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] form)
  vars             ::= var | (var-with-options*)
  var-with-options ::= var | (var var-option*)
  var-option       ::= form

WITH accepts a list of binding clauses. Each binding clause must be a list. The variables are optional, so we can as clauses lists with one or two elements:

  - A list with one element: That element is a form that must be a WITH expansion defined with DEFWITH.
    In this case, the WITH expansion will receive NIL as the list of variables to be bound.
      
      (with (((my-function arg))) ; <- expanded using the expansion of my-function
        ...)

  - A list with two elements: The first element must be a symbol or a list of symbols with or without options.
    The second element is a form that must be a WITH expansion.

      (with (((member1 (myvar member2))  (slots object))  ; <- MEMBER1 and MYVAR are bound with the values from
                                                               the class members MEMBER1 and MEMBER2 of OBJECT
        ...)

    Here, MEMBER2 is an option of MYVAR. Options can be of any form, not just symbols. As SLOTS is a
    with expansion defined with DEFWITH, it will receive (MEMBER1 (MYVAR MEMBER2)) as the variables to be bound,
    but only MEMBER1 and MYVAR must/should be bound.

In order to define a WITH expansion you must use DEFWITH.
`````

<a id="FUNCTION:CLITH:WITHP"></a>
<a id="FUNCTION:CLITH-DOCS:TAG3"></a>
#### Function: clith\:withp \(sym\)

`````text
Checks wether a symbol denotes a WITH expansion.
`````


<a id="TITLE:CLITH-DOCS:TAG5"></a>
## WITH expansions

<a id="TITLE:CLITH-DOCS:CL-SYMBOLS"></a>
### Common Lisp symbols

The following Common Lisp functions have a ```WITH expansion```\:

* ```make-broadcast-stream```
* ```make-concatenated-stream```
* ```make-echo-stream```
* ```make-string-input-stream```
* ```make-string-output-stream```
* ```make-synonym-stream```
* ```make-two-way-stream```
* ```open```


<a id="TITLE:CLITH-DOCS:CL-MACROS"></a>
### Common Lisp WITH\- macros

The following symbols have a ```WITH expansion```\. They are listed with their respective ```WITH-``` macro\:

<table>
<tr>
<td>accessors</td><td>with-accessors</td></tr><tr>
<td>compilation-unit</td><td>with-compilation-unit</td></tr><tr>
<td>condition-restarts</td><td>with-condition-restarts</td></tr><tr>
<td>hash-table-iterator</td><td>with-hash-table-iterator</td></tr><tr>
<td>input-from-string</td><td>with-input-from-string</td></tr><tr>
<td>output-to-string</td><td>with-output-to-string</td></tr><tr>
<td>package-iterator</td><td>with-package-iterator</td></tr><tr>
<td>simple-restart</td><td>with-simple-restart</td></tr><tr>
<td>slots</td><td>with-slots</td></tr><tr>
<td>standard-io-syntax</td><td>with-standard-io-syntax</td></tr></table>