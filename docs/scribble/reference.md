<a id="TITLE:CLITH-DOCS:REFERENCE"></a>
# Reference

* [API reference](/docs/scribble/reference.md#TITLE:CLITH-DOCS:TAG1)
* [Built\-in WITH expansions](/docs/scribble/reference.md#TITLE:CLITH-DOCS:CL-SYMBOLS)



<a id="TITLE:CLITH-DOCS:TAG1"></a>
## API reference

* [clith\:defwith](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG3)
* [clith\:with](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG2)
* [clith\:withp](/docs/scribble/reference.md#FUNCTION:CLITH-DOCS:TAG4)


<a id="FUNCTION:CLITH:DEFWITH"></a>
<a id="FUNCTION:CLITH-DOCS:TAG3"></a>
#### Macro: clith\:defwith \(name \(vars args with\-body\) \&body body\)

`````text
Define a WITH expansion. A WITH expansion controls how the macro WITH is expanded. This macro has
the following syntax:

  (DEFWITH name (vars args with-body) declaration* body*)

  name             ::= symbol
  vars             ::= symbol | (var-with-options*)
  var-with-options ::= symbol | (symbol option*)
  option           ::= destructuring-lambda-argument
  args             ::= destructuring-lambda-list
  with-body        ::= symbol
  declaration      ::= declaration-form | docstring
  body             ::= form

When using (NAME ARGS*) inside the macro WITH, it will expand to the value returned by DEFWITH.
The variables to be bound are passed through VARS (VARS will always be a list) and the arguments passed
to NAME are bound to ARGS. Finally, WITH-BODY is bound to the body of the WITH macro. Keep in mind that
WITH-BODY can contain declarations.

As an example, let's define the with expansion MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

  (defwith my-file (vars (filespec &rest options) body)
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

Finally, note that we put a docstring when we defined MY-FILE. We can retrieve it with DOCUMENTATION:

  (documentation 'my-file 'with)  ;; --> "Open a file."
`````

<a id="FUNCTION:CLITH:WITH"></a>
<a id="FUNCTION:CLITH-DOCS:TAG2"></a>
#### Macro: clith\:with \(bindings \&body body\)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= symbol | ([vars] form)
  vars             ::= symbol | (var-with-options*)
  var-with-options ::= symbol | (symbol var-option*)
  var-option       ::= form

WITH accepts a list of binding clauses. Each binding clause can be a symbol or a list. Depending on
this, the behaeviour of WITH is slightly different:

  - A symbol: The symbol is bound to NIL.

    (with (x)  ; X is bound to NIL
      ...)

  - A list with one element. That element can be a WITH expansion or not:

    * A WITH expansion: The form is expanded according to DEFWITH. In this case,
      the WITH expansion will receive NIL as the list of variables to be bound.

      (with (((init-video-system)))  ; Possible expansion that should finalize the video system at the end
        ;; Doing video stuff
        )

    * Otherwise: The form is placed untouched. It will be evaluated normally.

      (with (((print 3)))  ; Just prints 3
        ...)

  - A list with two elements: The first element must be a symbol or a list of symbols with
    or without options. The second element is a form that can be a WITH expansion:

    * A WITH expansion: The form is expanded according to DEFWITH.

      (with ((my-file (open "~/my-file.txt")))  ; Expanded to WITH-OPEN-FILE
        ...)

    * Otherwise: The form is placed into a MULTIPLE-VALUE-BIND expression.

      (with ((x 3)
             ((y z) (floor 4 5)))  ; Forms placed into MULTIPLE-VALUE-BIND
        ...)

Binding clauses that uses a WITH expansion accepts an extended syntax. Each variable can have options.
These options should be used inside DEFWITH to control the expansion with better precision:

      (defwith slots (vars (object) body)
        `(with-slots ,vars ,object
           ,@body))

      (defstruct 3d-vector x y z)

      (with ((v (make-3d-vector :x 1 :y 2 :z 3))
             ((x (up y) z) (slots v)))
        (+ x up z))

Macros and symbol-macros are treated specially. If a macro or symbol-macro is used, they
will be expanded with MACROEXPAND-1 and its result is the form, or WITH expansion, this macro uses.
`````

<a id="FUNCTION:CLITH:WITHP"></a>
<a id="FUNCTION:CLITH-DOCS:TAG4"></a>
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
