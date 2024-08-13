<a id="header-clith-docs-reference"></a>
# Reference

<a id="function-clith-defwith"></a>
#### Macro: clith:defwith (name (vars args &rest with-body) &body body)

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

As an example, let's define the with expander MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

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

<a id="function-clith-with"></a>
#### Macro: clith:with (bindings &body body)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] form)
  vars             ::= var | (var-with-options*)
  var-with-options ::= var | (var var-option*)
  var-option       ::= form

WITH accepts a list of binding clauses. Each binding clause must be a list. The variables are optional, so we can as clauses lists with one or two elements:

  - A list with one element: That element is a form that must be a WITH expander defined with DEFWITH.
    In this case, the WITH expander will receive NIL as the list of variables to be bound.
      
      (with (((my-function arg))) ; <- expanded using the expansion of my-function
        ...)

  - A list with two elements: The first element must be a symbol or a list of symbols with or without options.
    The second element is a form that must be a WITH expander.

      (with (((member1 (myvar member2))  (slots object))  ; <- MEMBER1 and MYVAR are bound with the values from
                                                               the class members MEMBER1 and MEMBER2 of OBJECT
        ...)

    Here, MEMBER2 is an option of MYVAR. Options can be of any form, not just symbols. As SLOTS is a
    with expander defined with DEFWITH, it will receive (MEMBER1 (MYVAR MEMBER2)) as the variables to be bound,
    but only MEMBER1 and MYVAR must/should be bound.

In order to define a WITH expander you must use DEFWITH.
`````