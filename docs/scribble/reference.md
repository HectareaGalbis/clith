<a id="header-adp-github-reference"></a>
# Reference

<a id="function-clith-defwith"></a>
#### Macro: clith:defwith (name (vars args &rest with-body) &body body)

`````text
Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. This macro has
the following syntax:

  (DEFWITH name (vars args with-body-args*) body*)

  name             ::= symbol
  vars             ::= (var-with-options*)
  var-with-options ::= symbol | (symbol option*)
  option           ::= form
  args             ::= destructuring-lambda-list
  body             ::= form

The symbol NAME will be available to use inside WITH performing a custom expansion defined by DEFWITH.
The variables to be bound are passed through VARS (VARS will always be a list) and the arguments passed
to NAME are bound to ARGS. Finally, WITH-BODY is bound to the body of the WITH macro. Note that WITH-BODY
can contain declarations.

As an example, let's define the with expander MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

  (defwith my-file (vars (filespec &rest options) &body body)
    (with-gensyms (stream)
      `(with-open-file (,stream ,filespec ,@options)
         (multiple-value-bind ,vars ,stream
           ,@body))))

As VARS is always a list, we can use MULTIPLE-VALUE-BIND in case additional variables are passed.
Also, we are assuming here that no additional options are passed with the variables to be bound.

Now, using WITH:

  (with ((file (my-file "~/file.txt" :direction :output)))
    (print "Hey!" file))

`````

<a id="function-clith-with"></a>
#### Macro: clith:with (bindings &body body)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding    ::= var | ([vars] form)
  vars       ::= var | (list-var*)
  list-var   ::= var | (var var-option*)
  var-option ::= form

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:

  - A symbol: The symbol is bound to NIL.
      
      (with (x) ; <- X is bound to NIL
        ...)

  - A list with one element: That element is a form that will be evaluated unless it is a WITH expander. If it
    is a with expander defined with DEFWITH, DEFWITH will receive NIL as the list of variables to be bound.
      
      (with (((my-function arg))) ; <- evaluated or expanded
        ...)

  - A list with two elements: The first element must be a symbol, a list of symbols to be bound, or a list
    of symbols with options. The second element is a form that will be evaluated or expanded.

      (with ((x 1)                                        ; <- X is bound to 1
             ((a b c) (values 4 5 6)))                    ; <- A, B and C are bound to 4, 5 and 6 respectively.
             ((member1 (myvar member2))  (slots object))  ; <- MEMBER1 and MYVAR are bound with the values from
                                                               the class members MEMBER1 and MEMBER2 of OBJECT
        ...)

    Here, MEMBER2 is an option of MYVAR. Options can be of any form, not just symbols. As SLOTS is a
    with expander defined with DEFWITH, it will receive (MEMBER1 (MYVAR MEMBER2)) as the variables to be bound,
    but only MEMBER1 and MYVAR must/should be bound.

These forms are the basic features of WITH. But, if you need even more control of what WITH should do, you
can use expanders. You can define an expander with DEFWITH.

Suppose we have (MAKE-WINDOW TITLE) and (DESTROY-WINDOW WINDOW). We want to control the expansion of WITH 
in order to use both functions. Let's define the WITH expander:

   (defwith make-window (vars (title) &body body)
     (let ((window-var (gensym)))
       `(let ((,window-var (make-window ,title)))
          (multiple-value-bind ,vars ,window-var
            ,@body
            (destroy-window ,window-var)))))

We use MULTIPLE-VALUE-BIND in case the user supply more than 1 variable. Another option could be throw an error.

Now we can use our expander in WITH:

   (with ((my-window (make-window "My window")))
     ;; Doing things with the window
     )
 
After the body of WITH is evaluated, MY-WINDOW will be destroyed by DESTROY-WINDOW.
`````