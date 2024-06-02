<a id="header-adp-github-reference"></a>
# Reference

<a id="function-clith-define-with-expander"></a>
#### Macro: clith:define-with-expander (name (vars with-body &rest args) &body body)

`````text
Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. 
See WITH for more information and some examples.
`````

<a id="function-clith-with"></a>
#### Macro: clith:with (bindings &body body)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding  ::= var | ([vars] form)
  vars     ::= var | (list-var*)
  list-var ::= var | (var var)

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:

  - A symbol: The symbol is bound to NIL.
      
      (with (x) ; <- X is bound to NIL
        ...)

  - A list with one element: That element is a form that will be evaluated unless it is a WITH expander.
      
      (with ((my-function)) ; <- evaluated or expanded
        ...)

  - A list with two elements: The first element must be a symbol or a list of symbols to be bound. The second
    element is a form that will be evaluated or expanded.

      (with ((x 1)                                        ; <- X is bound to 1
             ((a b c) (values 4 5 6)))                    ; <- A, B and C are bound to 4, 5 and 6 respectively.
             ((member1 (myvar member2))  (slots object))  ; <- member1 and myvar are bound with the values from
                                                               the class members member1 and member2 of object
        ...)

These forms are the basic features of WITH. But, if you need even more control of what WITH should do, you
can use expanders. You can define an expander with DEFINE-WITH-EXPANDER.

Suppose we have (MAKE-WINDOW TITLE) and (DESTROY-WINDOW WINDOW). We want to control the expansion of WITH 
in order to use both functions. Let's define the WITH expander:

   (clith:define-with-expander make-window (vars body title)
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