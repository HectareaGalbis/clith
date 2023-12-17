<a id="header-adp-github-reference"></a>
# Reference

<a id="function-clith-define-let-expander"></a>
#### Macro: clith:define-let-expander (name bind-vars with-body (&rest args) &body body)

`````text
Define a let expander. A let expander controls how a let-like or multiple-value-bind-like form is expanded
when encountered within the macro WITH. See WITH for more information and some example.
`````

<a id="function-clith-define-nest-expander"></a>
#### Macro: clith:define-nest-expander (name with-body (&rest args) &body body)

`````text
Define a nest expander. A nest expander controls how a nest-like form is expanded when encountered within the
macro WITH. See WITH for more information and some example.
`````

<a id="function-clith-destroyer"></a>
#### Generic function: clith:destroyer (obj)

`````text
Destructs an object bound by a LET*-like or MULTIPLE-VALUE-BIND-like form at the end of a WITH macro call.
`````

<a id="function-clith-with"></a>
#### Macro: clith:with (bindings &body body)

`````text
This macro has the following systax:

  (WITH (binding*) declaration* expr*)

  binding                  ::= nest-form | let-form | multiple-value-bind-form | labels-form
  nest-form                ::= (expr)
  let-form                 ::= var | (var expr)
  multiple-value-bind-form ::= ((var*) expr)
  labels-form              ::= (func lambda-list expr+) 
  var                      ::= symbol
  func                     ::= symbol | (setf symbol)

WITH is a combination of LET*, MULTIPLE-VALUE-BIND, UIOP:NEST and LABELS. It can bind variables, bind functions
 and nest expressions.

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:
  - A symbol: Works like LET*.
      
      (with (x)
        (setf x 5)
        (print x))
      
      --- Expands to ---
      (let* (x)
        (progn 
          (setf x 5) 
          (print x)))

  - A list with one element: Works like UIOP:NEST.
      
      (with ((x)
             ((let ((k 5)))))
        (print k))

      --- Expands to ---
      (uiop/utility:nest 
        x 
        (let ((k 5))) 
        (progn (print k)))

  - A list with two elements: Works like LET* or MULTIPLE-VALUE-BIND. Also, the function DESTROYER is called
    with the bound variables at the end of the WITH macro. Using the MULTIPLE-VALUE-BIND form will result in
    calling the DESTROYER function only with the first bound variable. 

      (with ((x 1)
             ((a b c) (values 4 5 6)))
        (print x))

      --- Expands to ---
      (let* ((x 1))
        (unwind-protect
            (multiple-value-bind (a b c) (values 4 5 6)
              (unwind-protect 
                  (progn 
                    (print x)) 
                (destroyer a)))
          (progn 
            (destroyer x))))

  - A list with at least three elements. Works like LABELS.

      (with ((hello (name)
               (format t "Hello ~a" name)))
        (hello))

      --- Expands to ---
      (labels ((hello (name)
                 (format t "hello ~a" name)))
        (progn 
          (hello)))

These forms are the basic features of WITH. But, if you need even more control of what WITH should do, you
can use expanders. There are two types: LET-EXPANDERS and NEST-EXPANDERS. You can define an expander with
DEFINE-LET-EXPANDER and DEFINE-NEST-EXPANDER. You can control how the WITH macro is expanded when a let form
or nest form is used.

 - Using a NEST EXPANDER: Suppose we have the functions (INIT-SYSTEM) and (TERMINATE-SYSTEM). We want to use the
   macro this way:

     (with ((init-system))
       (doing-something))

   We want control how the macro WITH expands. Let's define a nest expander:

     (clith:define-nest-expander init-system body ()
       `(progn
          (init-system)
          ,@body
          (terminate-system)))

   The name of the expander should be EQ to the function name in order to not confuse the user. Now, if we use
   the macro WITH:

     (with (((init-system)))
       (do))

     --- Expands to ---
     (progn 
       (init-system) 
       (progn (do)) 
       (terminate-system))

 - Using a LET EXPANDER: Suppose we have (CREATE-WINDOW TITLE) and (DESTROY-WINDOW WINDOW). However,
   the library we are using also export the macro (WITH-WINDOW WINDOW-NAME TITLE &BODY BODY) that is way 
   more efficient than calling CREATE-WINDOW and DESTROY-WINDOW. The idea begind LET EXPANDERS is have both 
   flexibility and power at the same time. First, we can define the method CLITH:DESTROYER as normal:

     (defmethod clith:destroyer ((object window-type))
       (destroy-window object))

   Now the user can define their own function returning a window and WITH will destroy the window automatically.
   This is flexible, but not powerful because we are not using WITH-WINDOW. Let's define a LET-EXPANDER:

     (clith:define-let-expander create-window vars body (title)
       `(with-window ,(car vars) ,title
          ,@body))

   Suppose the user function that creates a window is named CREATE-USER-WINDOW. Now we have both flexibility
   and power:

     (with ((user-window (create-user-window))
            (fast-window (create-window "Fast window")))
       (do-something))

     --- Expands to ---
     (let* ((user-window (create-user-window)))
       (unwind-protect
           (with-window fast-window "fast window" 
             (progn (do-something)))
         (progn 
           (clith:destroyer user-window))))

   Remember that there are LET forms and MULTIPLE-VALUE-BIND forms. A LET EXPANDER will be used with a
   MULTIPLE-VALUE-BIND form as well, so we need to take care of additional or missing binding variables.
   The recommended way to define a LET EXPANDER is using MULTIPLE-VALUE-BIND somewhere:

     (clith:define-let-expander create-window vars body (title)
       (let ((window-var (gensym)))
         `(with-window ,window-var ,title
            (multiple-value-bind ,vars ,window-var
              ,@body))))

   Now, if we try to bind more that one variable, they will be bound to NIL.
`````