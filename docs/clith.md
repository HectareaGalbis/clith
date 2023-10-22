<h1 id="HEADER:CLITH:API-REFERENCE-HEADER">Clith API reference</h1>



<h3 id="HEADER:ADP-GITHUB:HEADERTAG614">DESTROYER</h3>



<h4 id="FUNCTION:CLITH:DESTROYER">Generic function: DESTROYER</h4>

```common-lisp
(DEFGENERIC CLITH:DESTROYER
    (OBJ)
  (:DOCUMENTATION
   "Destructs an object bound by a LET*-like or MULTIPLE-VALUE-BIND-like form at the end of a WITH macro call.")
  (:METHOD (OBJ) (VALUES)))
```

<h3 id="HEADER:ADP-GITHUB:HEADERTAG615">With</h3>



<h4 id="FUNCTION:CLITH:WITH">Macro: WITH</h4>

```common-lisp
(DEFMACRO CLITH:WITH (BINDINGS &BODY BODY)
  "This macro has the following systax:

  (WITH (binding*) declaration expr*)

  binding                  ::= nest-form | let-form | multiple-value-bind-form | labels-form
  nest-form                ::= (expr)
  let-form                 ::= var | (var expr)
  multiple-value-bind-form ::= ((var*) expr)
  labels-form              ::= (func lambda-list expr+) 
  var                      ::= symbol

WITH is a combination of LET*, MULTIPLE-VALUE-BIND, UIOP:NEST and LABELS. It can bind variables, functions and nest expressions.

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the clause is, WITH's behaeviour is different:
  - A symbol: Works like LET*.
      
      (with (x)
        (setf x 5)
        (print x))
      
      --- Expands to ---
      (let* (x)
        (setf x 5)
        (print x))

  - A list with one element: Works like UIOP:NEST.
      
      (with ((x)
             ((let ((k 5)))))
        (print k))

      --- Expands to ---
      (UIOP/UTILITY:NEST X 
                         (LET ((K 5))) 
                         (PROGN (PRINT K)))

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
               (format t \"Hello ~a\" name)))
        (hello))

      --- Expands to ---
      (labels ((hello (name)
                 (format t \"hello ~a\" name)))
        (progn 
          (hello)))

Artificial example computing fibonacci numbers:

      (with (((*a* *b*) (values 0 1))
             (one-step ()
               (let ((aux (+ *a* *b*)))
                 (setf *a* *b*
                       *b* aux)))
             ((loop for i from 1 to 10 do (one-step))))
        (declare (special *a* *b*) (optimize (speed 3)))
        (print *a*))

      --- Expands to ---
      (multiple-value-bind (*a* *b*) (values 0 1)
        (declare (special *b*) (special *a*))
        (unwind-protect
            (labels ((one-step ()
                       (let ((aux (+ *a* *b*)))
                         (setf *a* *b*
                               *b* aux))))
              (uiop/utility:nest (loop for i from 1 to 10
                                       do (one-step))
                                 (locally 
                                   (declare (optimize (speed 3))) 
                                   (print *a*))))
          (destroyer *a*)))"
  (CHECK-BINDINGS BINDINGS)
  (LET* ((DECLARATIONS
          (WHEN (AND BODY (LISTP (CAR BODY)) (EQ (CAAR BODY) 'DECLARE))
            (CDAR BODY)))
         (REAL-BODY
          (IF DECLARATIONS
              (CDR BODY)
              BODY)))
    (MULTIPLE-VALUE-BIND (EXTRACTED-DECLARATIONS BODY-DECLARATIONS)
        (EXTRACT-BINDING-DECLARATIONS BINDINGS DECLARATIONS)
      (MAKE-WITH-FORM BINDINGS REAL-BODY EXTRACTED-DECLARATIONS
                      BODY-DECLARATIONS))))
```

<h3 id="HEADER:ADP-GITHUB:HEADERTAG616">Predefined \'DESTROYERS\'</h3>



<h4>Method: DESTROYER</h4>

```common-lisp
(DEFMETHOD CLITH:DESTROYER ((OBJ STREAM)) "Closes a stream." (CLOSE OBJ))
```

