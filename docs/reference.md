<a id="header-adp-github-reference"></a>
# Reference

<a id="function-clith-destroyer"></a>
#### Generic function: clith:destroyer (obj)

`````text
Destructs an object bound by a LET*-like or MULTIPLE-VALUE-BIND-like form at the end of a WITH macro call.
`````

<a id="function-clith-with"></a>
#### Macro: clith:with (bindings &body body)

`````text
This macro has the following systax:

  (WITH (binding*) declaration expr*)

  binding                  ::= nest-form | let-form | multiple-value-bind-form | labels-form
  nest-form                ::= (expr)
  let-form                 ::= var | (var expr)
  multiple-value-bind-form ::= ((var*) expr)
  labels-form              ::= (func lambda-list expr+) 
  var                      ::= symbol

WITH is a combination of LET*, MULTIPLE-VALUE-BIND, UIOP:NEST and LABELS. It can bind variables, functions and
 nest expressions.

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:
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
               (format t "Hello ~a" name)))
        (hello))

      --- Expands to ---
      (labels ((hello (name)
                 (format t "hello ~a" name)))
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
          (destroyer *a*)))
`````