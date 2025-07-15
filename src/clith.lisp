
(in-package #:clith)


(exp:defexpander with)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun check-vars (vars extendedp)
    "
Checks the syntax of variables.
If extendedp, it is accepted to have options after a variable.
I.e. if extendedp, a variable can be a list with a symbol followed by other forms."
    (check-type vars (or symbol list))
    (when (listp vars)
      (loop for var in vars
            if (not extendedp)
              do (check-type var symbol)
            else
              do (check-type var (or symbol list))
                 (when (listp var)
                   (check-type (car var) symbol)))))
  
  (defun check-defwith (name vars with-body)
    (check-type name symbol)
    (check-vars vars t)
    (check-type with-body symbol)))


(defmacro defwith (name (vars args with-body) &body body)
  "Define a WITH expansion. A WITH expansion controls how the macro WITH is expanded. This macro has
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
    \"Open a file.\"
    (with-gensyms (stream)
      `(with-open-file (,stream ,filespec ,@options)
         (multiple-value-bind ,vars ,stream
           ,@body))))

As VARS is always a list, we can use MULTIPLE-VALUE-BIND in case additional variables are passed.
Also, we are assuming here that no additional options are passed with the variables to be bound.

Now, using WITH:

  (with ((file (my-file \"~/file.txt\" :direction :output)))
    (print \"Hey!\" file))

Finally, note that we put a docstring when we defined MY-FILE. We can retrieve it with DOCUMENTATION:

  (documentation 'my-file 'with)  ;; --> \"Open a file.\""
  (check-defwith name vars with-body)
  `(exp:defexpansion with ,name (,vars ,args ,with-body)
     ,@body))

(defun withp (sym)
  "Checks wether a symbol denotes a WITH expansion."
  (check-type sym symbol)
  (exp:expansionp 'with sym))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun expand-binding-expression (expression env)
    "Returns the actual expression to be used inside a binding clause."
    (if (and (listp expression) (eq (car expression) 'with))
        expression
        (macroexpand-1 expression env)))
  
  (defun with-expansion-p (expression env)
    "Checks if a expression is a with expansion."
    (let ((actual-expression (expand-binding-expression expression env)))
      (and (listp actual-expression)
           (exp:expansionp 'with (car actual-expression)))))
  
  (defun check-binding (binding env)
    "Checks the syntax of a binding clause."
    (unless (or (symbolp binding)
                (and (listp binding)
                     (not (null binding))
                     (<= (length binding) 2)))
      (error "CLITH error: Expected a binding form but ~s was found." binding))
    (when (and (listp binding)
               (= (length binding) 2))
      (check-vars (car binding) (with-expansion-p (cadr binding) env))))

  (defun check-bindings (bindings env)
    "Checks the syntax of binding clauses."
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings but ~s was found." bindings))
    (loop for binding in bindings
	  do (check-binding binding env)))

  (defun var-declaration-p (id)
    "Checks if ID is an identifier of a variable declaration."
    (member id '(dynamic-extent ignore ignorable special type)))
  
  (defun extract-declaration-single-var (var declarations)
    "
Splits DECLARATIONS into two lists of declarations.
The former is a list of declarations that applies to VAR.
The latter are the rest of declarations.
"
    (if declarations
        (let* ((current-declaration (car declarations))
               (rest-declarations (cdr declarations))
               (id (car current-declaration))
               (vars (if (eq id 'type)
                         (cddr current-declaration)
                         (cdr current-declaration)))
               (var-type (when (eq id 'type)
                           (list (cadr current-declaration)))))
          (multiple-value-bind (extracted-declarations body-declarations)
              (extract-declaration-single-var var rest-declarations)
            (if (and (var-declaration-p (car current-declaration))
                     (member var vars))
                (let* ((new-extracted-declarations (cons `(,id ,@var-type ,var)
                                                         extracted-declarations))
                       (new-body-declarations (if (= (length vars) 1)
                                                  body-declarations
                                                  (cons `(,id ,@var-type ,@(remove var vars))
                                                        body-declarations))))
                  (values new-extracted-declarations
                          new-body-declarations))
                (values extracted-declarations
                        (cons current-declaration body-declarations)))))
        (values nil nil)))

  (defun extract-var-declarations (vars declarations)
    "
Splits DECLARATIONS into two lists of declarations.
The former is a list of declarations that applies to one variable in VARS.
The latter are the rest of declarations.
"
    (if vars
        (let ((var (car vars))
              (rest-vars (cdr vars)))
          (multiple-value-bind (extracted-declarations rest-declarations)
              (extract-var-declarations rest-vars declarations)
            (multiple-value-bind (new-extracted-declarations new-rest-declarations)
                (extract-declaration-single-var var rest-declarations)
              (values (append extracted-declarations new-extracted-declarations)
                      new-rest-declarations))))
        (values nil declarations)))

  (defun canonize-binding (binding env)
    "
Transforms BINDING to a form that is more apropiate to process.
In particular, turns BINDING into a list of two elements.
The former element is a list of the binding symbols written by user.
The latter is the form that returns the values which variables will be bound to.
"
    (cond
      ((symbolp binding)
       (list (list binding) nil))
      ((= (length binding) 1)
       (list nil (expand-binding-expression (car binding) env)))
      ((= (length binding) 2)
       (let ((vars (ensure-list (car binding))))
         (list vars (expand-binding-expression (cadr binding) env))))))

  (defun extract-declarations (body)
    "Extracts the declarations of BODY."
    (multiple-value-bind (actual-body declarations) (parse-body body)
      (values (mapcan #'cdr declarations) actual-body)))
  
  (defun split-declarations (canonized-bindings declarations)
    "
Splits DECLARATIONS into two lists.
The former is a list of declarations that applies to one of the variables used in CANONIZED-BINDINGS.
The latter are the rest of declarations.
"
    (if (null canonized-bindings)
        (values nil declarations)
        (let ((binding (car canonized-bindings))
              (rest-bindings (cdr canonized-bindings)))
          (multiple-value-bind (binding-declarations rest-declarations)
              (split-declarations rest-bindings declarations)
            (let ((syms (mapcar (lambda (x) (car (ensure-list x))) (car binding))))
              (multiple-value-bind (new-binding-declarations new-rest-declarations)
                  (extract-var-declarations syms rest-declarations)
                (values (append new-binding-declarations binding-declarations)
                        new-rest-declarations)))))))

  (defun with-macro-binding-p (canonized-binding env)
    "Checks if a binding has a custom expansion."
    (with-expansion-p (cadr canonized-binding) env))
  
  (defun expand-with-expansion (macro-name vars args body)
    "Expands a WITH expansion."
    (handler-case (exp:expand 'with `(,macro-name ,vars ,args ,body))
      (error (c)
        (error "Error expanding the WITH expansion ~a:~%~a" macro-name c))))

  (defun make-with-macro-form (binding body declaration strictp env)
    "Expands a WITH expansion given its BINDING, its BODY and its DECLARATION."
    (let ((actual-body (if declaration
                           (cons `(declare ,@declaration) body)
                           body)))
      (let ((vars (car binding)))
        (cond
          ((with-macro-binding-p binding env)
            (let ((macro-name (caadr binding))
                  (args (cdadr binding)))
              (list (expand-with-expansion macro-name vars args actual-body))))
          ((not strictp)
           (let ((expression (cadr binding)))
             (if vars
                 (list `(multiple-value-bind ,vars ,expression
                          ,@actual-body))
                 (cons expression actual-body))))
          (t
           (error "This expression is not a WITH expansion: ~s" (cadr binding)))))))

  (defun make-with-form (bindings body binding-declarations body-declarations strictp env)
    "Returns the WITH macro expansion."
    (if (null bindings)

        (if body-declarations
            (values `((locally
                          (declare ,@body-declarations)
                        ,@body))
                    binding-declarations)
            (values body
                    binding-declarations))

        (let ((binding (car bindings))
              (rest-bindings (cdr bindings)))
          (multiple-value-bind (inner-form rest-declarations)
              (make-with-form rest-bindings body binding-declarations body-declarations strictp env)
            (multiple-value-bind (binding-declaration new-rest-declarations)
                (split-declarations (list binding) rest-declarations)
              (values (make-with-macro-form binding inner-form binding-declaration strictp env)
                      new-rest-declarations))))))

  (defun expand-with (bindings body strictp env)
    (check-bindings bindings env)
    (let ((canonized-bindings (mapcar (lambda (binding) (canonize-binding binding env)) bindings)))
      (multiple-value-bind (declarations actual-body) (extract-declarations body)
        (multiple-value-bind (binding-declarations body-declarations)
            (split-declarations canonized-bindings declarations)
          (let ((with-form (make-with-form canonized-bindings actual-body binding-declarations body-declarations strictp env)))
            (case (length with-form)
              (0 nil)
              (1 (car with-form))
              (t (cons 'progn with-form)))))))))



(defmacro with* (bindings &body body &environment env)
  "This macro has the following systax:

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

  - A list with one element. That element must be a WITH expansion or not:

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

      (with ((my-file (open \"~/my-file.txt\")))  ; Expanded to WITH-OPEN-FILE
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
will be expanded with MACROEXPAND-1 and its result is the form, or WITH expansion, this macro uses."
  (expand-with bindings body nil env))

(defmacro with (bindings &body body &environment env)
  "Same as WITH*, but only WITH expansions are allowed."
  (expand-with bindings body t env))
