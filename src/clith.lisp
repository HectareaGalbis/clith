
(in-package #:clith)


(exp:defexpander with)


(defmacro defwith (name (vars args with-body) &body body)
  "Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. This macro has
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
    \"Open a file.\"
    (with-gensyms (stream)
      `(with-open-file (,stream ,filespec ,@options)
         (multiple-value-bind ,vars ,stream
           ,@body))))

As VARS is always a list, we can use MULTIPLE-VALUE-BIND in case additional variables are passed.

Now, using WITH:

  (with ((file (my-file \"~/file.txt\" :direction :output)))
    (print \"Hey!\" file))

Finally, note that we put a docstring when we defined MY-FILE. We can retrieve it with DOCUMENTATION:

  (documentation 'my-file 'with)  ;; --> \"Open a file.\""
  (check-type name symbol)
  (check-type with-body symbol)
  (unless (or (symbolp vars)
              (and (listp vars)
                   (every #'symbolp vars)))
    (error "Expected a symbol or a list of symbols but found: ~s" vars))
  `(exp:defexpansion with ,name (,vars ,args ,with-body)
     ,@body))

(defun withp (sym)
  "Checks wether a symbol denotes a WITH expansion."
  (check-type sym symbol)
  (exp:expansionp 'with sym))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun check-variables (vars)
    "Checks the syntax of binding variables."
    (unless (or (symbolp vars)
                (and (listp vars)
                     (every #'symbolp vars)))
      (error "CLITH error: The vars to be bound must be a symbol or a list of symbols but found: ~s"
             vars)))

  (defun check-form (form)
    "Checks the syntax of the binding form."
    (unless (and (listp form)
                 (exp:expansionp 'with (car form)))
      (error "CLITH error: The form must be a with expansion but found: ~s" form)))
  
  (defun check-binding (binding)
    "Checks the syntax of a binding clause."
    (unless (and (listp binding)
                 (not (null binding))
                 (<= (length binding) 2))
      (error "CLITH error: Expected a binding form but ~s was found." binding))
    (case (length binding)
      (1
       (check-form (car binding)))
      (2
       (check-variables (car binding))
       (check-form (cadr binding)))))

  (defun check-bindings (bindings)
    "Checks the syntax of binding clauses."
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings but ~s was found." bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

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

  (defun canonize-binding (binding)
    "
Transforms BINDING to a form that is more apropiate to process.
In particular, turns BINDING into a list of two elements.
The former element is a list the binding symbols written by user.
The latter is the form that returns the values which variables will be bound to.
"
    (cond
      ((= (length binding) 1)
       (list nil (car binding)))
      ((= (length binding) 2)
       (let ((vars (ensure-list (car binding))))
         (list vars (cadr binding))))))

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
            (let ((syms (car binding)))
              (multiple-value-bind (new-binding-declarations new-rest-declarations)
                  (extract-var-declarations syms rest-declarations)
                (values (append new-binding-declarations binding-declarations)
                        new-rest-declarations)))))))

  (defun expand-with-expansion (macro-name vars args body)
    "Expands a WITH expansion."
    (handler-case (exp:expand 'with `(,macro-name ,vars ,args ,body))
      (error (c)
        (error "Error expanding the WITH expansion ~a:~%~a" macro-name c))))

  (defun make-with-macro-form (binding body declaration)
    "Expands a WITH expansion given its BINDING, its BODY and its DECLARATION."
    (let* ((vars (car binding))
           (macro-name (caadr binding))
           (args (cdadr binding))
           (actual-body (if declaration
                            (cons `(declare ,@declaration) body)
                            body)))
      (list (expand-with-expansion macro-name vars args actual-body))))

  (defun make-with-form (bindings body binding-declarations body-declarations)
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
              (make-with-form rest-bindings body binding-declarations body-declarations)
            (multiple-value-bind (binding-declaration new-rest-declarations)
                (split-declarations (list binding) rest-declarations)
              (values (make-with-macro-form binding inner-form binding-declaration)
                      new-rest-declarations)))))))


(defmacro with (bindings &body body)
  "This macro has the following systax:

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

In order to define a WITH expansion you must use DEFWITH."
  (check-bindings bindings)
  (let ((canonized-bindings (mapcar #'canonize-binding bindings)))
    (multiple-value-bind (declarations actual-body) (extract-declarations body)
      (multiple-value-bind (binding-declarations body-declarations)
          (split-declarations canonized-bindings declarations)
        (let ((with-form (make-with-form canonized-bindings actual-body binding-declarations body-declarations)))
          (if (= (length with-form) 1)
              (car with-form)
              (cons 'progn with-form)))))))
