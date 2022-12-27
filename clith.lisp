
(in-package "CLITH")

(adp:in-file #P"docs/api")


(adp:header "Clith API reference" api-reference-header)


(adp:defmacro defwith (suffix constructor destructor &optional docstring)
  "This macro has the following syntax:

  (DEFWITH suffix constructor destructor &optional docstring)

  suffix      ::= An interned symbol
  constructor ::= A form that evaluates to a function
  destructor  ::= A form that evaluates to a function
  docstring   ::= NIL | string

Defines a new macro called WITH-suffix with DOCSTRING as documentation string. This new macro has the following
syntax:

  (WITH-suffix vars args decalration* body-expr*)

  vars      ::= symbol | (symbol*)
  args      ::= (form*)
  body-expr ::= form

This new macro binds the symbols in vars with the values returned by constructor. The constructor is called with
the arguments in args. Then the body-exprs are evaluated. Finally, the destructor is called. The destructor will
receive all the values returned by the constructor. 

The binding of vars work the same as MULTIPLE-VALUE-BIND. If there are more vars than values returned, extra
values of NIL are given to the remaining vars. If there are more values than vars, the excess values are
discarded."
  (unless (typep suffix 'symbol)
    (error "CLITH error: Expected a symbol but ~s was found."
	   suffix))
  (unless (symbol-package suffix)
    (error "CLITH error: The symbol ~s is not interned."
	   suffix))
  (unless (typep docstring '(or null string))
    (error "CLITH error: The docstring must be NIL or a string but ~s was found."
	   docstring))
  (let ((name (intern (format nil "WITH-~a" (symbol-name suffix)) (symbol-package suffix)))
	(var (make-symbol "VAR"))
	(args (make-symbol "ARGS"))
	(body (make-symbol "BODY")))
    (with-gensyms (var-list once-constructor once-destructor)
      `(adp:defmacro ,name (,var ,args &body ,body)
	 ,@(when docstring
	     `(,docstring))
	 (let ((,var-list (if (listp ,var)
			      ,var
			      (list ,var))))
	   (once-only ((,once-constructor ,constructor) (,once-destructor ,destructor))
	     `(progn
		(unless (typep ,,once-constructor 'function)
		  (error "CLITH error: The form ~s does not evaluate to a function."
			 ,',constructor))
		(unless (typep ,,once-destructor 'function)
		  (error "CLITH error: The form ~s does not evaluate to a function."
			 ,',destructor))
		(multiple-value-bind ,,var-list (funcall ,,once-constructor ,@,args)
		  (unwind-protect
		       (locally
			   ,@,body)
		    (funcall ,,once-destructor ,@,var-list))))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun check-binding (binding)
    (unless (and (listp binding)
		 (= (length binding) 2))
      (error "CLITH error: Expected a binding, i.e. a list of two elements. Found: ~s"
	     binding))
    (unless (or (symbolp (car binding))
		(and (listp (car binding))
		     (every #'symbolp (car binding))))
      (error "CLITH error: Expected a symbol or a list of symbols as a first element of a binding. Found: ~s"
	     (car binding)))
    (unless (and (listp (cadr binding))
		 (symbolp (caadr binding)))
      (error "CLITH error: The constructor form must be a list starting with a WITH suffix but ~s was found."
	     (cadr binding)))
    (let ((suffix (caadr binding)))
      (unless (and (symbol-package suffix)
		   (find-symbol (format nil "WITH-~a" (symbol-name suffix)) (symbol-package suffix)))
	(error "CLITH error: The symbol ~s is not a valid WITH suffix."
	       (caadr binding)))))

  (defun check-bindings (bindings)
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings. Found: ~s"
	     bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

  (defun with-impl (bindings body)
    (if bindings
	(destructuring-bind (bind-vars (suffix &rest args)) (car bindings)
	  (let ((name (find-symbol (format nil "WITH-~a" (symbol-name suffix)) (symbol-package suffix))))
	    `(,name ,bind-vars ,args
		    ,@(if (null (cdr bindings))
			  body
			  `(,(with-impl (cdr bindings) body))))))
	`(locally
	     ,@body))))

(adp:defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding            ::= (vars suffix-constructor)
  var-or-vars        ::= symbol | (symbol*)
  suffix-constructor ::= (suffix arg*)
  suffix             ::= symbol
  arg                ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms.

Each suffix must be a symbol that was used in DEFWITH to define a 'with macro'. 

WITH expands to nested WITH-suffix forms."
  (check-bindings bindings)
  (with-impl bindings body))

