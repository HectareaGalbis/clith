
(in-package "CLITH")

(adp:in-file #P"docs/api")


(adp:header "Clith API reference" api-reference-header)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *with-constructor* '#:with-constructor)
  (defvar *constructor-property* '#:constructor-property)
  (defvar *destructor-property* '#:destructor-property)

  (defvar *with-expander* '#:with-expander)
  (defvar *lambda-list-property* '#:lambda-list-property)
  (defvar *body-property* '#:body-property)

  (defun check-constructor-name (name)
    (unless (symbolp name)
      (error "CLITH error: Expected a symbol but ~s was found."
	     name))
    (when (get name *with-expander*)
      (warn "CLITH warning: Redefining the 'with expander' ~s as a 'with constructor'."
	    name))))

(defun check-functions (constructor destructor)
    (unless (typep constructor 'function)
      (error "CLITH error: The object ~s is not a function."
	     constructor))
    (unless (typep destructor 'function)
      (error "CLITH error: The object ~s is not a function."
	     destructor)))

(adp:defmacro defwith (constructor-name constructor destructor)
  "CONSTRUCTOR-NAME must be a symbol. CONSTRUCTOR and DESTRUCTOR must be forms that evaluate to a function.
DEFWITH defines a way to destruct an object returned by the function CONSTRUCTOR within the WITH macro. The
DESTRUCTOR must receive the same number of values that CONSTRUCTOR returns.

If CONSTRUCTOR-NAME has already a constructor and a destructor, they are replaced by CONSTRUCTOR and
DESTRUCTOR.

If this form is at top-level, effects will take place at compile time."
  (check-constructor-name constructor-name)
  (once-only (constructor destructor)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (remprop ',constructor-name *with-expander*)
	 (remprop ',constructor-name *lambda-list-property*)
	 (remprop ',constructor-name *body-property*)
	 (setf (get ',constructor-name *with-constructor*) t))
       (check-functions ,constructor ,destructor)
       (setf (get ',constructor-name *constructor-property*) ,constructor)
       (setf (get ',constructor-name *destructor-property*) ,destructor)
       (values ',constructor-name))))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun check-expander-name (name)
    (unless (symbolp name)
      (error "CLITH error: Expected a symbol but ~s was found."
	     name))
    (when (get name *with-constructor*)
      (warn "CLITH warning: Redefining the 'with constructor' ~s as a 'with expander'."
	    name))))

(adp:defmacro define-with-expander (expander-name destructuring-lambda-list &body body)
  "Defines an expander for the WITH macro called EXPANDER-NAME. The DESTRUCTURING-LAMBDA-LIST must receive a
&body or &rest argument. That argument will be the body forms used within the WITH macro. The expander must
return the form that the WITH macro will expand to."
  (check-expander-name expander-name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (remprop ',expander-name *with-constructor*)
       (remprop ',expander-name *constructor-property*)
       (remprop ',expander-name *destructor-property*)
       (setf (get ',expander-name *with-expander*) t)
       (setf (get ',expander-name *lambda-list-property*) ',destructuring-lambda-list)
       (setf (get ',expander-name *body-property*) ',body))
     (values ',expander-name)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun check-binding (binding)
    (unless (and (listp binding)
		 (member (length binding) '(1 2) :test #'=))
      (error "CLITH error: Expected a list of one or two elements but ~s was found."
	     binding))
    (when (= (length binding) 2)
      (unless (or (symbolp (car binding))
		  (and (listp (car binding))
		       (every #'symbolp (car binding))))
	(error "CLITH error: The vars to be bound must be a symbol or a list of symbols but ~s was found."
	       (car binding)))))

  (defun check-bindings (bindings)
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings but ~s was found."
	     bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

  (defun plistp (l)
    (or (null l)
	(and (consp l)
	     (plistp (cdr l)))))
  
  (defun make-with (vars call-form body)
    (let* ((list-form-p (and (plistp call-form)
			     (symbolp (car call-form))))
	   (with-constructor-p (and list-form-p
				    (get (car call-form) *with-constructor*)))
	   (with-expander-p (and list-form-p
				 (get (car call-form) *with-expander*))))
      (cond
	(with-constructor-p
	    (let ((constructor-name (car call-form))
		  (args (cdr call-form)))
	      (with-gensyms (results)
		`(let ((,results (multiple-value-list (funcall (get ',constructor-name *constructor-property*) ,@args))))
		   (unwind-protect
			(multiple-value-bind ,vars (values-list ,results)
			  ,@body)
		     (apply (get ',constructor-name *destructor-property*) ,results))))))
	(with-expander-p
	    (let ((expander-name (car call-form))
		  (args (cdr call-form)))
	      (eval `(destructuring-bind ,(get expander-name *lambda-list-property*) ',(append args body)
		       ,@(get expander-name *body-property*)))))
	(t
	 `(multiple-value-bind ,vars ,call-form
	    ,@body)))))

  
  (defun with-impl (bindings body)
    (if bindings
	(let* ((var-or-vars (if (= (length (car bindings)) 1)
				nil
				(caar bindings)))
	       (vars (if (symbolp var-or-vars)
			 (list var-or-vars)
			 var-or-vars))
	       (call-form (if (= (length (car bindings)) 1)
			      (caar bindings)
			      (cadar bindings))))
	  (if (cdr bindings)
	      (make-with vars call-form (list (with-impl (cdr bindings) body)))
	      (make-with vars call-form body)))
	`(locally
	     ,@body))))


(adp:defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] constructor-form) | (expander-form)
  vars             ::= symbol | (symbol*)
  constructor-form ::= (constructor-name arg*)
  expander-form    ::= (expander-name arg*)
  constructor-name ::= symbol
  expander-name    ::= symbol
  arg              ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms. 
Each constructor-name must be a symbol that was used in DEFWITH. The same goes to expander-name that must be a
symbol used in DEFINE-WITH-EXPANDER. 

Each constructor is called with the values placed after the constructor-name. The returned values are used to
bind the vars. vars are bound as if using MULTIPLE-VALUE-BIND. I.e. if there are more vars than values
returned, extra values of NIL are given to the remaining vars. If there are more values than vars, the excess
values are discarded. After these variables are bound, the forms are evaluated. At the end, each destructor is
called with the values returned by each constructor respectively.

Each expander is expanded like a macro does. They will receive the args plus the body forms of the WITH macro.
Its expansion is what the WITH macro will expand to."
  (check-bindings bindings)
  (with-impl bindings body))

