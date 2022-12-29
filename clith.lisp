
(in-package "CLITH")

(adp:in-file #P"docs/api")


(adp:header "Clith API reference" api-reference-header)


(defvar *constructor-property* '#:constructor-property)

(defvar *destructor-property* '#:destructor-property)


(defun check-constructor-name (name)
  (unless (symbolp name)
    (error "CLITH error: Expected a symbol but ~s was found."
	   name)))

(defun check-functions (constructor destructor)
    (unless (typep constructor 'function)
      (error "CLITH error: The object ~s is not a function."
	     constructor))
    (unless (typep destructor 'function)
      (error "CLITH error: The object ~s is not a function."
	     destructor)))

(adp:defun defwith (constructor-name constructor destructor)
  "CONSTRUCTOR-NAME must be a symbol. CONSTRUCTOR and DESTRUCTOR must be functions. DEFWITH Defines a
constructor-name for the macro WITH. This will enable the use of a form with the syntax

  (constructor-name arg*)

within the WITH macro. arg* denotes the arguments that CONSTRUCTOR must receive. The DESTRUCTOR must receive the
same number of values that CONSTRUCTOR returns.

If CONSTRUCTOR-NAME had already associated a constructor and a destructor, they are replaced by CONSTRUCTOR and
DESTRUCTOR.

If this form is at top-level, effects will take place at compile time."
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (check-constructor-name constructor-name)
    (check-functions constructor destructor)
    (setf (get constructor-name *constructor-property*) constructor)
    (setf (get constructor-name *destructor-property*) destructor)
    (values constructor-name)))


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
	       (car binding))))
    (let ((call-form (if (= (length binding) 1)
			 (car binding)
			 (cadr binding))))
      (unless (and (listp call-form)
		   (symbolp (car call-form))
		   (get (car call-form) *constructor-property*))
	(error "CLITH error: ~s is not a valid constructor-form."
	       call-form))))

  (defun check-bindings (bindings)
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings. Found: ~s"
	     bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

  (defun make-with (constructor-name vars args body)
    (with-gensyms (results)
      `(let ((,results (multiple-value-list (funcall (get ',constructor-name *constructor-property*) ,@args))))
	 (unwind-protect
	      (multiple-value-bind ,vars (values-list ,results)
		,@body)
	   (apply (get ',constructor-name *destructor-property*) ,results)))))
  
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
			      (cadar bindings)))
	       (constructor-name (car call-form))
	       (args (cdr call-form)))
	  (make-with constructor-name vars args (list (with-impl (cdr bindings) body))))
	`(locally
	     ,@body))))


;; Explicar mejor

(adp:defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] constructor-form)
  vars             ::= symbol | (symbol*)
  constructor-form ::= (constructor-name arg*)
  constructor-name ::= symbol
  arg              ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms. 
Each constructor-name must be a symbol that was used in DEFWITH. 

Firstly, each constructor is called with the values placed after the constructor-name. The returned values are
used to bind the vars. vars are bound as if using MULTIPLE-VALUE-BIND. I.e. if there are more vars than values
returned, extra values of NIL are given to the remaining vars. If there are more values than vars, the excess
values are discarded. After these variables are bound, the forms are evaluated. Finally, each destructor is
called with the values returned by each constructor respectively."
  (check-bindings bindings)
  (with-impl bindings body))

