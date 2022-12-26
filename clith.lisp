
(in-package "CLITH")

(adp:in-file #P"docs/api")


(adp:header "Clith API reference")


(adp:defgeneric make-with-destructor (object)
  (:documentation
   "Generic function that must return a procedure that should destroy an object. That procedure must receive at
least one argument being the object to be destroyed. In particular, it should receive as many arguments as
values the constructor returns."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun check-class (class)
    (unless (symbolp class)
      (error "CLITH error: ~s must be a symbol denoting a class."
	     class))))

(adp:defmacro define-with-destructor (class lambda-list &body body)
  "Syntactic sugar for defining a method for MAKE-WITH-DESTRUCTOR. The fist argument is the class of the object
to be destroyed."
  (check-class class)
  (with-gensyms (object)
    `(defmethod make-with-destructor ((,object ,class))
       (declare (ignore ,object))
       (lambda ,lambda-list
	 ,@body))))


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
	     (car binding))))

  (defun check-bindings (bindings)
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings. Found: ~s"
	     bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

  (defun with-impl (bindings body)
    (if bindings
	(destructuring-bind (bind-var-or-vars values-form) (car bindings)
	  (let ((bind-vars (if (symbolp bind-var-or-vars)
			       (list bind-var-or-vars)
			       bind-var-or-vars)))
	    (with-gensyms (ret-values)
	      `(let ((,ret-values (multiple-value-list ,values-form)))
		 (destructuring-bind (,@bind-vars &rest #1=#:ignore) ,ret-values
		   (declare (ignore #1#))
		   (unwind-protect
			,(with-impl (cdr bindings) body)
		     (apply (make-with-destructor ,(car bind-vars)) ,ret-values)))))))
	`(locally
	     ,@body))))

(adp:defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding     ::= (var-or-vars constructor-form)
  var-or-vars ::= symbol | (symbol+)
  constructor-form ::= form

WITH binds some variables like LET does, but it destroys the bound objects after evaluating the body forms. Each
constructor-form must return at least one object. The first object returned must be of a class type that has
specialized the MAKE-WITH-DESTRUCTOR method. After evaluating the body forms, each value returned by the
constructor-form is received by the destructor created by MAKE-WITH-DESTRUCTOR.

var-or-vars indicates the symbols to be bound with the values returned by constructor-form. It works like 
MULTIPLE-VALUE-BIND, i.e., if there are more symbols than values returned, extra values of NIL are given to the
remaining symbols. If there are more values than symbols, the excess values are discarded."
  (check-bindings bindings)
  (with-impl bindings body))

