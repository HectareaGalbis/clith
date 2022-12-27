
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

(defun defwith (constructor-name constructor destructor)
  (check-constructor-name constructor-name)
  (check-functions constructor destructor)
  (setf (get constructor-name *constructor-property*) constructor)
  (setf (get constructor-name *destructor-property*) destructor))


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
	(error "CLITH error: Expected a symbol or a list of symbols but ~s was found.")))
    (let ((call-form (if (= (length binding) 1)
			 (car binding)
			 (cadr binding))))
      (unless (and (listp call-form)
		   (symbolp (car call-form))
		   (get (car call-form) *constructor-property*))
	(error "CLITH error: ~s is not a valid WITH call form."))))

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
	   (apply (get ',constructor-name *destructor-property*) ,,results)))))
  
  (defun with-impl (bindings body)
    (if bindings
	(let* ((vars (if (= (length (car bindings)) 1)
			 nil
			 (caar bindings)))
	       (call-form (if (= (length (car bindings)) 1)
			      (caar bindings)
			      (cadar bindings)))
	       (constructor-name (car call-form))
	       (args (cdr call-form)))
	  (make-with constructor-name vars args (with-impl (cdr bindings) body)))
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

Each constructor-name must be a symbol that was used in DEFWITH."
  (check-bindings bindings)
  (with-impl bindings body))

