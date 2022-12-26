
(in-package "CLW")


(defgeneric with-destructor-arity (object)
  (:method (object)
    (declare (ignore object))
    (values 1)))


(adp:defmacro define-with-destructor-arity (class v)
  (check-type v (or integer list) "an integer or a list of integers")
  (with-gensyms (object)
    `(defmethod with-destructor-arity ((,object ,class))
       (declare (ignore ,object))
       (values ,v))))


(defgeneric make-with-destructor (object))


(adp:defmacro define-with-destructor (class lambda-list &body body)
  (with-gensyms (object)
    `(defmethod make-with-destructor ((,object ,class))
       (declare (ignore ,object))
       (lambda ,lambda-list
	 ,@body))))


(defun with-impl (bindings body)
  (destructuring-bind (bind-var-or-vars values-form) (car bindings)
    (let ((bind-vars (if (symbolp bind-var-or-vars)
			 (list bind-var-or-vars)
			 bind-var-or-vars)))
      (with-gensyms (ret-values)
	`(let ((,ret-values (multiple-value-list ,values-form)))
	   (destructuring-bind (,@bind-vars &rest #1=#:ignore) ,ret-values
	     (declare (ignore #1#))
	     (unwind-protect
		  ,@(if (null (cdr bindings))
			body
			`(,(with-impl (cdr bindings) body)))
	       
	       (apply (make-with-destructor ,(car bind-vars)) ,ret-values))))))))

(adp:defmacro with (bindings &body body)
  (with-impl bindings body))

