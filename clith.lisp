
(in-package #:clith)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *with-expander* '#:with-expander)
  (defvar *lambda-list-property* '#:lambda-list-property)
  (defvar *body-property* '#:body-property))


(defgeneric destroyer (obj)
  (:documentation
   "Destructs an object bound by a LET*-like or MULTIPLE-VALUE-BIND-like form at the end of a WITH macro call.")
  (:method (obj)
    (values)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun check-binding (binding)
    (unless (or (symbolp binding)
                (listp binding))
      (error "CLITH error: Expected a binding form but ~s was found."
             binding))
    (when (listp binding)
      (when (= (length binding) 2)
        (unless (or (symbolp (car binding))
		    (and (listp (car binding))
		         (every #'symbolp (car binding))))
	  (error "CLITH error: The vars to be bound must be a symbol or a list of symbols but ~s was found."
	         (car binding))))
      (when (> (length binding) 2)
        (unless (symbolp (car binding))
          (error "CLITH error: The function name to be bound must be a symbol but ~s was found."
                 (car binding)))
        (unless (and (listp (cadr binding))
		     (every #'symbolp (cadr binding)))
          (error "CLITH error: Expected a list of variables as the function arguments but ~s was found."
                 (cadr binding))))))

  (defun check-bindings (bindings)
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings but ~s was found."
	     bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

  (defun let-binding-p (binding)
    (or (symbolp binding)
        (and (listp binding)
             (= (length binding) 2)
             (symbolp (car binding)))))

  (defun multiple-value-binding-p (binding)
    (and (listp binding)
         (= (length binding) 2)
         (listp (car binding))))

  (defun label-binding-p (binding)
    (and (listp binding)
         (> (length binding) 2)))

  (defun nest-expression-p (binding)
    (and (listp binding)
         (= (length binding) 1)))

  (defun collect-let-bindings (bindings)
    (loop for (binding . rest-bindings) on bindings
          while (let-binding-p binding)
          for last-rest-bindings = rest-bindings
          collect binding into let-bindings
          finally (return (values let-bindings last-rest-bindings))))

  (defun collect-multiple-value-binding (bindings)
    (let ((binding (car bindings))
          (rest-bindings (cdr bindings)))
      (when (multiple-value-binding-p binding)
        (values binding rest-bindings))))

  (defun collect-labels-bindings (bindings)
    (loop for (binding . rest-bindings) on bindings
          while (label-binding-p binding)
          for last-rest-bindings = rest-bindings
          collect binding into labels-bindings
          finally (return (values labels-bindings last-rest-bindings))))

  (defun collect-nest-expressions (bindings)
    (loop for (binding . rest-bindings) on bindings
          while (nest-expression-p binding)
          for last-rest-bindings = rest-bindings
          collect binding into nest-expressions
          finally (return (values nest-expressions last-rest-bindings))))
  
  (defun make-let-form (bindings body)
    (let* ((proper-bindings (remove-if #'symbolp bindings))
           (vars-to-destroy (mapcar #'car proper-bindings)))
      `(let* ,bindings
         ,(if vars-to-destroy
              `(unwind-protect
                    ,body
                 (progn
                   ,@(mapcar (lambda (var)
                               `(destroyer ,var))
                             (reverse vars-to-destroy))))
              body))))

  (defun make-multiple-value-bind-form (binding body)
    `(multiple-value-bind ,@binding
       (unwind-protect
            ,body
         (destroyer ,(caar binding)))))

  (defun make-labels-form (bindings body)
    `(labels ,bindings
       ,body))

  (defun make-nest-form (bindings body)
    (let ((nest-expressions (mapcar #'car bindings)))
      `(uiop:nest ,@nest-expressions ,body)))

  (defun make-with-form (bindings body)
    (cond
      (bindings
       (multiple-value-bind (let-bindings rest-bindings) (collect-let-bindings bindings)
         (when let-bindings
           (return-from make-with-form
             (make-let-form let-bindings (make-with-form rest-bindings body)))))
       (multiple-value-bind (multi-binding rest-bindings) (collect-multiple-value-binding bindings)
         (when multi-binding
           (return-from make-with-form
             (make-multiple-value-bind-form multi-binding (make-with-form rest-bindings body)))))
       (multiple-value-bind (labels-bindings rest-bindings) (collect-labels-bindings bindings)
         (when labels-bindings
           (return-from make-with-form
             (make-labels-form labels-bindings (make-with-form rest-bindings body)))))
       (multiple-value-bind (nest-exprs rest-bindings) (collect-nest-expressions bindings)
         (when nest-exprs
           (return-from make-with-form
             (make-nest-form nest-exprs (make-with-form rest-bindings body))))))
      (t
       `(locally
            ,@body)))))


;; Each binding can be:
;; A symbol: Then it works like (let* (symbol) ...)
;; A list with one element. It works like (uiop:nest element (progn ...))
;; A list with two elements, where the second one is a lisp expression:
;;   - The first element is a symbol. Then it work like (let* ((symbol expr)) ... (with-destructor symbol))
;;   - The first element is a list of symbols. Then it works like (multiple-value-bind symbols expr ... (with-destructor (car symbols)))
;; A list with more than two elements. Then it works like (labels (first-elem second-elem rest-elems) ...)

;; Tell the story about LET, MULTIPLE-VALUE-BIND and UIOP:NEST having and adventure and having the child WITH. Later, they adopt the pet |DESTROYER|
;; When saying they had a pet, show the installation code.
;; When WITH is saying its first words, show the equivalent LET*, MULTIPLE-VALUE-BIND and UIOP:NEST code but with WITH.

(defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* expr*)

  binding                  ::= nest-form | let-form | multiple-value-bind-form | labels-form
  nest-form                ::= (expr)
  let-form                 ::= var | (var expr)
  multiple-value-bind-form ::= ((var*) expr)
  labels-form              ::= (func lambda-list expr+) 
  var                      ::= symbol

WITH is a combination of LET*, MULTIPLE-VALUE-BIND, UIOP:NEST and LABELS. It can bind variables, bind functions
 and nest expressions.

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:
  - A symbol: Works like LET*.
      
      (with (x)
        (setf x 5)
        (print x))
      
      --- Expands to ---
      (let* (x)
        (locally 
          (setf x 5) 
          (print x)))

  - A list with one element: Works like UIOP:NEST.
      
      (with ((x)
             ((let ((k 5)))))
        (print k))

      --- Expands to ---
      (uiop/utility:nest 
        x 
        (let ((k 5))) 
        (locally 
          (print k)))

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
                  (locally 
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
        (locally 
          (hello)))

Artificial example computing fibonacci numbers:

      (with (((a b) (values 0 1))
             (one-step ()
               (let ((aux (+ a b)))
                 (setf a b
                       b aux)))
             ((loop for i from 1 to 10 do (one-step))))
        (declare (optimize (speed 3)))
        (print a))

      --- Expands to ---
      (multiple-value-bind (a b) (values 0 1)
        (unwind-protect
            (labels ((one-step ()
                       (let ((aux (+ a b)))
                         (setf a b
                               b aux))))
              (uiop/utility:nest
                (loop for i from 1 to 10
                      do (one-step))
                (locally 
                  (declare (optimize (speed 3))) 
                  (print a))))
          (destroyer a)))"
  (check-bindings bindings)
  (make-with-form bindings body))


(defmethod destroyer ((obj stream))
  "Closes a stream."
  (close obj))
