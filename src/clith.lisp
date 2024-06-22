
(in-package #:clith)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *with-expanders* (make-hash-table))
  (defvar *cl-expanders* (make-hash-table :test 'equal)))

(defmacro defwith (name ((vars &rest args) &rest with-body) &body body)
  "Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. This macro has
the following syntax:

  (DEFWITH name ((vars args*) with-body-args*) body*)

  name             ::= symbol
  vars             ::= (var-with-options*)
  var-with-options ::= symbol | (symbol option*)
  option           ::= form
  body             ::= form

The symbol NAME will be available to use inside WITH performing a custom expansion defined by DEFWITH.
The variables to be bound are passed through VARS (VARS will always be a list) and the arguments passed
to NAME are bound to ARGS. Finally, WITH-BODY is bound to the body of the WITH macro. Note that WITH-BODY
can contain declarations.

As an example, let's define the with expander MY-FILE. We will make WITH to be expanded to WITH-OPEN-FILE.

  (defwith my-file ((vars filespec &rest options) &body body)
    (with-gensyms (stream)
      `(with-open-file (,stream ,filespec ,@options)
         (multiple-value-bind ,vars ,stream
           ,@body))))

As VARS is always a list, we can use MULTIPLE-VALUE-BIND in case additional variables are passed.
Also, we are assuming here that no additional options are passed with the variables to be bound.

Now, using WITH:

  (with ((file (my-file \"~/file.txt\" :direction :output)))
    (print \"Hey!\" file))
"
  (check-type name symbol)
  (with-gensyms (func pre-vars pre-args pre-with-body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (flet ((,func (,pre-vars ,pre-with-body ,pre-args)
                (destructuring-bind ((,vars ,@args) ,@with-body) `((,,pre-vars ,@,pre-args) ,@,pre-with-body)
                  ,@body)))
         (setf (gethash ',name *with-expanders*) #',func)
         ',name))))

(defmacro define-cl-expander (name (vars with-body &rest args) &body body)
  "Define a cl macro. A cl macro controls how a WITH binding form is expanded when a cl name is encountered.
This is a private macro and the user should not use it."
  (with-gensyms (func pre-vars pre-args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (flet ((,func (,pre-vars ,with-body ,pre-args)
                  (destructuring-bind (,vars ,@args) `(,,pre-vars ,@,pre-args)
                    ,@body)))
         (setf (gethash ,(symbol-name name) *cl-expanders*) #',func)
         ,(symbol-name name)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun check-binding (binding)
    (unless (or (symbolp binding)
                (and (listp binding)
                     (not (null binding))
                     (<= (length binding) 2)))
      (error "CLITH error: Expected a binding form but ~s was found." binding))
    (when (listp binding)
      (when (= (length binding) 2)
        (unless (or (symbolp (car binding))
		    (and (listp (car binding))
		         (every (lambda (x)
                                  (or (symbolp x)
                                      (and (listp x)
                                           (symbolp (car x)))))
                                (car binding))))
	  (error "CLITH error: The vars to be bound must be a symbol or a list where each element is a symbol or a list of two symbols but found: ~s"
	         (car binding))))))

  (defun check-bindings (bindings)
    (unless (listp bindings)
      (error "CLITH error: Expected a list of bindings but ~s was found." bindings))
    (loop for binding in bindings
	  do (check-binding binding)))

  (defun var-declaration-p (id)
    (member id '(dynamic-extent ignore ignorable special type)))
  
  (defun extract-declaration-single-var (var declarations)
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
    (cond
      ((symbolp binding)
       (list (list binding) nil))
      ((= (length binding) 1)
       (list nil (car binding)))
      ((= (length binding) 2)
       (let ((vars (ensure-list (car binding))))
         (list vars (cadr binding))))
      (t (error "Expected a binding but the following was found: ~s" binding))))

  (defun cl-macro-binding-p (canonized-binding)
    (let ((expression (cadr canonized-binding)))
      (and (listp expression)
           (gethash (symbol-name (car expression)) *cl-expanders*)
           t)))
  
  (defun with-macro-binding-p (canonized-binding)
    (let ((expression (cadr canonized-binding)))
      (and (listp expression)
           (gethash (car expression) *with-expanders*)
           t)))

  (defun extract-declarations (body)
    (loop for (possible-declaration . rest-body) on body
          if (and (listp possible-declaration)
                  (eq (car possible-declaration) 'declare))
            append (cdr possible-declaration) into declarations
          else
            return (values declarations (cons possible-declaration rest-body))))
  
  (defun split-declarations (canonized-bindings declarations)
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

  (defun make-with-macro-form (binding body declaration)
    (let* ((vars (car binding))
           (macro-name (caadr binding))
           (args (cdadr binding))
           (func (gethash macro-name *with-expanders*)))
      (apply func `(,vars ,(append (when declaration (list (cons 'declare declaration))) (list body)) ,args))))

  (defun make-cl-macro-form (binding body declaration)
    (let* ((vars (car binding))
           (macro-name (caadr binding))
           (args (cdadr binding))
           (func (gethash (symbol-name macro-name) *cl-expanders*)))
      (apply func `(,vars ,(append (when declaration (list (cons 'declare declaration))) (list body)) ,args))))
  
  (defun make-multiple-value-bind-form (binding body declaration)
    `(multiple-value-bind ,@binding
         ,@(when declaration
             `((declare ,@declaration)))
       ,body))


  (defun make-with-form (bindings body binding-declarations body-declarations)
    (if (null bindings)
        
        (if body-declarations
            (values `(locally
                         (declare ,@body-declarations)
                       ,@body)
                    binding-declarations)
            (values `(progn
                       ,@body)
                    binding-declarations))
        
        (let ((binding (car bindings))
              (rest-bindings (cdr bindings)))
          (multiple-value-bind (inner-form rest-declarations)
              (make-with-form rest-bindings body binding-declarations body-declarations)
            (multiple-value-bind (binding-declaration new-rest-declarations)
                (split-declarations (list binding) rest-declarations)
              (values (cond
                        ((cl-macro-binding-p binding)
                         (make-cl-macro-form binding inner-form binding-declaration))
                        ((with-macro-binding-p binding)
                         (make-with-macro-form binding inner-form binding-declaration))
                        (t
                         (make-multiple-value-bind-form binding inner-form binding-declaration)))
                      new-rest-declarations)))))))


(defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding    ::= var | ([vars] form)
  vars       ::= var | (list-var*)
  list-var   ::= var | (var var-option*)
  var-option ::= form

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:

  - A symbol: The symbol is bound to NIL.
      
      (with (x) ; <- X is bound to NIL
        ...)

  - A list with one element: That element is a form that will be evaluated unless it is a WITH expander. If it
    is a with expander defined with DEFWITH, DEFWITH will receive NIL as the list of variables to be bound.
      
      (with (((my-function arg))) ; <- evaluated or expanded
        ...)

  - A list with two elements: The first element must be a symbol, a list of symbols to be bound, or a list
    of symbols with options. The second element is a form that will be evaluated or expanded.

      (with ((x 1)                                        ; <- X is bound to 1
             ((a b c) (values 4 5 6)))                    ; <- A, B and C are bound to 4, 5 and 6 respectively.
             ((member1 (myvar member2))  (slots object))  ; <- MEMBER1 and MYVAR are bound with the values from
                                                               the class members MEMBER1 and MEMBER2 of OBJECT
        ...)

    Here, MEMBER2 is an option of MYVAR. Options can be of any form, not just symbols. As SLOTS is a
    with expander defined with DEFWITH, it will receive (MEMBER1 (MYVAR MEMBER2)) as the variables to be bound,
    but only MEMBER1 and MYVAR must/should be bound.

These forms are the basic features of WITH. But, if you need even more control of what WITH should do, you
can use expanders. You can define an expander with DEFWITH.

Suppose we have (MAKE-WINDOW TITLE) and (DESTROY-WINDOW WINDOW). We want to control the expansion of WITH 
in order to use both functions. Let's define the WITH expander:

   (defwith make-window ((vars title) &body body)
     (let ((window-var (gensym)))
       `(let ((,window-var (make-window ,title)))
          (multiple-value-bind ,vars ,window-var
            ,@body
            (destroy-window ,window-var)))))

We use MULTIPLE-VALUE-BIND in case the user supply more than 1 variable. Another option could be throw an error.

Now we can use our expander in WITH:

   (with ((my-window (make-window \"My window\")))
     ;; Doing things with the window
     )
 
After the body of WITH is evaluated, MY-WINDOW will be destroyed by DESTROY-WINDOW."
  (check-bindings bindings)
  (let ((canonized-bindings (mapcar #'canonize-binding bindings)))
    (multiple-value-bind (declarations actual-body) (extract-declarations body)
      (multiple-value-bind (binding-declarations body-declarations)
          (split-declarations canonized-bindings declarations)
        (make-with-form canonized-bindings actual-body binding-declarations body-declarations)))))
