
(in-package #:clith)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *let-expanders* (make-hash-table))
  (defvar *nest-expanders* (make-hash-table)))

(defmacro define-let-expander (name bind-vars with-body (&rest args) &body body)
  "Define a let expander. A let expander controls how a let-like or multiple-value-bind-like form is expanded
when encountered within the macro WITH. See WITH for more information and some example."
  (with-gensyms (func)
    `(progn
       (defmacro ,func (,bind-vars ,with-body ,@args)
         ,@body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *let-expanders*) ',func)))))

(defmacro define-nest-expander (name with-body (&rest args) &body body)
  "Define a nest expander. A nest expander controls how a nest-like form is expanded when encountered within the
macro WITH. See WITH for more information and some example."
  (with-gensyms (func)
    `(progn
       (defmacro ,func (,with-body ,@args)
         ,@body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *nest-expanders*) ',func)))))

(defgeneric destroyer (obj)
  (:documentation
   "Destructs an object bound by a LET*-like or MULTIPLE-VALUE-BIND-like form at the end of a WITH macro call.")
  (:method (obj)
    (declare (ignore obj))
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

  (defun var-declaration-p (id)
    (member id '(dynamic-extent ignore ignorable special type)))

  (defun func-declaration-p (id)
    (member id '(dynamic-extent ignore ignorable inline not-inline ftype)))

  (defun var-func-declaration-p (id)
    (member id '(dynamic-extent ignore ignorable)))
  
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

  (defun extract-declaration-single-func (func declarations)
    (if declarations
        (let* ((current-declaration (car declarations))
               (rest-declarations (cdr declarations))
               (id (car current-declaration))
               (complete-func (if (var-func-declaration-p id)
                                  `(function ,func)
                                  func))
               (funcs (if (eq id 'ftype)
                          (cddr current-declaration)
                          (cdr current-declaration)))
               (func-type (when (eq id 'ftype)
                            (list (cadr current-declaration)))))
          (multiple-value-bind (extracted-declarations body-declarations)
              (extract-declaration-single-func func rest-declarations)
            (if (and (func-declaration-p (car current-declaration))
                     (member complete-func funcs :test #'equal))
                (let* ((new-extracted-declarations (cons `(,id ,@func-type ,complete-func)
                                                         extracted-declarations))
                       (new-body-declarations (if (= (length funcs) 1)
                                                  body-declarations
                                                  (cons `(,id ,@func-type ,@(remove complete-func funcs :test #'equal))
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

  (defun extract-func-declarations (funcs declarations)
    (if funcs
        (let ((func (car funcs))
              (rest-funcs (cdr funcs)))
          (multiple-value-bind (extracted-declarations rest-declarations)
              (extract-func-declarations rest-funcs declarations)
            (multiple-value-bind (new-extracted-declarations new-rest-declarations)
                (extract-declaration-single-func func rest-declarations)
              (values (append extracted-declarations new-extracted-declarations)
                      new-rest-declarations))))
        (values nil declarations)))
  
  (defun let-binding-p (binding)
    (or (symbolp binding)
        (and (listp binding)
             (= (length binding) 2)
             (symbolp (car binding)))))

  (defun let-expander-p (binding)
    (and (listp binding)
         (= (length binding) 2)
         (listp (cadr binding))
         (symbolp (caadr binding))
         (gethash (caadr binding) *let-expanders*)
         t))
  
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

  (defun nest-expander-p (binding)
    (and (nest-expression-p binding)
         (listp (car binding))
         (gethash (caar binding) *nest-expanders*)))

  (defun extract-declarations (body)
    (loop for (possible-declaration . rest-body) on body
          if (and (listp possible-declaration)
                  (eq (car possible-declaration) 'declare))
            append (cdr possible-declaration) into declarations
          else
            return (values declarations (cons possible-declaration rest-body))))
  
  (defun split-declarations (bindings declarations)
    (if (null bindings)
        (values nil declarations)
        (let ((binding (car bindings))
              (rest-bindings (cdr bindings)))
          (multiple-value-bind (binding-declarations rest-declarations)
              (split-declarations rest-bindings declarations)
            (cond
              ((or (let-binding-p binding)
                   (multiple-value-binding-p binding))
               (let ((syms (cond
                             ((symbolp binding)
                              (list binding))
                             ((symbolp (car binding))
                              (list (car binding)))
                             (t
                              (car binding)))))
                 (multiple-value-bind (new-binding-declarations new-rest-declarations)
                     (extract-var-declarations syms rest-declarations)
                   (values (append new-binding-declarations binding-declarations)
                           new-rest-declarations))))
              ((label-binding-p binding)
               (let ((funcs (list (car binding))))
                 (multiple-value-bind (new-binding-declarations new-rest-declarations)
                     (extract-func-declarations funcs rest-declarations)
                   (values (append new-binding-declarations binding-declarations)
                           new-rest-declarations))))
              ((nest-expression-p binding)
               (values binding-declarations
                       rest-declarations)))))))

  (defun collect-next-binding (predicate elements)
    (if (null elements)
        (values nil nil)
        (if (funcall predicate (car elements))
            (values (car elements) (cdr elements) t)
            (values nil elements))))

  (defun collect-following-bindings (predicate elements)
    (multiple-value-bind (element rest-elements) (collect-next-binding predicate elements)
      (if element
          (multiple-value-bind (new-elements new-rest-elements)
              (collect-following-bindings predicate rest-elements)
            (values (cons element new-elements) new-rest-elements))
          (values nil rest-elements))))
  
  (defun collect-let-expander-binding (bindings)
    (collect-next-binding #'let-expander-p bindings))
  
  (defun collect-let-bindings (bindings)
    (collect-following-bindings (lambda (binding)
                                  (and (let-binding-p binding)
                                       (not (let-expander-p binding))))
                                bindings))

  (defun collect-multiple-value-binding (bindings)
    (collect-next-binding #'multiple-value-binding-p bindings))

  (defun collect-labels-bindings (bindings)
    (collect-following-bindings #'label-binding-p bindings))

  (defun collect-nest-expander-binding (bindings)
    (collect-next-binding #'nest-expander-p bindings))
  
  (defun collect-nest-bindings (bindings)
    (collect-following-bindings (lambda (binding)
                                  (and (nest-expression-p binding)
                                       (not (nest-expander-p binding))))
                                bindings))

  (defun make-let-expander-form (binding body declaration)
    (let* ((bind-vars (ensure-list (car binding)))
           (user-func (caadr binding))
           (args (cdadr binding))
           (func (gethash user-func *let-expanders*)))
      (macroexpand-1 `(,func ,bind-vars (,@(when declaration `((declare ,@declaration))) ,body) ,@args))))
  
  (defun make-let-form (bindings body declaration)
    (let* ((proper-bindings (remove-if #'symbolp bindings))
           (vars-to-destroy (mapcar #'car proper-bindings)))
      `(let* ,bindings
         ,@(when declaration
             `((declare ,@declaration)))
         ,(if vars-to-destroy
              `(unwind-protect
                    ,body
                 (progn
                   ,@(mapcar (lambda (var)
                               `(destroyer ,var))
                             (reverse vars-to-destroy))))
              body))))

  (defun make-multiple-value-bind-form (binding body declaration)
    `(multiple-value-bind ,@binding
         ,@(when declaration
             `((declare ,@declaration)))
       (unwind-protect
            ,body
         (destroyer ,(caar binding)))))

  (defun make-labels-form (bindings body declaration)
    `(labels ,bindings
       ,@(when declaration
           `((declare ,@declaration)))
       ,body))

  (defun make-nest-expander-form (binding body declaration)
    (declare (ignore declaration))
    (let* ((user-func (caar binding))
           (args (cdar binding))
           (func (gethash user-func *nest-expanders*)))
      (macroexpand-1 `(,func (,body) ,@args))))

  (defun make-nest-form (bindings body declaration)
    (declare (ignore declaration))
    (let ((nest-expressions (mapcar #'car bindings)))
      `(uiop:nest ,@nest-expressions ,body)))

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
        (macrolet ((try-make-with-binding-form (collect-func make-form-func)
                     (with-gensyms (form-bindings rest-bindings binding-declaration inner-form
                                                  rest-declarations new-rest-declarations)
                       `(multiple-value-bind (,form-bindings ,rest-bindings) (,collect-func bindings)
                          (when ,form-bindings
                            (multiple-value-bind (,inner-form ,rest-declarations)
                                (make-with-form ,rest-bindings body binding-declarations body-declarations)
                              (multiple-value-bind (,binding-declaration ,new-rest-declarations)
                                  (split-declarations (ensure-list ,form-bindings) ,rest-declarations)
                                (return-from make-with-form
                                  (values (,make-form-func ,form-bindings ,inner-form ,binding-declaration)
                                          ,new-rest-declarations)))))))))
          (try-make-with-binding-form collect-let-expander-binding make-let-expander-form)
          (try-make-with-binding-form collect-let-bindings make-let-form)
          (try-make-with-binding-form collect-multiple-value-binding make-multiple-value-bind-form)
          (try-make-with-binding-form collect-labels-bindings make-labels-form)
          (try-make-with-binding-form collect-nest-expander-binding make-nest-expander-form)
          (try-make-with-binding-form collect-nest-bindings make-nest-form)))))


(defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* expr*)

  binding                  ::= nest-form | let-form | multiple-value-bind-form | labels-form
  nest-form                ::= (expr)
  let-form                 ::= var | (var expr)
  multiple-value-bind-form ::= ((var*) expr)
  labels-form              ::= (func lambda-list expr+) 
  var                      ::= symbol
  func                     ::= symbol | (setf symbol)

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
        (progn 
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
        (progn (print k)))

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
                  (progn 
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
        (progn 
          (hello)))

These forms are the basic features of WITH. But, if you need even more control of what WITH should do, you
can use expanders. There are two types: LET-EXPANDERS and NEST-EXPANDERS. You can define an expander with
DEFINE-LET-EXPANDER and DEFINE-NEST-EXPANDER. You can control how the WITH macro is expanded when a let form
or nest form is used.

 - Using a NEST EXPANDER: Suppose we have the functions (INIT-SYSTEM) and (TERMINATE-SYSTEM). We want to use the
   macro this way:

     (with ((init-system))
       (doing-something))

   We want control how the macro WITH expands. Let's define a nest expander:

     (clith:define-nest-expander init-system body ()
       `(progn
          (init-system)
          ,@body
          (terminate-system)))

   The name of the expander should be EQ to the function name in order to not confuse the user. Now, if we use
   the macro WITH:

     (with (((init-system)))
       (do))

     --- Expands to ---
     (progn 
       (init-system) 
       (progn (do)) 
       (terminate-system))

 - Using a LET EXPANDER: Suppose we have (CREATE-WINDOW TITLE) and (DESTROY-WINDOW WINDOW). However,
   the library we are using also export the macro (WITH-WINDOW WINDOW-NAME TITLE &BODY BODY) that is way 
   more efficient than calling CREATE-WINDOW and DESTROY-WINDOW. The idea begind LET EXPANDERS is have both 
   flexibility and power at the same time. First, we can define the method CLITH:DESTROYER as normal:

     (defmethod clith:destroyer ((object window-type))
       (destroy-window object))

   Now the user can define their own function returning a window and WITH will destroy the window automatically.
   This is flexible, but not powerful because we are not using WITH-WINDOW. Let's define a LET-EXPANDER:

     (clith:define-let-expander create-window vars body (title)
       `(with-window ,(car vars) ,title
          ,@body))

   Suppose the user function that creates a window is named CREATE-USER-WINDOW. Now we have both flexibility
   and power:

     (with ((user-window (create-user-window))
            (fast-window (create-window \"Fast window\")))
       (do-something))

     --- Expands to ---
     (let* ((user-window (create-user-window)))
       (unwind-protect
           (with-window fast-window \"fast window\" 
             (progn (do-something)))
         (progn 
           (clith:destroyer user-window))))

   Remember that there are LET forms and MULTIPLE-VALUE-BIND forms. A LET EXPANDER will be used with a
   MULTIPLE-VALUE-BIND form as well, so we need to take care of additional or missing binding variables.
   The recommended way to define a LET EXPANDER is using MULTIPLE-VALUE-BIND somewhere:

     (clith:define-let-expander create-window vars body (title)
       (let ((window-var (gensym)))
         `(with-window ,window-var ,title
            (multiple-value-bind ,vars ,window-var
              ,@body))))

   Now, if we try to bind more that one variable, they will be bound to NIL."
  (check-bindings bindings)
  (multiple-value-bind (declarations actual-body) (extract-declarations body)
    (multiple-value-bind (binding-declarations body-declarations) (split-declarations bindings declarations)
      (make-with-form bindings actual-body binding-declarations body-declarations))))


(defmethod destroyer ((obj stream))
  "Closes a stream."
  (close obj))

(define-let-expander open vars body (&rest args)
  (with-gensyms (file-stream)
    `(with-open-file (,file-stream ,@args)
       (multiple-value-bind ,vars ,file-stream
         ,@body))))
