
(in-package #:clith)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *with-expanders* (make-hash-table))
  (defvar *cl-expanders* (make-hash-table :test 'equal)))

(defmacro define-with-expander (name (vars with-body &rest args) &body body)
  "Define a WITH macro. A WITH macro controls how a WITH binding form is expanded. 
See WITH for more information and some examples."
  (check-type with-body symbol)
  (with-gensyms (func pre-vars pre-args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (flet ((,func (,pre-vars ,with-body ,pre-args)
                (destructuring-bind (,vars ,@args) `(,,pre-vars ,@,pre-args)
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
                                           (= (length x) 2)
                                           (symbolp (car x))
                                           (symbolp (cadr x)))))
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

  ;; (defun func-declaration-p (id)
  ;;   (member id '(dynamic-extent ignore ignorable inline not-inline ftype)))

  ;; (defun var-func-declaration-p (id)
  ;;   (member id '(dynamic-extent ignore ignorable)))
  
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

  ;; (defun extract-declaration-single-func (func declarations)
  ;;   (if declarations
  ;;       (let* ((current-declaration (car declarations))
  ;;              (rest-declarations (cdr declarations))
  ;;              (id (car current-declaration))
  ;;              (complete-func (if (var-func-declaration-p id)
  ;;                                 `(function ,func)
  ;;                                 func))
  ;;              (funcs (if (eq id 'ftype)
  ;;                         (cddr current-declaration)
  ;;                         (cdr current-declaration)))
  ;;              (func-type (when (eq id 'ftype)
  ;;                           (list (cadr current-declaration)))))
  ;;         (multiple-value-bind (extracted-declarations body-declarations)
  ;;             (extract-declaration-single-func func rest-declarations)
  ;;           (if (and (func-declaration-p (car current-declaration))
  ;;                    (member complete-func funcs :test #'equal))
  ;;               (let* ((new-extracted-declarations (cons `(,id ,@func-type ,complete-func)
  ;;                                                        extracted-declarations))
  ;;                      (new-body-declarations (if (= (length funcs) 1)
  ;;                                                 body-declarations
  ;;                                                 (cons `(,id ,@func-type ,@(remove complete-func funcs :test #'equal))
  ;;                                                       body-declarations))))
  ;;                 (values new-extracted-declarations
  ;;                         new-body-declarations))
  ;;               (values extracted-declarations
  ;;                       (cons current-declaration body-declarations)))))
  ;;       (values nil nil)))

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

  ;; (defun extract-func-declarations (funcs declarations)
  ;;   (if funcs
  ;;       (let ((func (car funcs))
  ;;             (rest-funcs (cdr funcs)))
  ;;         (multiple-value-bind (extracted-declarations rest-declarations)
  ;;             (extract-func-declarations rest-funcs declarations)
  ;;           (multiple-value-bind (new-extracted-declarations new-rest-declarations)
  ;;               (extract-declaration-single-func func rest-declarations)
  ;;             (values (append extracted-declarations new-extracted-declarations)
  ;;                     new-rest-declarations))))
  ;;       (values nil declarations)))

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
  
  ;; (defun let-binding-p (binding)
  ;;   (or (symbolp binding)
  ;;       (and (listp binding)
  ;;            (= (length binding) 2)
  ;;            (symbolp (car binding)))))

  ;; (defun let-expander-p (binding)
  ;;   (and (listp binding)
  ;;        (= (length binding) 2)
  ;;        (listp (cadr binding))
  ;;        (symbolp (caadr binding))
  ;;        (gethash (caadr binding) *let-expanders*)
  ;;        t))
  
  ;; (defun multiple-value-binding-p (binding)
  ;;   (and (listp binding)
  ;;        (= (length binding) 2)
  ;;        (listp (car binding))))

  ;; (defun label-binding-p (binding)
  ;;   (and (listp binding)
  ;;        (> (length binding) 2)))

  ;; (defun nest-expression-p (binding)
  ;;   (and (listp binding)
  ;;        (= (length binding) 1)))

  ;; (defun nest-expander-p (binding)
  ;;   (and (nest-expression-p binding)
  ;;        (listp (car binding))
  ;;        (gethash (caar binding) *nest-expanders*)))

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
          (print binding)
          (multiple-value-bind (binding-declarations rest-declarations)
              (split-declarations rest-bindings declarations)
            (let ((syms (mapcar (lambda (x) (car (ensure-list x))) (car binding))))
              (print syms)
              (multiple-value-bind (new-binding-declarations new-rest-declarations)
                  (extract-var-declarations syms rest-declarations)
                (values (append new-binding-declarations binding-declarations)
                        new-rest-declarations)))))))

  ;; (defun collect-next-binding (predicate elements)
  ;;   (if (null elements)
  ;;       (values nil nil)
  ;;       (if (funcall predicate (car elements))
  ;;           (values (car elements) (cdr elements) t)
  ;;           (values nil elements))))

  ;; (defun collect-following-bindings (predicate elements)
  ;;   (multiple-value-bind (element rest-elements) (collect-next-binding predicate elements)
  ;;     (if element
  ;;         (multiple-value-bind (new-elements new-rest-elements)
  ;;             (collect-following-bindings predicate rest-elements)
  ;;           (values (cons element new-elements) new-rest-elements))
  ;;         (values nil rest-elements))))
  
  ;; (defun collect-let-expander-binding (bindings)
  ;;   (collect-next-binding #'let-expander-p bindings))
  
  ;; (defun collect-let-bindings (bindings)
  ;;   (collect-following-bindings (lambda (binding)
  ;;                                 (and (let-binding-p binding)
  ;;                                      (not (let-expander-p binding))))
  ;;                               bindings))

  ;; (defun collect-multiple-value-binding (bindings)
  ;;   (collect-next-binding #'multiple-value-binding-p bindings))

  ;; (defun collect-labels-bindings (bindings)
  ;;   (collect-following-bindings #'label-binding-p bindings))

  ;; (defun collect-nest-expander-binding (bindings)
  ;;   (collect-next-binding #'nest-expander-p bindings))
  
  ;; (defun collect-nest-bindings (bindings)
  ;;   (collect-following-bindings (lambda (binding)
  ;;                                 (and (nest-expression-p binding)
  ;;                                      (not (nest-expander-p binding))))
  ;;                               bindings))

  ;; (defun make-let-expander-form (binding body declaration)
  ;;   (let* ((bind-vars (ensure-list (car binding)))
  ;;          (user-func (caadr binding))
  ;;          (args (cdadr binding))
  ;;          (func (gethash user-func *let-expanders*)))
  ;;     (macroexpand-1 `(,func ,bind-vars (,@(when declaration `((declare ,@declaration))) ,body) ,@args))))
  
  ;; (defun make-let-form (bindings body declaration)
  ;;   (let* ((proper-bindings (remove-if #'symbolp bindings))
  ;;          (vars-to-destroy (mapcar #'car proper-bindings)))
  ;;     `(let* ,bindings
  ;;        ,@(when declaration
  ;;            `((declare ,@declaration)))
  ;;        ,(if vars-to-destroy
  ;;             `(unwind-protect
  ;;                   ,body
  ;;                (progn
  ;;                  ,@(mapcar (lambda (var)
  ;;                              `(destroyer ,var))
  ;;                            (reverse vars-to-destroy))))
  ;;             body))))

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

  ;; (defun make-labels-form (bindings body declaration)
  ;;   `(labels ,bindings
  ;;      ,@(when declaration
  ;;          `((declare ,@declaration)))
  ;;      ,body))

  ;; (defun make-nest-expander-form (binding body declaration)
  ;;   (declare (ignore declaration))
  ;;   (let* ((user-func (caar binding))
  ;;          (args (cdar binding))
  ;;          (func (gethash user-func *nest-expanders*)))
  ;;     (macroexpand-1 `(,func (,body) ,@args))))

  ;; (defun make-nest-form (bindings body declaration)
  ;;   (declare (ignore declaration))
  ;;   (let ((nest-expressions (mapcar #'car bindings)))
  ;;     `(uiop:nest ,@nest-expressions ,body)))


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
                      new-rest-declarations))))))

  
  ;; (defun make-with-form (bindings body binding-declarations body-declarations)
  ;;   (if (null bindings)
  ;;       (if body-declarations
  ;;           (values `(locally
  ;;                        (declare ,@body-declarations)
  ;;                      ,@body)
  ;;                   binding-declarations)
  ;;           (values `(progn
  ;;                      ,@body)
  ;;                   binding-declarations))
  ;;       (macrolet ((try-make-with-binding-form (collect-func make-form-func)
  ;;                    (with-gensyms (form-bindings rest-bindings binding-declaration inner-form
  ;;                                                 rest-declarations new-rest-declarations)
  ;;                      `(multiple-value-bind (,form-bindings ,rest-bindings) (,collect-func bindings)
  ;;                         (when ,form-bindings
  ;;                           (multiple-value-bind (,inner-form ,rest-declarations)
  ;;                               (make-with-form ,rest-bindings body binding-declarations body-declarations)
  ;;                             (multiple-value-bind (,binding-declaration ,new-rest-declarations)
  ;;                                 (split-declarations (ensure-list ,form-bindings) ,rest-declarations)
  ;;                               (return-from make-with-form
  ;;                                 (values (,make-form-func ,form-bindings ,inner-form ,binding-declaration)
  ;;                                         ,new-rest-declarations)))))))))
  ;;         (try-make-with-binding-form collect-let-expander-binding make-let-expander-form)
  ;;         (try-make-with-binding-form collect-let-bindings make-let-form)
  ;;         (try-make-with-binding-form collect-multiple-value-binding make-multiple-value-bind-form)
  ;;         (try-make-with-binding-form collect-labels-bindings make-labels-form)
  ;;         (try-make-with-binding-form collect-nest-expander-binding make-nest-expander-form)
  ;;         (try-make-with-binding-form collect-nest-bindings make-nest-form))))
  
  )


(defmacro with (bindings &body body)
  "This macro has the following systax:

  (WITH (binding*) declaration* form*)

  binding ::= var | ([vars] form)
  vars    ::= var | (var*)

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:

  - A symbol: The symbol is bound to NIL.
      
      (with (x) ; <- X is bound to NIL
        ...)

  - A list with one element: That element is a form that will be evaluated unless it is a WITH expander.
      
      (with ((my-function)) ; <- evaluated or expanded
        ...)

  - A list with two elements: The first element must be a symbol or a list of symbols to be bound. The second
    element is a form that will be evaluated or expanded.

      (with ((x 1) ; <- X is bound to 1
             ((a b c) (values 4 5 6))) ; <- A, B and C are bound to 4, 5 and 6 respectively.
        ...)

These forms are the basic features of WITH. But, if you need even more control of what WITH should do, you
can use expanders. You can define an expander with DEFINE-WITH-EXPANDER.

Suppose we have (MAKE-WINDOW TITLE) and (DESTROY-WINDOW WINDOW). We want to control the expansion of WITH 
in order to use both functions. Let's define the WITH expander:

   (clith:define-with-expander window (vars body title)
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
