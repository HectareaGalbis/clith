
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

  ;; (defun extract-var-declarations (vars declaration)
  ;;   (multiple-value-bind (extracted-declarations body-declarations)
  ;;       (extract-var-declarations-aux (cdr declaration))
  ;;     (values `(declare ,@extracted-declarations)
  ;;             `(declare ,@body-declarations))))

  ;; (defun extract-func-declarations (funcs declaration)
  ;;   (multiple-value-bind (extracted-declarations body-declarations)
  ;;       (extract-func-declarations-aux (cdr declaration))
  ;;     (values `(declare ,@extracted-declarations)
  ;;             `(declare ,@body-declarations))))

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
  
  (defun extract-binding-declarations (bindings declarations)
    (if bindings
        (let ((binding (car bindings))
              (rest-bindings (cdr bindings)))
          (multiple-value-bind (extracted-declarations rest-declarations)
              (extract-binding-declarations rest-bindings declarations)
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
                 (multiple-value-bind (new-extracted-declarations new-rest-declarations)
                     (extract-var-declarations syms rest-declarations)
                   (values (cons new-extracted-declarations extracted-declarations)
                           new-rest-declarations))))
              ((label-binding-p binding)
               (let ((funcs (list (car binding))))
                 (multiple-value-bind (new-extracted-declarations new-rest-declarations)
                     (extract-func-declarations funcs rest-declarations)
                   (values (cons new-extracted-declarations extracted-declarations)
                           new-rest-declarations))))
              ((nest-expression-p binding)
               (values (cons nil extracted-declarations)
                       rest-declarations)))))
        (values nil declarations)))

  (defun collect-let-bindings (bindings)
    (loop for (binding . rest-bindings) on bindings
          while (or (symbolp binding)
                    (and (listp binding)
                         (= (length binding) 2)
                         (symbolp (car binding))))
          for last-rest-bindings = rest-bindings
          collect binding into let-bindings
          finally (return (values let-bindings last-rest-bindings))))

  (defun collect-multiple-value-binding (bindings)
    (let ((binding (car bindings))
          (rest-bindings (cdr bindings)))
      (when (and (listp binding)
                 (= (length binding) 2)
                 (listp (car binding)))
        (values binding rest-bindings))))

  (defun collect-labels-bindings (bindings)
    (loop for (binding . rest-bindings) on bindings
          while (and (listp binding)
                     (> (length binding) 2))
          for last-rest-bindings = rest-bindings
          collect binding into labels-bindings
          finally (return (values labels-bindings last-rest-bindings))))

  (defun collect-nest-expressions (bindings)
    (loop for (binding . rest-bindings) on bindings
          while (and (listp binding)
                     (= (length binding) 1))
          for last-rest-bindings = rest-bindings
          collect binding into nest-expressions
          finally (return (values nest-expressions last-rest-bindings))))
  
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

  (defun make-nest-form (bindings body)
    (let ((nest-expressions (mapcar #'car bindings)))
      `(uiop:nest ,@nest-expressions ,body)))

  (defun make-with-form (bindings body binding-declarations body-declarations)
    (let ((binding-declaration (car binding-declarations))
          (rest-binding-declarations (cdr binding-declarations)))
      (cond
        (bindings
         (multiple-value-bind (let-bindings rest-bindings) (collect-let-bindings bindings)
           (when let-bindings
             (return-from make-with-form
               (make-let-form let-bindings (make-with-form rest-bindings body rest-binding-declarations
                                                           body-declarations)
                              binding-declaration))))
         (multiple-value-bind (multi-binding rest-bindings) (collect-multiple-value-binding bindings)
           (when multi-binding
             (return-from make-with-form
               (make-multiple-value-bind-form multi-binding (make-with-form rest-bindings body
                                                                               rest-binding-declarations
                                                                               body-declarations)
                                            binding-declaration))))
         (multiple-value-bind (labels-bindings rest-bindings) (collect-labels-bindings bindings)
           (when labels-bindings
             (return-from make-with-form
               (make-labels-form labels-bindings (make-with-form rest-bindings body rest-binding-declarations
                                                                 body-declarations)
                                 binding-declaration))))
         (multiple-value-bind (nest-exprs rest-bindings) (collect-nest-expressions bindings)
           (when nest-exprs
             (return-from make-with-form
               (make-nest-form nest-exprs (make-with-form rest-bindings body rest-binding-declarations
                                                          body-declarations))))))
        (t
         (if body-declarations
             `(locally
                  (declare ,@body-declarations)
                ,@body)
             `(progn
                ,@body)))))))


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

  (WITH (binding*) declaration expr*)

  binding                  ::= nest-form | let-form | multiple-value-bind-form | labels-form
  nest-form                ::= (expr)
  let-form                 ::= var | (var expr)
  multiple-value-bind-form ::= ((var*) expr)
  labels-form              ::= (func lambda-list expr+) 
  var                      ::= symbol

WITH is a combination of LET*, MULTIPLE-VALUE-BIND, UIOP:NEST and LABELS. It can bind variables, functions and
 nest expressions.

WITH accepts a list of binding clauses. Each binding clause must be a symbol or a list. Depending of what the
clause is, WITH's behaeviour is different:
  - A symbol: Works like LET*.
      
      (with (x)
        (setf x 5)
        (print x))
      
      --- Expands to ---
      (let* (x)
        (setf x 5)
        (print x))

  - A list with one element: Works like UIOP:NEST.
      
      (with ((x)
             ((let ((k 5)))))
        (print k))

      --- Expands to ---
      (UIOP/UTILITY:NEST X 
                         (LET ((K 5))) 
                         (PROGN (PRINT K)))

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

Artificial example computing fibonacci numbers:

      (with (((*a* *b*) (values 0 1))
             (one-step ()
               (let ((aux (+ *a* *b*)))
                 (setf *a* *b*
                       *b* aux)))
             ((loop for i from 1 to 10 do (one-step))))
        (declare (special *a* *b*) (optimize (speed 3)))
        (print *a*))

      --- Expands to ---
      (multiple-value-bind (*a* *b*) (values 0 1)
        (declare (special *b*) (special *a*))
        (unwind-protect
            (labels ((one-step ()
                       (let ((aux (+ *a* *b*)))
                         (setf *a* *b*
                               *b* aux))))
              (uiop/utility:nest (loop for i from 1 to 10
                                       do (one-step))
                                 (locally 
                                   (declare (optimize (speed 3))) 
                                   (print *a*))))
          (destroyer *a*)))"
  (check-bindings bindings)
  (let* ((declarations (when (and body
                                  (listp (car body))
                                  (eq (caar body) 'declare))
                         (cdar body)))
         (real-body (if declarations
                        (cdr body)
                        body)))
    (multiple-value-bind (extracted-declarations body-declarations)
        (extract-binding-declarations bindings declarations)
      (make-with-form bindings real-body extracted-declarations body-declarations))))


(defmethod destroyer ((obj stream))
  "Closes a stream."
  (close obj))
