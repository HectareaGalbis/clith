
(in-package #:clith)


(define-cl-expander accesors (vars body instance-form)
  `(with-accessors ,vars ,instance-form
     ,@body))

(define-cl-expander compilation-unit (vars body &rest options)
  `(with-compilation-unit ,options
     (multiple-value-bind ,vars nil
       ,@body)))

(define-cl-expander condition-restarts (vars body condition-form restart-form)
  `(with-condition-restarts ,condition-form ,restart-form
     (multiple-value-bind ,vars nil
       ,@body)))

(define-cl-expander hash-table-iterator (vars body hash-table)
  (with-gensyms (name)
    `(with-hash-table-iterator (,name ,hash-table)
       (multiple-value-bind ,vars ,name
         ,@body))))

(define-cl-expander input-from-string (vars body string &key index start end)
  (with-gensyms (var)
    `(with-input-from-string (,var ,string :index ,index :start ,start :end ,end)
       (multiple-value-bind ,vars ,var
         ,@body))))

(define-cl-expander open-file (vars body filespec &rest options)
  (with-gensyms (stream)
    `(with-open-file (,stream ,filespec ,@options)
       (multiple-value-bind ,vars ,stream
         ,@body))))

(define-cl-expander open-stream (vars body stream)
  (with-gensyms (var)
    `(with-open-stream (,var ,stream)
       (multiple-value-bind ,vars ,var
         ,@body))))

(define-cl-expander output-to-string (vars body &rest args)
  (with-gensyms (var)
    `(with-output-to-string (,var ,@args)
       (multiple-value-bind ,vars ,var
         ,@body))))

(define-cl-expander package-iterator (vars body package-list-form &rest symbol-types)
  (with-gensyms (name)
    `(with-package-iterator (,name ,package-list-form ,@symbol-types)
       (multiple-value-bind ,vars ,name
         ,@body))))

(define-cl-expander simple-restart (vars body format-control &rest format-arguments)
  (with-gensyms (name)
    `(with-simple-restart (,name ,format-control ,@format-arguments)
       (multiple-value-bind ,vars ,name
         ,@body))))

(define-cl-expander slots (vars body instance-form)
  `(with-slots ,vars ,instance-form
     ,@body))

(define-cl-expander standard-io-syntax (vars body)
  `(with-standard-io-syntax
     (multiple-value-bind ,vars nil
       ,@body)))
