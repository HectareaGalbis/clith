
(in-package #:clith)


(define-cl-expander accesors (vars body instance-form)
  `(with-accessors ,vars ,instance-form
     ,@body))

(define-cl-expander compilation-unit (() body &rest options)
  `(with-compilation-unit ,options
     ,@body))

(define-cl-expander condition-restarts (() body condition-form restart-form)
  `(with-condition-restarts ,condition-form ,restart-form
     ,@body))

(define-cl-expander hash-table-iterator ((name) body hash-table)
  `(with-hash-table-iterator (,name ,hash-table)
     ,@body))

(define-cl-expander input-from-string ((var) body string &key index start end)
  `(with-input-from-string (,var ,string :index ,index :start ,start :end ,end)
     ,@body))

(define-cl-expander open-file ((stream) body filespec &rest options)
  `(with-open-file (,stream ,filespec ,@options)
     ,@body))

(define-cl-expander open-stream ((var) body stream)
  `(with-open-stream (,var ,stream)
     ,@body))

(define-cl-expander output-to-string ((var) body &rest args)
  `(with-output-to-string (,var ,@args)
     ,@body))

(define-cl-expander package-iterator ((name) body package-list-form &rest symbol-types)
  `(with-package-iterator (,name ,package-list-form ,@symbol-types)
     ,@body))

(define-cl-expander simple-restart ((name) body format-control &rest format-arguments)
  `(with-simple-restart (,name ,format-control ,@format-arguments)
     ,@body))

(define-cl-expander slots (vars body instance-form)
  `(with-slots ,vars ,instance-form
     ,@body))

(define-cl-expander standard-io-syntax (() body)
  `(with-standard-io-syntax
     ,@body))


(defwith make-broadcast-stream ((stream) (&rest streams) &body body)
  (with-gensyms (stream-sym)
    `(let* ((,stream-sym (make-broadcast-stream ,@streams))
            (,stream ,stream-sym))
       (unwind-protect
            (progn ,@body)
         (close ,stream-sym)))))

(defwith make-concatenated-stream ((stream) (&rest input-streams) &body body)
  (with-gensyms (stream-sym)
    `(let* ((,stream-sym (make-concatenated-stream ,@input-streams))
            (,stream ,stream-sym))
       (unwind-protect
            (progn ,@body)
         (close ,stream-sym)))))

(defwith make-echo-stream ((stream) (input-stream output-stream) &body body)
  (with-gensyms (stream-sym)
    `(let* ((,stream-sym (make-concatenated-stream ,input-stream ,output-stream))
            (,stream ,stream-sym))
       (unwind-protect
            (progn ,@body)
         (close ,stream-sym)))))

(defwith make-string-input-stream ((stream) (string &optional start end) &body body)
  (with-gensyms (stream-sym)
    `(let* ((,stream-sym (make-string-input-stream ,string ,start ,end))
            (,stream ,stream-sym))
       (unwind-protect
            (progn ,@body)
         (close ,stream-sym)))))

(defwith make-string-output-stream ((stream) (&key element-type) &body body)
  (with-gensyms (stream-sym)
    `(let* ((,stream-sym (make-string-output-stream :element-type ,element-type))
            (,stream ,stream-sym))
       (unwind-protect
            (progn ,@body)
         (close ,stream-sym)))))

(defwith make-two-way-stream ((stream) (input-stream output-stream) &body body)
  (with-gensyms (stream-sym)
    `(let* ((,stream-sym (make-two-way-stream ,input-stream ,output-stream))
            (,stream ,stream-sym))
       (unwind-protect
            (progn ,@body)
         (close ,stream-sym)))))

(defwith open ((stream) (filespec &rest options) &body body)
  `(with-open-file (,stream ,filespec ,@options)
     ,@body))
