
(in-package #:clith)


(defwith accessors ((&rest slot-entry) (instance-form) &body body)
  `(with-accessors (,@slot-entry) ,instance-form
     ,@body))

(defwith compilation-unit (() (&rest options) &body body)
  `(with-compilation-unit ,options
     ,@body))

(defwith condition-restarts (() (condition-form restart-form) &body body)
  `(with-condition-restarts ,condition-form ,restart-form
     ,@body))

(defwith hash-table-iterator ((name) (hash-table) &body body)
  `(with-hash-table-iterator (,name ,hash-table)
     ,@body))

(defwith input-from-string ((var) (string &key index start end) &body body)
  `(with-input-from-string (,var ,string :index ,index :start ,start :end ,end)
     ,@body))

(defwith output-to-string ((var) (&rest args) &body body)
  `(with-output-to-string (,var ,@args)
     ,@body))

(defwith package-iterator ((name) (package-list-form &rest symbol-types) &body body)
  `(with-package-iterator (,name ,package-list-form ,@symbol-types)
     ,@body))

(defwith simple-restart ((name) (format-control &rest format-arguments) &body body)
  `(with-simple-restart (,name ,format-control ,@format-arguments)
     ,@body))

(defwith slots ((&rest slot-entry) (instance-form) &body body)
  `(with-slots (,@slot-entry) ,instance-form
     ,@body))

(defwith standard-io-syntax (() () &body body)
  `(with-standard-io-syntax
     ,@body))


(defwith cl:make-broadcast-stream ((stream) (&rest streams) &body body)
  `(with-open-stream (,stream (make-broadcast-stream ,@streams))
     ,@body))

(defwith cl:make-concatenated-stream ((stream) (&rest input-streams) &body body)
  `(with-open-stream (,stream (make-concatenated-stream ,@input-streams))
     ,@body))

(defwith cl:make-echo-stream ((stream) (input-stream output-stream) &body body)
  `(with-open-stream (,stream (make-concatenated-stream ,input-stream ,output-stream))
     ,@body))

(defwith cl:make-string-input-stream ((stream) (string &optional start end) &body body)
  `(with-open-stream (,stream (make-string-input-stream ,string ,start ,end))
     ,@body))

(defwith cl:make-string-output-stream ((stream) (&key element-type) &body body)
  `(with-open-stream (,stream (make-string-output-stream :element-type ,element-type))
     ,@body))

(defwith cl:make-synonym-stream ((stream) (symbol) &body body)
  `(with-open-stream (,stream (make-synonym-stream ,symbol))
     ,@body))

(defwith cl:make-two-way-stream ((stream) (input-stream output-stream) &body body)
  `(with-open-stream (,stream (make-two-way-stream ,input-stream ,output-stream))
     ,@body))

(defwith cl:open ((stream) (filespec &rest options) &body body)
  `(with-open-file (,stream ,filespec ,@options)
     ,@body))

