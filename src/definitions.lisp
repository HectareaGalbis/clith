
(in-package #:clith)


(defwith cl:make-broadcast-stream ((stream) (&rest streams) body)
  `(with-open-stream (,stream (make-broadcast-stream ,@streams))
     ,@body))

(defwith cl:make-concatenated-stream ((stream) (&rest input-streams) body)
  `(with-open-stream (,stream (make-concatenated-stream ,@input-streams))
     ,@body))

(defwith cl:make-echo-stream ((stream) (input-stream output-stream) body)
  `(with-open-stream (,stream (make-concatenated-stream ,input-stream ,output-stream))
     ,@body))

(defwith cl:make-string-input-stream ((stream) (string &optional start end) body)
  `(with-open-stream (,stream (make-string-input-stream ,string ,start ,end))
     ,@body))

(defwith cl:make-string-output-stream ((stream) (&key element-type) body)
  `(with-open-stream (,stream (make-string-output-stream :element-type ,element-type))
     ,@body))

(defwith cl:make-synonym-stream ((stream) (symbol) body)
  `(with-open-stream (,stream (make-synonym-stream ,symbol))
     ,@body))

(defwith cl:make-two-way-stream ((stream) (input-stream output-stream) body)
  `(with-open-stream (,stream (make-two-way-stream ,input-stream ,output-stream))
     ,@body))

(defwith cl:open ((stream) (filespec &rest options) body)
  `(with-open-file (,stream ,filespec ,@options)
     ,@body))

