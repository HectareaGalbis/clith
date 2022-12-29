
(in-package :clith)


(defmacro defwith-streams (&rest names)
  (let ((defwith-forms (mapcar (lambda (name)
				 `(defwith ,name #',name #'close))
			       names)))
    `(progn
       ,@defwith-forms)))

(defwith-streams open make-broadcast-stream make-concatenated-stream make-echo-stream make-string-input-stream
  make-string-output-stream make-synonym-stream make-two-way-stream)
