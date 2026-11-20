(in-package #:clith/test)

(defvar *tests* '())
(defvar *failures*)

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *tests* (append (remove ',name *tests*) (list ',name)))
     ',name))

(defun report-failure (control &rest arguments)
  (push (apply #'format nil control arguments) *failures*))

(defmacro is (expected form)
  `(let ((expected ,expected))
     (handler-case
         (let ((actual ,form))
           (unless (equal expected actual)
             (report-failure "~s~%    expected ~s~%    got      ~s" ',form expected actual)))
       (error (e)
         (report-failure "~s~%    signaled ~a" ',form e)))))

(defmacro signals (condition-type form)
  `(handler-case
       (progn ,form
              (report-failure "~s~%    did not signal ~s" ',form ',condition-type))
     (,condition-type () nil)
     (error (e)
       (report-failure "~s~%    signaled ~s instead of ~s" ',form (type-of e) ',condition-type))))

(defun run-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test *tests*)
      (let ((*failures* '()))
        (handler-case (funcall test)
          (error (e) (report-failure "unexpected error: ~a" e)))
        (cond
          ((null *failures*)
           (incf passed))
          (t
           (incf failed)
           (format t "~&FAIL ~s~%" test)
           (dolist (failure (reverse *failures*))
             (format t "  ~a~%" failure))))))
    (format t "~&~d passed, ~d failed~%" passed failed)
    (zerop failed)))
