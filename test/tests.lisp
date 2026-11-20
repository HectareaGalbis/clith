(in-package #:clith/test)

(defmacro with-expansion-defined (definition &body body)
  `(progn
     (eval ',definition)
     ,@body))

(deftest defwith-without-whole
  (with-expansion-defined
      (defwith no-whole ((var) body a b)
        `(let ((,var (list ,a ,b)))
           ,@body))
    (is '(1 2) (eval '(with ((x (no-whole 1 2))) x)))))

(deftest defwith-whole-receives-user-form
  (with-expansion-defined
      (defwith whole-form (&whole w (var) body &rest args)
        (declare (ignore args))
        `(let ((,var ',w))
           ,@body))
    (is '(whole-form 1 2) (eval '(with ((x (whole-form 1 2))) x)))
    (is '(whole-form) (eval '(with ((x (whole-form))) x)))))

(deftest defwith-whole-excludes-vars-and-body
  (with-expansion-defined
      (defwith whole-only (&whole w vars body)
        `(list ',w ',vars ',body))
    (is '((whole-only) (x) ((declare (ignorable x)) :body))
        (eval '(with ((x (whole-only))) (declare (ignorable x)) :body)))))

(deftest defwith-whole-and-environment
  (with-expansion-defined
      (defwith whole-env (&whole w &environment e (var) body a)
        `(let ((,var (list ',w ,(and e t) ,a)))
           ,@body))
    (is '((whole-env 5) t 5) (eval '(with ((x (whole-env 5))) x)))))

(deftest defwith-whole-with-dotted-lambda-list
  (with-expansion-defined
      (defwith whole-dotted (&whole w (var) body . r)
        `(let ((,var (list ',w ',r)))
           ,@body))
    (is '((whole-dotted :a :b) (:a :b))
        (eval '(with ((x (whole-dotted :a :b))) x)))))

(deftest defwith-whole-with-aux
  (with-expansion-defined
      (defwith whole-aux (&whole w (var) body a &aux (n (length w)))
        `(let ((,var (list ',w ,n ,a)))
           ,@body))
    (is '((whole-aux 7) 2 7) (eval '(with ((x (whole-aux 7))) x)))))

(deftest defwith-whole-declarations
  (with-expansion-defined
      (defwith whole-ignored (&whole w (var) body)
        (declare (ignore w))
        `(let ((,var :ignored))
           ,@body))
    (is :ignored (eval '(with ((x (whole-ignored))) x)))))

(deftest defwith-whole-through-macro
  (with-expansion-defined
      (defwith whole-macro (&whole w (var) body &rest args)
        (declare (ignore args))
        `(let ((,var ',w))
           ,@body))
    (eval '(defmacro whole-macro-alias (&rest args)
            `(whole-macro :alias ,@args)))
    (is '(whole-macro :alias 1)
        (eval '(with ((x (whole-macro-alias 1))) x)))))

(deftest defwith-whole-with-docstring
  (with-expansion-defined
      (defwith whole-documented (&whole w (var) body)
        "Documented expansion."
        `(let ((,var ',w))
           ,@body))
    (is "Documented expansion." (documentation 'whole-documented 'with))
    (is '(whole-documented) (eval '(with ((x (whole-documented))) x)))))

(deftest defwith-defines-expansion
  (with-expansion-defined
      (defwith defined-expansion ((var) body)
        `(let ((,var nil))
           ,@body))
    (is t (withp 'defined-expansion))))
