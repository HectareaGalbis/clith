
(in-package #:adpgh)

@header{Common Lisp wITH}

Welcome to Clith!

This library defines the macro @fref[clith:with]. It is like the 'with expression' in Python but better. It allows you to create some objects, bind them to some variables, evaluate some expressions using these variables, and lastly the objects are destroyed automatically. Even more, you can bind functions like LABELS does and nest expressions like UIOP:NEST.

@mini-table-of-contents[]

@subheader{Installation}

This library is available at Ultralisp. If you don't have it already, you can add it evaluating this:

@code-block[:lang "common-lisp"]{
(ql-dist:install-dist "http://dist.ultralisp.org/"
	              :prompt nil)
}

After this you can install Clith using Quicklisp:

@code-block[:lang "common-lisp"]{
(ql:quickload :clith)
}

@subheader{Documentation}

@itemize[
        @item{@href[:tag reference]}
]

@subheader{A brief guide}

The simplest way to use @fref[clith:with] is like using LET or MULTIPLE-VALUE-BIND:

@example{
(clith:with ((x 5)
             ((q r) (floor 45 32)))
  (+ x q r))
}

But you can also bind functions:

@example{
(clith:with ((hello (name)
               (format t "~%Hello ~a!" name)))
  (hello "there"))
}

Or nest other expressions. This can be useful when using other with- macros:

@code-block[:lang "common-lisp"]{
(clith:with ((with-open-file (file "~/my-file.txt")))
  (print (read file)))
}

And, of course, you can mix it all up.

@example{
(clith:with ((n 10)
             (add2 (a b)
               (+ a b))
             ((with-output-to-string (str))))
  (format str "The result is ~a" (add2 10 n)))
}

The bound variables are destroyed automatically at the end of @fref[clith:with]. More precisely, the generic function @fref[clith:destroyer] is called for almost all bound variables. Variables bound by a LET-like binding clause will be destroyed always. However, only the first bound variable within a MULTIPLE-VALUE-BIND-like form will be destroyed.

@code-block[:lang "common-lisp"]{
(clith:with ((n 10)
             ((a b) (values 1 2)))
  (print n))
}

Expands to:

@code-block[:lang "common-lisp"]{
(let* ((n 10))
  (unwind-protect
      (multiple-value-bind (a b) (values 1 2)
        (unwind-protect
            (progn (print n))
          (destroyer a)))
    (progn
      (destroyer n))))
}

Observe that only @code{N} and @code{A} are destroyed.

The function @fref[clith:destroyer] is already defined for stream objects. In fact, this is the implementation of the method you can find in the source code.

@code-block[:lang "common-lisp"]{
(defmethod destroyer ((obj stream))
  "Closes a stream."
  (close obj))
}
