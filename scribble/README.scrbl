
(in-package #:clith-docs)

@output-file["/README.md"]

@title[:toc nil]{Common Lisp wITH}

Welcome to Clith!

@table-of-contents[]

@subtitle{Introduction}

This library defines the macro @fref[with]. This macro binds symbols to objects that can be finalized automatically in a personalized way.

@code-block[:lang "common-lisp"]{
  (with ((file (open "~/file.txt" :direction :output)))
    (print "Hello Clith!" file))
}

@fref[with] is powerful enough to support almost every regular @code{WITH-} macro:

@example|{
(defwith slots (vars (object) body)
  `(with-slots ,vars ,object
     ,@body))

(defstruct 3d-vector x y z)

(with ((p (make-3d-vector :x 1 :y 2 :z 3))
       ((z (up y) x) (slots p)))
  (+ x up z))
}|

It supports declarations:

@example{
(with (((x y z) (values 1 2 3))
       ((a b c) (values 'a 'b 'c)))
  (declare (ignore a y c))
  (values x b z))
}

And it detects macros and symbol-macros:

@example{
(symbol-macrolet ((my-file (open "~/file.txt")))
  (with ((f my-file))
    (read f)))
}

@subtitle{Installation}

@itemize[
@item{Manual:}
]
@code-block[:lang "sh"]{
cd ~/common-lisp
git clone https://github.com/Hectarea1996/clith.git
}
@itemize[
@item{Quicklisp:}
]
@code-block[:lang "common-lisp"]{
(ql:quickload "clith")
}

@subtitle{Reference}

@itemize[
        @item{@tref[reference]}
]

@subtitle{Getting started}

@fref[with] can be used as @clref[let] or @clref[multiple-value-bind]:

@example{
(with (x
       (y 3)
       ((q r) (floor 4 5)))
  (values x y q r))
}

The macro @fref[with] uses @code{WITH expansions} in a similar way to @code{setf}. These expansions control how the macro  @fref[with] is expanded.

@example{
(let (some-stream)

  (with ((the-stream (open "~/test.txt")))
    (setf some-stream the-stream)
    (format t "Stream opened? ~s~%" (open-stream-p some-stream)))

  (format t "Stream opened after? ~s" (open-stream-p some-stream)))
}

Every Common Lisp function that creates an object that should be closed/destroyed has a @code{WITH expansion} defined by @code{CLITH}. For example, functions like @clref[open] or @clref[make-two-way-stream] have a @code{WITH expansion}. See all the functions in the @tref[cl-symbols]{reference}.

Also, we can check if a symbol denotes a @code{WITH expansion} using @fref[withp]:

@example{
(withp 'open)
}

@subtitle{Defining a WITH expansion}

@subsubtitle{Simple example: MAKE-WINDOW}

In order to extend the macro @fref[with] we need to define a @code{WITH expansion}. To do so, we use @fref[defwith].

Suppose we have @code{(MAKE-WINDOW TITLE)} and @code{(DESTROY-WINDOW WINDOW)}. We want to control the expansion of WITH in order to use both functions. Let's define the WITH expansion:

@example|{
(defwith make-window ((window) (title) body)
  "Makes a window that will be destroyed after the end of WITH."
  (let ((window-var (gensym)))
    `(let ((,window-var (make-window ,title)))
       (unwind-protect
           (let ((,window ,window-var))
             ,@body)
         (destroy-window ,window-var)))))
}|

This is a common implementation of a @code{WITH-} macro. Note that we specified @code{(window)} to specify that only one variable is wanted.

Now we can use our expansion:

@code-block{
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
}
   
After the evaluation of the body, @code{my-window} will be destroyed by @code{destroy-window}.

@subsubtitle{No need to return a value: INIT-SUBSYSTEM}

There are @code{WITH-} macros that doesn't return anything. They just initialize something that should be finalized at the end. Imagine that we have the functions @code{INIT-SUBSYSTEM} and @code{FINALIZE-SUBSYSTEM}. Let's define a @code{WITH expansion} that calls to @code{FINALIZE-SUBSYSTEM}:

@code-block[:lang "common-lisp"]|{
(defwith init-subsystem (() () body) ; <- No variables to bind and no arguments.
  "Initialize the subsystem and finalize it at the end of WITH."
  `(progn
     (init-subsystem)
     (unwind-protect
         (progn ,@body)
       (finalize-subsystem)))) 
}|

Now we don't need to worry about finalizing the subsystem:

@code-block[:lang "common-lisp"]{
(with (((init-subsystem)))
  ...)
}

@subsubtitle{Extended syntax: GENSYMS}

Some @code{WITH-} macros like @clref[with-slots] allow to specify some options to variables. Let's try to make a @code{WITH} expansion that works like @code{alexandria:with-gensyms}. Each variable should optionally accept the prefix for the fresh generated symbol.

We want to achieve something like this:

@code-block[:lang "common-lisp"]{
(with ((sym1 (gensyms))                  ; <- Regular syntax
       ((sym2 (sym3 "FOO")) (gensyms)))  ; <- Extended syntax for SYM3
  ...)
}

In order to do this, we are using @clref[gensym]:

@example|{
(defwith gensyms (vars () body)
  (let* ((list-vars (mapcar #'alexandria:ensure-list vars))
         (sym-vars (mapcar #'car list-vars))
         (prefixes (mapcar #'cdr list-vars))
         (let-bindings (mapcar (lambda (sym-var prefix)
                                 `(,sym-var (gensym ,(if prefix (car prefix) (symbol-name sym-var)))))
                               sym-vars prefixes)))
    `(let ,let-bindings
       ,@body))) 
}|

Each element in @code{VARS} can be a symbol or a list. That's the reason we are using @code{alexandria:ensure-list}. @code{LIST-VARS} will contain lists where the first element is the symbol to bound and can have a second element, the prefix. We store then the symbols in @code{SYM-VARS} and the prefixes in @code{PREFIXES}. Note that if a prefix is not specified, then the corresponding element in @code{PREFIXES} will be @code{NIL}. If some @code{PREFIX} is @code{NIL}, we use the name of the respective @code{SYM-VAR}. Finally, we create the @code{LET-BINDING} and use it in the final form.

Let's try it out:

@example{
(with ((x (gensyms))
       ((y z) (gensyms))
       (((a "CUSTOM-A") (b "CUSTOM-B") c) (gensyms)))
  (values (list x y z a b c)))
}

@subtitle{Documentation}

The macro @fref[defwith] accepts a docstring that can be retrieved with the function @clref[documentation]. Check out again the definition of the expansion of @code{make-window} above. Note that we wrote a docstring.

@example{
(documentation 'make-window 'with)
}

We can also @code{setf} the docstring:

@example{
(setf (documentation 'make-window 'with) "Another docstring!")
(documentation 'make-window 'with)
}


@subtitle{Declarations}

The macro @fref[with] accepts declarations. These declarations are moved to the correct place at expansion time. For example, imagine we want to open two windows, but the variables can be ignored:

@code-block[:lang "common-lisp"]{
(with ((w1 (make-window "Window 1"))
       (w2 (make-window "Window 2")))
  (declare (ignorable w1 w2))
  (print "Hello world!"))
}

Let's see the expanded code:

@example{
(macroexpand-1 '(with ((w1 (make-window "Window 1"))
                       (w2 (make-window "Window 2")))
                  (declare (ignorable w1 w2))
                  (print "Hello world!")))
}

Observe that the declarations are in the right place. Every symbol that can be bound is a candidate for a declaration. If more that one candidate is found (same symbol appearing more than once) the last one is selected.
