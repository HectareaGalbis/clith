
(in-package #:clith-docs)

@output-file["/README.md"]

@title[:toc nil]{Common Lisp wITH}

Welcome to Clith!

This library defines the macro @fref[clith:with]. It allows you to create some objects, bind them to some variables, evaluate some expressions using these variables. Once the process is complete, the objects are automatically destroyed.

@table-of-contents[]

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

The macro @fref[with] uses @code{WITH expansions} in a similar way to @code{setf}. These expansions control how the macro  @fref[with] is expanded.

@example{
(let (some-stream)

  (with ((the-stream (open "~/test.txt")))
    (setf some-stream the-stream)
    (format t "Stream opened? ~s~%" (open-stream-p some-stream)))

  (format t "Stream opened after? ~s" (open-stream-p some-stream)))
}

Every Common Lisp function that creates an object that should be closed/destroyed at the end has a @code{WITH expansion} defined by @code{CLITH}. For example, functions like @clref[open] or @clref[make-two-way-stream] have a @code{WITH expansion}. See all the functions in the @tref[cl-symbols]{reference}.

Also, we can check if a symbol denotes a @code{WITH expansion} using @fref[withp]:

@example{
(withp 'open)
}

@subtitle{Defining a WITH expansion}

In order to extend the macro @fref[with] we need to define a @code{WITH expansion}. To do so, we use @fref[defwith].

Suppose we have @code{(MAKE-WINDOW TITLE)} and @code{(DESTROY-WINDOW WINDOW)}. We want to control the expansion of WITH in order to use both functions. Let's define the WITH expansion:

@example|{
(defwith make-window ((window) (title) body)
  "Makes a window that will be destroyed after the end of WITH."
  (let ((window-var (gensym)))
    `(let ((,window-var (make-window ,title)))
       (let ((,window ,window-var))
         (unwind-protect
             (progn ,@body)
           (destroy-window ,window-var))))))
}|

This is a common implementation of a 'with-' macro. Note that we specified @code{(window)} to specify that only one variable is wanted.

Now we can use our expansion:

@code-block{
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
}
   
After the evaluation of the body, @code{my-window} will be destroyed by @code{destroy-window}.

@subtitle{Expansion's documentation}

The macro @fref[defwith] accepts a docstring that can be retrieved with the function @code{documentation}. Check out again the definition of the expansion of @code{make-window} above. Note that we wrote a docstring.

@example{
(documentation 'make-window 'with)
}

We can also @code{setf} the docstring:

@example{
(setf (documentation 'make-window 'with) "Another docstring!")
(documentation 'make-window 'with)
}


@subtitle{Declarations}

The macro @fref[with] accepts declarations. These declarations are moved to the correct place at expansion time. For example, imagine we want to open two windows, but only one variable will be used. The other one must be ignored:

@code-block[:lang "common-lisp"]{
(with ((w1 (make-window "Window 1"))
       (w2 (make-window "Window 2")))
  (declare (ignore w1))
  (print "Hello world!")
)
}

Let's see the expanded code:

@example{
(macroexpand-1 '(with ((w1 (make-window "Window 1"))
                       (w2 (make-window "Window 2")))
                  (declare (ignore w1))
                  (print "Hello world!")))
}

Observe that the declaration is in the right place. Every symbol that can be bound is a candidate for a declaration. If more that one candidate is found (same symbol appearing more than once) the last one is selected.
