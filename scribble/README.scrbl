
(in-package #:clith-docs)

@output-file["/README.md"]

@title[:toc nil]{Common Lisp wITH}

Welcome to Clith!

This library defines the macro @fref[clith:with]. It allows you to create some objects, bind them to some variables, evaluate some expressions using these variables, and lastly the objects are destroyed automatically.

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

The macro @fref[with] uses @code{WITH expansions} in a similarly way to @code{setf}. These expansions control how the macro  @fref[with] is expanded.

Every common lisp function that creates an object that should be closed at the end has a @code{WITH expansion}. For example, functions like @clref[open] or @clref[make-two-way-stream] have a @code{WITH expansion}. See all the function in the @tref[cl-symbols]{reference}.

@example{
(let (some-stream)

  (with ((the-stream (open "~/test.txt")))
    (setf some-stream the-stream)
    (format t "Stream opened? ~s~%" (open-stream-p some-stream)))

  (format t "Stream opened after? ~s" (open-stream-p some-stream)))
}

On the other hand, clith defines new symbols for each common lisp @code{with-} macro. Examples are @code{package-iterator} or @code{slots}. See all the macros in the @tref[cl-macros]{reference}.

@example{
(defstruct vec2
  x
  y)

(let ((start (make-vec2 :x 5 :y 10)))
  (with (((x y) (slots start)))
    (+ x y)))
}

The macro @fref[with] can accept optional arguments for each slot:

@example{
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (+ x1 y1 x2 y2)))
}

Here, the slot @code{x1} is receiving the argument @code{x}. Each @code{WITH expansion} is responsible to manage these arguments.

Lastly, we can check if a symbol denotes a @code{WITH expansion} using @fref[withp]:

@example{
(withp 'slots)
}

@subtitle{Defining a WITH expansion}

In order to extend the macro @fref[with] we need to define a @code{WITH expansion}. To do so, we use @fref[defwith].

Suppose we have @code{(MAKE-WINDOW TITLE)} and @code{(DESTROY-WINDOW WINDOW)}. We want to control the expansion of WITH in order to use both functions. Let's define the WITH expansion:

@example|{
(defwith make-window ((window) (title) &body body)
  "Makes a window that will be destroyed after the end of WITH."
  (let ((window-var (gensym)))
    `(let ((,window-var (make-window ,title)))
       (let ((,window ,window-var))
         (unwind-protect
             (progn ,@body)
           (destroy-window ,window-var))))))
}|

This is a common implementation of a 'with-' macro. Note that we specified @code{(window)} to specify that only one variable is wanted.

Now we can use our expansion in WITH:

@code-block{
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
}
   
After the evaluation of with's body, @code{my-window} will be destroyed by @code{destroy-window}.

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

The macro @fref[with] accepts declarations. These declarations are moved to the correct place at expansion time. For example, consider again the example with the points, but this time, we want to ignore two arguments:

@example{
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (declare (ignore y1 x2))
    (+ x1 y2)))
}

Let's see the expanded code:

@example{
(macroexpand-1 '(with ((((x1 x) (y1 y)) (slots start))
                       (((x2 x) (y2 y)) (slots end)))
                  (declare (ignore y1 x2))
                  (+ x1 y2)))
}

Observe that every declaration is in the right place. But how this work?

@fref[with] assumes that symbols to be bound will be in certain places. Each symbol in the declaration is searched over all the places that can contain a symbol to be bound. It is searched from bottom to top. When a symbol is found, a declaration of that symbol is created there.

If you want to know exactly where these places are, check out the syntax of the @fref[with] macro:

@code-block[:lang "text"]{
  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] form)
  vars             ::= var | (var-with-options*)
  var-with-options ::= var | (var var-option*)
  var-option       ::= form
}

@code{var} are those places where a declaration can be placed.
