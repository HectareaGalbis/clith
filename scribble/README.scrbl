
(in-package #:clith-docs)

@select-output-file["/README.md"]

@header{Common Lisp wITH}

Welcome to Clith!

This library defines the macro @fref[clith:with]. It allows you to create some objects, bind them to some variables, evaluate some expressions using these variables, and lastly the objects are destroyed automatically.

@mini-table-of-contents[]

@subheader{Installation}

@itemize[
@item{Manual:}
]
@code-block[:lang "sh"]{
cd ~/common-lisp
git clone https://github.com/Hectarea1996/clith.git
}
@itemize[
@item{Quicklisp (Ultralisp):}
]
@code-block[:lang "common-lisp"]{
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
(ql:quickload "clith")
}

@subheader{Reference}

@itemize[
        @item{@href[:tag reference]}
]

@subheader{Basic usage}

The macro @fref[with] uses @code{WITH expanders} simarly @code{setf} uses @code{setf expanders}. These expanders controls how the macro  @fref[with] is expanded.

Let's take a look at the built-in @code{slots} expander. As the name suggest, it will expand into a @code{with-slots} expression:

@example{
(defstruct vec2
  x
  y)

(let ((start (make-vec2 :x 5 :y 10)))
  (with (((x y) (slots start)))
    (+ x y)))
}

The macro @fref[with] also accepts options for each variable we want to bind. In the above example, what happens if we have two points?

@code-block{
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with (((x y) (slots origin))
         ((x y) (slots end)))   ;; <-- Name collision!!
    (+ x y x y)))
}

We should specify, as if using @code{with-slots}, that we want to reference the slot @code{x} or @code{y} using another symbol.

@example{
(let ((start (make-vec2 :x 5 :y 10))
      (end   (make-vec2 :x -3 :y -4)))
  (with ((((x1 x) (y1 y)) (slots start))
         (((x2 x) (y2 y)) (slots end)))
    (+ x1 y1 x2 y2)))
}

@subheader{Defining a WITH expander}

In order to extend the macro @fref[with] we need to define a @code{WITH expander}. To do so, we use @fref[defwith].

Suppose we have @code{(MAKE-WINDOW TITLE)} and @code{(DESTROY-WINDOW WINDOW)}. We want to control the expansion of WITH in order to use both functions. Let's define the WITH expander:

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

This is a common implementation of a 'with-' macro. Note that we specified @code{(window)} to specify that only
one variable is wanted.

Now we can use our expander in WITH:

@code-block{
(with ((my-window (make-window "My window")))
  ;; Doing things with the window
  )
}
   
After the body of @fref[with] is evaluated, @code{my-window} will be destroyed by @code{destroy-window}.

@subheader{Expander's documentation}

The macro @fref[defwith] accepts a docstring that can be retrieved with the function @code{documentation}. Check out again the definition of the expansion of @code{make-window} above. Note that we wrote a docstring.

@example{
(documentation 'make-window 'with)
}

We can also @code{setf} the docstring:

@example{
(setf (documentation 'make-window 'with) "Another docstring!")
(documentation 'make-window 'with)
}


@subheader{Declarations}

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

@fref[with] assumes that variables to be bound will be in certain places. Each variable in the declaration is searched over all the places that can contain a variable to be bound. It is searched from bottom to top. When a variable is found, a declaration of that variable is created there.

If you want to know exactly where these places are, check out the syntax of the @fref[with] macro:

@code-block[:lang "text"]{
  (WITH (binding*) declaration* form*)

  binding          ::= ([vars] form)
  vars             ::= var | (var-with-options*)
  var-with-options ::= var | (var var-option*)
  var-option       ::= form
}

@code{var} are those places where a declaration can refer to.

@subheader{Built-in WITH expanders}

The next symbols from the package @code{CL} has a built-in expander:

@itemize[

@item{@code{make-broadcast-stream}}

@item{@code{make-concatenated-stream}}

@item{@code{make-echo-stream}}

@item{@code{make-string-input-stream}}

@item{@code{make-string-output-stream}}

@item{@code{make-two-way-stream}}

@item{@code{open}}

]

Additionally, every macro from the package @code{CL} whose name starts with @code{with-} has its own expander. We've already seen an example using the expander @code{slots}.

Since we cannot define new symbols in the package @code{CL}, these expanders are defined in a special way. @fref[with] will recognize all the symbols (for any package) whose name is equal to the name of the expander.

The complete list is:

@table[
@row[
@cell{CL Standard macro} @cell{WITH expander}
]
@row[
@cell{with-accesors} @cell{accesors}
]
@row[
@cell{with-compilation-unit} @cell{compilation-unit}
]
@row[
@cell{with-condition-restarts} @cell{condition-restarts}
]
@row[
@cell{with-hash-table-iterator} @cell{hash-table-iterator}
]
@row[
@cell{with-input-from-string} @cell{input-from-string}
]
@row[
@cell{with-open-file} @cell{open-file}
]
@row[
@cell{with-open-stream} @cell{open-stream}
]
@row[
@cell{with-output-to-string} @cell{output-to-string}
]
@row[
@cell{with-package-iterator} @cell{package-iterator}
]
@row[
@cell{with-simple-restart} @cell{simple-restart}
]
@row[
@cell{with-slots} @cell{slots}
]
@row[
@cell{with-standard-io-syntax} @cell{standard-io-syntax}
]
]
