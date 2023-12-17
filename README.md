<a id="header-adp-github-headertag614"></a>
# Common Lisp wITH

Welcome to Clith\!

This library defines the macro [clith\:with](/docs/reference.md#function-clith-with)\. It is like the \'with expression\' in Python but better\. It allows you to create some objects\, bind them to some variables\, evaluate some expressions using these variables\, and lastly the objects are destroyed automatically\. Even more\, you can bind functions like LABELS does and nest expressions like UIOP\:NEST\.

* [Common Lisp wITH](/README.md#header-adp-github-headertag614)
  * [Documentation](/README.md#header-adp-github-headertag615)
  * [Basic usage](/README.md#header-adp-github-headertag616)
  * [Customizing expansion](/README.md#header-adp-github-headertag629)
    * [Nest expanders](/README.md#header-adp-github-headertag630)
    * [Let expanders](/README.md#header-adp-github-headertag631)
  * [Declarations](/README.md#header-adp-github-headertag632)
  * [Emacs \+ Slime indentation support](/README.md#header-adp-github-headertag633)


<a id="header-adp-github-headertag615"></a>
## Documentation

* [Reference](/docs/reference.md#header-adp-github-reference)


<a id="header-adp-github-headertag616"></a>
## Basic usage

The simplest way to use [clith\:with](/docs/reference.md#function-clith-with) is like using LET or MULTIPLE\-VALUE\-BIND\:

`````common-lisp
(clith:with ((x 5)
             ((q r) (floor 45 32)))
  (+ x q r))
`````
`````common-lisp
19
`````

But you can also bind functions\:

`````common-lisp
(clith:with ((hello (name)
               (format t "~%Hello ~a!" name)))
  (hello "there"))
`````
`````text

Hello there!
`````
`````common-lisp
nil
`````

Or nest other expressions\. This can be useful when using other with\- macros\:

`````common-lisp
(clith:with ((with-open-file (file "~/my-file.txt")))
  (print (read file)))
`````

And\, of course\, you can mix it all up\.

`````common-lisp
(clith:with ((n 10)
             (add2 (a b)
               (+ a b))
             ((with-output-to-string (str))))
  (format str "The result is ~a" (add2 10 n)))
`````
`````common-lisp
"The result is 20"
`````

The bound variables are destroyed automatically at the end of [clith\:with](/docs/reference.md#function-clith-with)\. More precisely\, the generic function [clith\:destroyer](/docs/reference.md#function-clith-destroyer) is called for almost all bound variables\. Variables bound by a LET\-like binding clause will be destroyed always\. However\, only the first bound variable within a MULTIPLE\-VALUE\-BIND\-like form will be destroyed\.

`````common-lisp
(clith:with ((n 10)
             ((a b) (values 1 2)))
  (print n))
`````

Expands to\:

`````common-lisp
(let* ((n 10))
  (unwind-protect
      (multiple-value-bind (a b) (values 1 2)
        (unwind-protect
            (progn (print n))
          (destroyer a)))
    (progn
      (destroyer n))))
`````

Observe that only ``` N ``` and ``` A ``` are destroyed\.

The function [clith\:destroyer](/docs/reference.md#function-clith-destroyer) is already defined for stream objects\. In fact\, this is the implementation of the method you can find in the source code\.

`````common-lisp
(defmethod destroyer ((obj stream))
  "Closes a stream."
  (close obj))
`````


<a id="header-adp-github-headertag629"></a>
## Customizing expansion

There are some cases that [clith\:with](/docs/reference.md#function-clith-with) cannot resolve on its own\. Expanders are designed with this in mind\. When using a let\-like\, multiple\-value\-bind\-like or nest\-like form\, we can control how [clith\:with](/docs/reference.md#function-clith-with) is expanded\. For the two first forms we have ``` let expanders ```\, and for the last one we have ``` nest expanders ```\.

<a id="header-adp-github-headertag630"></a>
### Nest expanders

Let\'s see what problems are trying to solve ``` nest expanders ```\. Many libraries have functions that initialize something that must be terminated later\, but they don\'t return any object\. They just change the context\. Consider the following two functions\:

`````common-lisp
(initialize-audio-subsystem channels) ; Receives one argument
(terminate-audio-subsystem)
`````

Both functions does not return anything\. The macro that comes to our heads immediatly is something like\:

`````common-lisp
(with-audio-subsystem channels &body body)

;; Possible lisp-implementation
(defmacro with-audio-subsystem (channels &body body)
  `(progn
     (initialize-audio-subsystem ,channels)
     ,@body
     (terminate-audio-subsystem)))
`````

However\, we want to use [clith\:with](/docs/reference.md#function-clith-with)\. We need then to define a ``` nest expander ```\. We can do that with [clith\:define\-nest\-expander](/docs/reference.md#function-clith-define-nest-expander)\:

`````common-lisp
(clith:define-nest-expander initialize-audio-subsystem body (channels)
  `(progn
     (initialize-audio-subsystem ,channels)
     ,@body
     (terminate-audio-subsystem)))
`````

Note that this implementation is almost the same as the macro ``` with-audio-subsystem ```\. The only difference is how the body argument is received\.

Now we can use the function ``` initialize-audio-subsystem ``` into a nest\-like form\.

`````common-lisp
(with (((initialize-audio-subsystem 5)))
  ;; Doing something with audio
  )
`````

Leaving from the ``` WITH ``` expression will cause the audio subsystem to terminate\.


<a id="header-adp-github-headertag631"></a>
### Let expanders

As you might expect\, this will be very similar to ``` nest expanders ``` but for functions returning something\. But we have already the generic function [clith\:destroyer](/docs/reference.md#function-clith-destroyer)\. So\, what is the point of ``` let expanders ```\?

The best example I found is in the CFFI library\. In order to allocate some memory you can use [cffi\:foreign\-alloc](https://cffi.common-lisp.dev/manual/cffi-manual.html#foreign_002dalloc) \(and [cffi\:foreign\-free](https://cffi.common-lisp.dev/manual/cffi-manual.html#foreign_002dfree)\) or [cffi\:with\-foreign\-object](https://cffi.common-lisp.dev/manual/cffi-manual.html#with_002dforeign_002dobject)\. The main difference is that the former allocates memory at the heap\, while the latter does it at the stack\. And that is a big difference\. It is always preferable to use the stack\, and therefor\, the ``` cffi:with-foreign-object ``` macro\.

We are going to see that [clith\:destroyer](/docs/reference.md#function-clith-destroyer) gives the user flexibility\. They can define a function that returns an object and let [clith\:with](/docs/reference.md#function-clith-with) destroy it automatically\.

On the other hand\, ``` let expanders ``` will give the user the option of power\. We can expand a let\-like form to an expression that uses ``` cffi:with-foreign-object ```\.

Suppose we the following C struct\:

`````common-lisp
(cffi:defcstruct point
  (x :int)
  (y :int))
`````

And let\'s suppose that the way we are making the bindings is via wrapping pointers\. So we could define something like this\:

`````common-lisp
(defclass point ()
  ((point-ptr :initarg :point-ptr
              :accessor point-ptr)))

(defun make-point (x-pos y-pos)
  (let* ((ptr (cffi:foreign-alloc '(:struct point))))
    (cffi:with-foreign-slots ((x y) ptr (:struct point))
      (setf x x-pos)
      (setf y y-pos))
    (make-instance 'point :point-ptr ptr)))

(defun destroy-point (obj)
  (cffi:foreign-free (point-ptr obj)))
`````

We implement the method [clith\:destroyer](/docs/reference.md#function-clith-destroyer) as usual\:

`````common-lisp
(defmethod clith:destroyer ((obj point))
  (destroy-point obj))
`````

The user has now the flexibility of using [clith\:with](/docs/reference.md#function-clith-with) even defining their own functions\. Now is time to improve this with some power\. The efficient ``` cffi:with-foreign-object ``` should be used if ``` make-point ``` is called directly within the [clith\:with](/docs/reference.md#function-clith-with) macro\. Let\'s define a ``` let expander ``` with [clith\:define\-let\-expander](/docs/reference.md#function-clith-define-let-expander)\:

`````common-lisp
(clith:define-let-expander make-point bind-vars body (x-pos y-pos)
  (let ((ptr     (gensym)))
    `(cffi:with-foreign-object (,ptr '(:struct point))
       (cffi:with-foreign-slots ((x y) ,ptr (:struct point))
         (setf x ,x-pos)
         (setf y ,y-pos))
       (multiple-value-bind ,bind-vars (make-instance 'point :point-ptr ,ptr)
         ,@body))))
`````

After defining an uninterned symbol \(``` ptr ```\) we use ``` cffi:with-foreign-object ``` that creates the foreign object\, then we initialize it and wrap it within a lisp object\. Finally\, we bind the ``` bind-vars ``` provided by the user and evaluate the ``` body ```\. We use ``` multiple-value-bind ``` in order to maintain how [clith\:with](/docs/reference.md#function-clith-with) works with the multiple\-value\-bind\-like form\. In this case\, if more than one argument is provided\, all but the first will be bound to ``` NIL ```\.

And how we use it\? Well\, you know already how\. Just use [clith\:with](/docs/reference.md#function-clith-with) as always\!

`````common-lisp
(with ((heap-point (function-that-uses-make-point 1 2)) ; This will be destroyed by clith:destroyer
       (stack-point (make-point 3 5)))                  ; This will be expanded to cffi:with-foreign-object
  ;; Doing something with points
  )
`````

Observe what we\'ve got here\. Flexibility and power with the same interface\! The user only needs to know about the constructor ``` make-point ```\. No destructors and no other ``` with- ``` macros\.

I recommend to read the [clith\:with](/docs/reference.md#function-clith-with) reference\. There is an easier example of a ``` let exapander ``` that might be helpful to understand better how to create one\.


<a id="header-adp-github-headertag632"></a>
## Declarations

The macro [clith\:with](/docs/reference.md#function-clith-with) supports declarations\. Like declarations only work if used immediatly after a ``` let* ```\, ``` multiple-value-bind ``` or ``` labels ``` expression\, [clith\:with](/docs/reference.md#function-clith-with) must manage declarations specially\.

You don\'t need to think too much about this because [clith\:with](/docs/reference.md#function-clith-with) does everything on its own\, but some notes should be taken into account\:

Firstly\, suppose we have the following code\:

`````common-lisp
(with ((x 5)
       (increment-x ()
         (setf x (1+ x))))
  (declare (special x))
  #'increment-x)
`````

Someone may think that ``` x ``` is only special once the body start evaluating\. But the truth is that ``` x ``` is special right after its declaration\. Remember that declarations only work if they are placed right after the binding\. In this case\, this expression will expand first to a ``` let* ``` form\. I\.e\, ``` x ``` will be special when used inside the function ``` increment-x ```\. Then\, if we return that function bad things can happen because the special variable ``` x ``` could be unbound after exiting [clith\:with](/docs/reference.md#function-clith-with)\.

Another thing we should take into account is that declaration will affect only once to the lowest binding variable\. For example\:

`````
common-lisp(with ((x 5)
       (print-x () x)
       (x 10))
  (declare (special x))
  #'print-x)
`````

In this case\, the first ``` x ``` is not special\. The second one is\. So\, returning ``` #'print-x ``` is safe\.


<a id="header-adp-github-headertag633"></a>
## Emacs \+ Slime indentation support

If you try to indent the [clith\:with](/docs/reference.md#function-clith-with) macro using the label form\, you will see that something is not right\.

`````common-lisp
(with ((func (x y)
             (+ x y))))  ;; <-- Bad indentation
`````

We can make a small change to the slime\'s indentation system\. Just add the following code to your ``` init.el ``` file\:

`````emacs-lisp
;; ------ clith ------
(let ((name 'with)
      (indentation '((&whole 4 &rest (&whole 1 1 2 &body)) &body)))
  (put name 'common-lisp-indent-function indentation))
`````

Let\'s try to indent the same example\:

`````common-lisp
(with ((func (x y)
         (+ x y))))  ;; <-- Good indentation :D
`````