
(in-package #:clith-docs)

@title[:tag reference :toc nil]{Reference}

@table-of-contents[]


@subtitle{API reference}

@table-of-functions[]

@function-glossary[#:clith]


@subtitle{WITH expansions}

@subsubtitle[:tag cl-symbols]{Common Lisp symbols}

The following Common Lisp functions have a @code{WITH expansion}:

@itemize[
        @item{@code{make-broadcast-stream}}
        @item{@code{make-concatenated-stream}}
        @item{@code{make-echo-stream}}
        @item{@code{make-string-input-stream}}
        @item{@code{make-string-output-stream}}
        @item{@code{make-synonym-stream}}
        @item{@code{make-two-way-stream}}
        @item{@code{open}}
]

@subsubtitle[:tag cl-macros]{Common Lisp WITH- macros}

The following symbols have a @code{WITH expansion}. They are listed with their respective @code{WITH-} macro:

@table[
        @row[@cell{accessors} @cell{with-accessors}]
        @row[@cell{compilation-unit} @cell{with-compilation-unit}]
        @row[@cell{condition-restarts} @cell{with-condition-restarts}]
        @row[@cell{hash-table-iterator} @cell{with-hash-table-iterator}]
        @row[@cell{input-from-string} @cell{with-input-from-string}]
        @row[@cell{output-to-string} @cell{with-output-to-string}]
        @row[@cell{package-iterator} @cell{with-package-iterator}]
        @row[@cell{simple-restart} @cell{with-simple-restart}]
        @row[@cell{slots} @cell{with-slots}]
        @row[@cell{standard-io-syntax} @cell{with-standard-io-syntax}]
]
