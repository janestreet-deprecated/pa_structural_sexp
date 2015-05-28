This library defines a syntax extensions that simplifies building
s-expressions from ocaml values.

Basic use
=========

The building block of this preprocessor is the quotation:

    <:structural_sexp< expr >>

It evaluates to an s-expression. This is done by recursing down the
expression and converting all data constructors into s-expressions. If
an expression with a type annotation is found, then the type is used
to convert to s-expression. Expressions that are neither data
constructors nor annotated with a type will be rejected.

For instance:

    <:structural_sexp< { a = "hello" ; b = (Time.now () : Time.t) } >>

will be preprocessed into:

    List [List [Atom "a"; Atom "hello"];
          List [Atom "b"; <:sexp_of< Time.t >> (Time.now ())];
         ]

This does not require a record with fields a and b to exist (and if
one does exist, its sexp_of function will be ignored unless a type
annotation is added around the record).

Variant, polymorphic variants, tuples and lists are supported as
well.  Variants are analogous to records in that a type containing the
variant does not have to exist unless a type annotation is added to
the variant.

Derived quotations
==================

A couple of quotations are built on top of the previous one, for
convenience.

    <:structural_error< string-expr expr >>

will build a value of type `Error.t` rather than return an sexp. The
string expression is a message which will appear in the error.


    <:raise_structural_sexp< string-expr expr >>

is equivalent to `Error.raise <:structural_error< string-expr expr >>`.


    <:structural_or_error< string-expr expr >>

is equivalent to `Result.Error <:structural_error< string-expr expr >>`.

Recommended use for errors
==========================

This extension is primarily intended to build better errors, by making
building errors easier and more readable, particularly when using
records to add context:

    try Unix.rename tmpfile dst
    with exn ->
      <:raise_structural_sexp< "Error while renaming file"
                               { source = (tmpfile : string)
                               ; dest = (dst : string)
                               ; exn = (exn : exn)
                               } >>

