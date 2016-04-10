(use document)

(document (define ...)
    :usage '("variable binding:"
             (define varname value)
             (define mutability varname value)

             "function definition:"
             (define (varname args ...) body ...)
             (define mutability (varname args ...) body ...))

    :description '("Binds a value to a name in the current environment."
                   ""
                   "If a binding with the same name already exists, the definition will be replaced"
                   "if it is mutable.")

    :parameters  '((varname    "The name of the variable to be bound")
                   (value      "The value to bind to `varname`")
                   (mutability "Specifies whether the binding is mutable."
                               ((:mut     "The binding is mutable")
                                (:immut   "The binding is immutable")
                                (:builtin "The binding is mutable, but displays a warning if changed.")))
                   (body       "In a function definition form, this specifies the content of the function")
                   )

    :returns     '(unspecified)
    :conformance '(rnrs)
    :see-also    '(set! let let*)
    :example     '("(define (square x)"
                   "   (* x x))"))

(document (set! varname value)
    :description '("Replaces an existing variable binding if one exists, or creates a new binding "
                   "in the current environment otherwise.")
    :parameters  '((varname "The name of the variable to be bound")
                   (value   "The value to bind to `varname`"))
    :returns     '(unspecified)
    :conformance '(rnrs)
    :see-also    '(let let* define))

(document (or expression ...)
    :description '("Short-circuiting boolean 'or' function")
    :returns     '("If any of the given expressions evaluate as true, #t is returned, otherwise #f")
    :conformance '(rnrs)
    :see-also    '(and not))

(document (and expression ...)
    :description '("Short-circuiting boolean 'and' function")
    :returns     '("If all of the given expressions evaluate as true, #t is returned, otherwise #f")
    :conformance '(rnrs)
    :see-also    '(or not))

(document (not expression)
    :description '("boolean 'not function")
    :returns     '("If expression is true, then #f, otherwise #t")
    :conformance '(rnrs)
    :see-also    '(and or))
