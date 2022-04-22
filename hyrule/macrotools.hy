(import
  hy.compiler [HyASTCompiler calling-module]
  hyrule.iterables [coll? distinct flatten rest]
  hyrule.collections [walk])


(defmacro defmacro/g! [name args #* body]
  "Like `defmacro`, but symbols prefixed with 'g!' are gensymed.

  ``defmacro/g!`` is a special version of ``defmacro`` that is used to
  automatically generate :hy:func:`gensyms <hy.gensym>` for
  any symbol that starts with
  ``g!``.

  For example, ``g!a`` would become ``(hy.gensym \"a\")``.

  .. seealso::

    Section :ref:`using-gensym`
  "
  (setv syms (list
              (distinct
               (filter (fn [x]
                         (and (hasattr x "startswith")
                              (.startswith x "g!")))
                       (flatten body))))
        gensyms [])
  (for [sym syms]
    (.extend gensyms [sym `(hy.gensym ~(cut sym 2 None))]))

  (setv [docstring body] (if (and (isinstance (get body 0) str)
                                  (> (len body) 1))
                             #((get body 0) (tuple (rest body)))
                             #(None body)))

  `(defmacro ~name [~@args]
     ~docstring
     (setv ~@gensyms)
     ~@body))


(defmacro defmacro! [name args #* body]
  "Like `defmacro/g!`, with automatic once-only evaluation for 'o!' params.

  Such 'o!' params are available within `body` as the equivalent 'g!' symbol.

  Examples:
    ::

       => (defn expensive-get-number [] (print \"spam\") 14)
       => (defmacro triple-1 [n] `(+ ~n ~n ~n))
       => (triple-1 (expensive-get-number))  ; evals n three times
       spam
       spam
       spam
       42

    ::

       => (defmacro/g! triple-2 [n] `(do (setv ~g!n ~n) (+ ~g!n ~g!n ~g!n)))
       => (triple-2 (expensive-get-number))  ; avoid repeats with a gensym
       spam
       42

    ::

       => (defmacro! triple-3 [o!n] `(+ ~g!n ~g!n ~g!n))
       => (triple-3 (expensive-get-number))  ; easier with defmacro!
       spam
       42
  "
  (defn extract-o!-sym [arg]
    (cond (and (isinstance arg hy.models.Symbol) (.startswith arg "o!"))
            arg
          (and (isinstance args hy.models.List) (.startswith (get arg 0) "o!"))
            (get arg 0)))
  (setv os (lfor  x (map extract-o!-sym args)  :if x  x)
        gs (lfor s os (hy.models.Symbol (+ "g!" (cut s 2 None)))))

  (setv [docstring body] (if (and (isinstance (get body 0) str)
                                  (> (len body) 1))
                             #((get body 0) (tuple (rest body)))
                             #(None body)))

  `(defmacro/g! ~name ~args
     ~docstring
     `(do (setv ~@(sum (zip ~gs ~os) #()))
          ~@~body)))


(defn macroexpand-all [form [ast-compiler None]]
  "Recursively performs all possible macroexpansions in form, using the ``require`` context of ``module-name``.
  `macroexpand-all` assumes the calling module's context if unspecified.
  "
  (setv quote-level 0
        ast-compiler (or ast-compiler (HyASTCompiler (calling-module))))
  (defn traverse [form]
    (walk expand (fn [x] x) form))
  (defn expand [form]
    (nonlocal quote-level)
    ;; manages quote levels
    (defn +quote [[x 1]]
      (nonlocal quote-level)
      (setv head (get form 0))
      (+= quote-level x)
      (when (< quote-level 0)
        (raise (TypeError "unquote outside of quasiquote")))
      (setv res (traverse (cut form 1 None)))
      (-= quote-level x)
      `(~head ~@res))
    (if (and (isinstance form hy.models.Expression) form)
        (cond quote-level
               (cond (in (get form 0) '[unquote unquote-splice])
                       (+quote -1)
                     (= (get form 0) 'quasiquote) (+quote)
                     True (traverse form))
              (= (get form 0) 'quote) form
              (= (get form 0) 'quasiquote) (+quote)
              (= (get form 0) (hy.models.Symbol "require")) (do
               (ast-compiler.compile form)
               (return))
              (in (get form 0) '[except unpack-mapping])
               (hy.models.Expression [(get form 0) #* (traverse (cut form 1 None))])
              True (traverse (hy.macros.macroexpand form ast-compiler.module ast-compiler :result-ok False)))
        (if (coll? form)
            (traverse form)
            form)))
  (expand form))


(defmacro with-gensyms [args #* body]
  "Execute `body` with `args` as bracket of names to gensym for use in macros.

  ``with-gensym`` is used to generate a set of :hy:func:`gensyms <hy.gensym>`
  for use in a macro. The following code:

  Examples:
    ::

       => (with-gensyms [a b c]
       ...   ...)

    expands to::

       => (do
       ...   (setv a (hy.gensym)
       ...         b (hy.gensym)
       ...         c (hy.gensym))
       ...   ...)

  .. seealso::

     Section :ref:`using-gensym`
  "
  (setv syms [])
  (for [arg args]
    (.extend syms [arg `(hy.gensym '~arg)]))
  `(do
    (setv ~@syms)
    ~@body))
