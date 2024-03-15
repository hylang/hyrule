(import
  hy.compiler [HyASTCompiler calling-module]
  hyrule.iterables [coll? distinct flatten rest]
  hyrule.collections [walk])


(defmacro defmacro-kwargs [name params #* body]

  #[=[Define a macro that can take keyword arguments. When the macro
  is called, :hy:func:`match-fn-params` is used to match the
  arguments against ``params``, and the parameters are assigned to
  local variables that can be used in the macro body. ::

    (defmacro-kwargs do10times [form [print-iteration 'False]]
      (setv i (hy.gensym))
      `(for [~i (range 10)]
        (when ~print-iteration
          (print "Now on iteration:" ~i))
        ~form))

    (setv x [])
    (do10times
      (.append x 1))
    ; Nothing is printed.
    (do10times
      :print-iteration (> (len x) 17)
      (.append x 1))
    ; Iterations 8 and 9 are printed.]=]

  (setv [ps p-rest p-kwargs] (parse-fn-params params))
  (setv docstring None)
  (when (and body (isinstance (get body 0) hy.models.String))
    (setv [docstring #* body] body))
  (setv g (hy.gensym))
  `(defmacro ~name [#* ~g]
    ~@(if docstring [docstring] [])
    (setv ~g (hy.I.hyrule.match-fn-params ~g '~params))
    ~@(gfor
      k [
        #* (.keys ps)
        #* (if p-rest [p-rest] [])
        #* (if p-kwargs [p-kwargs] [])]
      `(setv ~(hy.models.Symbol k) (get ~g ~k)))
    ~@body))

(defn match-fn-params [args params]
  #[[Match an iterable of arguments against a parameter list in the
  style of a :hy:func:`defn` lambda list. The parameter-list syntax
  here is somewhat restricted: annotations are forbiddden, ``/`` and
  ``*`` aren't recognized, and nothing is allowed after ``#* args``
  other than ``#** kwargs``. Return a dictionary of the parameters and
  their values. ::

    (match-fn-params
      [1 :foo "x"]
      '[a [b 2] [c 3] #* args #** kwargs])
    ; => {"a" 1  "b" 2  "c" 3  "args" #()  "kwargs" {"foo" "x"}}

  If a default argument is a :ref:`model <hy:models>`, it's evaluated.
  The evaluation occurs in a minimal environment, with no access to
  surrounding global or local Python-level objects or macros. If this
  is too restrictive, use ``None`` as the default value and compute the
  real default value in other code.

  This function exists mostly to implement :hy:macro:`defmacro-kwargs`.]]

  (setv [ps p-rest p-kwargs] (parse-fn-params params))

  ; Loop over `args`.
  (setv  args (list args)  collected-rest []  collected-kwargs {}  i-pos 0)
  (while args
    (setv x (.pop args 0))
    (cond

      (and
          (isinstance x hy.models.Expression)
          x
          (isinstance (get x 0) hy.models.Symbol)
          (in (hy.mangle (get x 0)) ["unpack_iterable" "unpack_mapping"]))
        ; Unpacking would require evaluating the elements of `args`, which we
        ; want to avoid.
        (raise (TypeError "unpacking is not allowed in `args`"))

      (isinstance x hy.models.Keyword) (do
        ; A keyword argument
        (setv x (hy.mangle x.name))
        (when (or
            (in x collected-kwargs)
            (and (in x ps) (is-not (get ps x "value") None)))
          (raise (TypeError (+ "keyword argument repeated: " x))))
        (setv v (.pop args 0))
        (cond
          (in x ps)
            (setv (get ps x "value") v)
          p-kwargs
            (setv (get collected-kwargs x) v)
          True
            (raise (TypeError f"unexpected keyword argument '{x}'"))))

      True (do
        ; A positional argument
        (cond
          (< i-pos (len ps)) (do
            (setv [k d] (get (list (.items ps)) i-pos))
            (if (is (get d "value") None)
              (setv (get d "value") x)
              (raise (TypeError f"got multiple values for argument '{k}'"))))
          p-rest
            (.append collected-rest x)
          True
            (raise (TypeError f"takes {(len ps)} positional arguments but more were given")))
        (+= i-pos 1))))

  ; Return the result.
  (dict
    #** (dfor
      [p d] (.items ps)
      p (cond
        (is-not (get d "value") None)
          (get d "value")
        (is-not (get d "default") None)
          (get d "default")
        True
          (raise (TypeError f"missing a required positional argument: '{p}'"))))
    #** (if p-rest {p-rest (tuple collected-rest)} {})
    #** (if p-kwargs {p-kwargs collected-kwargs} {})))

(defn parse-fn-params [params]
  "A subroutine for `defmacro-kwargs` and `match-params`."
  (import
    funcparserlib.parser [maybe many]
    hy.model-patterns [SYM FORM sym brackets pexpr])

  (setv msym (>> SYM hy.mangle))
  (defn pvalue [root wanted]
    (>> (pexpr (+ (sym root) wanted)) (fn [x] (get x 0))))
  (setv [ps p-rest p-kwargs] (.parse
    (+
      (many (| msym (brackets msym FORM)))
      (maybe (pvalue "unpack-iterable" msym))
      (maybe (pvalue "unpack-mapping" msym)))
    params))
  (setv ps (dfor
    p ps
    :setv [k dv] (if (isinstance p hy.models.List) p [p None])
    k (dict :value None :default (if (isinstance dv hy.models.Object)
     (hy.eval dv {} :macros {})
     dv))))
  [ps p-rest p-kwargs])


(defmacro defmacro! [name args #* body]
  "Like `defmacro`, but with automatic gensyms and once-only evaluation.

  ``defmacro!`` automatically generates :hy:func:`gensyms <hy.gensym>` for
  any symbol that starts with ``g!``.
  For example, ``g!a`` would become ``(hy.gensym \"a\")``.

  Additionally, any params prefixed with ``o!`` are evaluated just once;
  they are then available within `body` with a ``g!`` prefix.

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

       => (defmacro! triple-2 [n] `(do (setv ~g!n ~n) (+ ~g!n ~g!n ~g!n)))
       => (triple-2 (expensive-get-number))  ; avoid repeats with a gensym
       spam
       42

    ::

       => (defmacro! triple-3 [o!n] `(+ ~g!n ~g!n ~g!n))
       => (triple-3 (expensive-get-number))  ; easier with `o!` prefix
       spam
       42
  "
  (defn extract-o!-sym [arg]
    (cond (and (isinstance arg hy.models.Symbol) (.startswith arg "o!"))
            arg
          (and (isinstance args hy.models.List) (.startswith (get arg 0) "o!"))
            (get arg 0)))
  (setv os (lfor  x (map extract-o!-sym args)  :if x  x)
        gs (lfor s os (hy.models.Symbol (+ "g!" (cut s 2 None))))
        syms (list
               (distinct
                 (filter (fn [x]
                           (and (hasattr x "startswith")
                                (.startswith x "g!")))
                         (flatten [gs body]))))
        gensyms []
        g!res (hy.gensym "res"))
  (for [sym syms]
      (.extend gensyms [sym `(hy.gensym ~(cut sym 2 None))]))

  (setv [docstring body] (if (and body
                                  (isinstance (get body 0) str)
                                  (> (len body) 1))
                             #((get body 0) (tuple (rest body)))
                             #(None body)))

  `(defmacro ~name ~args
     ~docstring
     (setv ~@gensyms
           ~g!res ((fn [] ~@body)))
     `(do
        (setv ~~gs ~~os)
        ~~g!res)))

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


(defn map-model [x f]
  #[[Recursively apply a callback to some code. The unary function ``f`` is called on the object ``x``, converting it to a :ref:`model <hy:models>` first if it isn't one already. If the return value isn't ``None``, it's converted to a model and used as the result. But if the return value is ``None``, and ``x`` isn't a :ref:`sequential model <hy:hysequence>`, then ``x`` is used as the result instead. ::

     (defn f [x]
       (when (= x 'b)
         'B))
     (map-model 'a f)  ; => 'a
     (map-model 'b f)  ; => 'B

  Recursive descent occurs when ``f`` returns ``None`` and ``x`` is sequential. Then ``map-model`` is called on all the elements of ``x`` and the results are bound up in the same model type as ``x``. ::

    (map-model '[a [b c] d] f)  ; => '[a [B c] d]

  The typical use of ``map-model`` is to write a macro that replaces models of a selected kind, however deeply they're nested in a tree of models. ::

    (defmacro lowercase-syms [#* body]
      "Evaluate `body` with all symbols downcased."
      (hy.I.hyrule.map-model `(do ~@body) (fn [x]
        (when (isinstance x hy.models.Symbol)
          (hy.models.Symbol (.lower (str x)))))))
    (lowercase-syms
      (SETV FOO 15)
      (+= FOO (ABS -5)))
    (print foo)  ; => 20

  That's why the parameters of ``map-model`` are backwards compared to ``map``: in user code, ``x`` is typically a symbol or other simple form whereas ``f`` is a multi-line anonymous function.]]

  (when (not (isinstance x hy.models.Object))
    (setv x (hy.as-model x)))
  (cond
    (is-not (setx value (f x)) None)
      (hy.as-model value)
    (isinstance x hy.models.Sequence)
      ((type x)
        (gfor  elem x  (map-model elem f))
        #** (cond
          (isinstance x hy.models.FString)
            {"brackets" x.brackets}
          (isinstance x hy.models.FComponent)
            {"conversion" x.conversion}
          True
            {}))
    True
      x))


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
       ...   ...)"
  (setv syms [])
  (for [arg args]
    (.extend syms [arg `(hy.gensym '~arg)]))
  `(do
    (setv ~@syms)
    ~@body))


(defreader /
  #[[Sugar for :hy:class:`hy.I`, to access modules without needing to explicitly import them first.
  Unlike ``hy.I``, ``#/`` cannot be used if the module name is only known at runtime.

  Examples:

    Access modules and their elements directly by name:

    ::

      => (type #/ re)
      <class 'module'>
      => #/ os.curdir
      "."
      => (#/ re.search r"[a-z]+" "HAYneedleSTACK")
      <re.Match object; :span #(3 9) :match "needle">

    Like ``hy.I``, separate submodule names with ``/``:

    ::

      => (#/ os/path.basename "path/to/file")
      "file"]]
  (.slurp-space &reader)
  (setv [mod #* ident] (.split (.read-ident &reader) ".")
        imp `(hy.I ~(hy.mangle (.replace mod "/" "."))))
  (if ident
    `(. ~imp ~@(map hy.models.Symbol ident))
    imp))
