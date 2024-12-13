(import
  hy.compiler [HyASTCompiler calling-module]
  hyrule.iterables [coll? flatten])


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
    hy.model-patterns [SYM FORM sym brackets pexpr whole])

  (setv msym (>> SYM hy.mangle))
  (defn pvalue [root wanted]
    (>> (pexpr (+ (sym root) wanted)) (fn [x] (get x 0))))
  (setv [ps p-rest p-kwargs] (.parse
    (whole [
      (many (| msym (brackets msym FORM)))
      (maybe (pvalue "unpack-iterable" msym))
      (maybe (pvalue "unpack-mapping" msym))])
    params))
  (setv ps (dfor
    p ps
    :setv [k dv] (if (isinstance p hy.models.List) p [p None])
    k (dict :value None :default (if (isinstance dv hy.models.Object)
     (hy.eval dv {} :macros {})
     dv))))
  [ps p-rest p-kwargs])


(defmacro defmacro! [name args #* body]

  #[[As :hy:func:`defmacro`, but with automatic gensyms. For each symbol in
  the body beginning with "``g!``", a gensym is implicitly created and assigned
  to that symbol. ::

    (defmacro! m []
      `(do
         (setv ~g!accum [])
         (for [x (range 3)]
           (.append ~g!accum x))
         ~g!accum))
    (m)  ; => [0 1 2]

  Furthermore, for each parameter of the macro beginning with "``o!``", the
  argument is evaluated exactly once, and the corresponding ``g!`` symbol is
  bound to a gensym to hold the value. ::

    (defmacro! m [o!x]
      `(+ ~g!x ~g!x ~g!x))
    (setv l (list "abc"))
    (m (.pop l))  ; => "ccc"]]

  (setv os (lfor x (flatten args)
                 :if (and (isinstance x hy.models.Symbol)
                          (.startswith x "o!"))
                 x)
        gs (lfor s os
                 (hy.models.Symbol (+ "g!" (cut s 2 None))))
        gensyms (gfor sym (sfor x (flatten [gs body])
                                :if (and (isinstance x hy.models.Symbol)
                                         (.startswith x "g!"))
                                x)
                      x [sym `(hy.gensym ~(cut sym 2 None))]
                      x)
        res (hy.gensym "res"))
  (setv docstring None)
  (when (and body
             (isinstance (get body 0) str)
             (> (len body) 1))
    (setv [docstring #* body] body))

  `(defmacro ~name ~args
     ~docstring
     (setv ~@gensyms
           ~res ((fn [] ~@body)))
     `(do
        (setv ~~gs ~~os)
        ~~res)))

(defn macroexpand-all [model [module None] [macros None]]
  "As :hy:func:`hy.macroexpand`, but with recursive descent through the input model, attempting to expand all macro calls throughout the tree::

    (defmacro m [] 5)
    (print (hy.repr (hy.macroexpand '(m))))
      ; => '5
    (print (hy.repr (hy.macroexpand '(do (m)))))
      ; => '(do (m))
    (print (hy.repr (macroexpand-all '(do (m)))))
      ; => '(do 5)

  ``macroexpand-all`` even expands macros in unquoted portions of quasiquoted forms::

    (print (hy.repr (macroexpand-all '(do `[4 (m) ~(m) 6]))))
      ; => '(do `[4 (m) ~5 6])"

  (setv  quote-level 0  module (or module (hy.compiler.calling-module)))

  (defn expand [m]

    (when (not (and (isinstance m hy.models.Expression) m))
      (return))
    (setv [head #* args] m)
    (when (not (isinstance head hy.models.Symbol))
      (return))
    (setv mhead (hy.mangle head))
    (nonlocal quote-level)
    (when (and (= quote-level 0) (= mhead "quote"))
      (return m))

    (setv quote-adjustment None)
    (cond
      (and quote-level (in mhead ["unquote" "unquote_splice"]))
        (setv quote-adjustment -1)
      (= mhead "quasiquote")
        (setv quote-adjustment 1))
    (when quote-adjustment
      (+= quote-level quote-adjustment))
    (setv new (if
       (or
         quote-level
         quote-adjustment
         (in mhead (.split "unquote unquote_splice unpack_mapping except hyx_exceptXasteriskX finally else")))
           ; These stub macros would cause a crash if we tried to
           ; expand them.
      `(~head ~@(gfor  e args  (map-model e expand)))
      (map-hyseq
        (hy.macroexpand m module macros)
        (fn [e] (map-model e expand)))))
    (when quote-adjustment
      (-= quote-level quote-adjustment))

    new)

  (map-model model expand))


(defn map-model [x f]
  #[[Recursively apply a callback to some code. The unary function ``f`` is called on the object ``x``, calling :hy:func:`hy.as-model` first. If the return value isn't ``None``, it's converted to a model and used as the result. But if the return value is ``None``, and ``x`` isn't a :ref:`sequential model <hy:hysequence>`, then ``x`` is used as the result instead. ::

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

  (_map-model (hy.as-model x) f))

(defn _map-model [x f]
  (if (is-not (setx value (f x)) None)
    (hy.as-model value)
    (map-hyseq x (fn [contents]
      (gfor  elem contents  (_map-model elem f))))))

(defn map-hyseq [x f]

  "Apply the function ``f`` to the contents of the :ref:`sequential model <hy:hysequence>` ``x`` gathered into a tuple. ``f`` should return an iterable object. This result is then wrapped in the original model type, preserving attributes such as the brackets of an :class:`hy.models.FString`. ::

    (map-hyseq '[:a :b :c] (fn [x]
      (gfor  e x  (hy.models.Keyword (.upper e.name)))))
      ; => '[:A :B :C]

  Unlike :hy:func:`map-model`, ``map-hyseq`` isn't inherently recursive.

  If ``x`` isn't a sequential Hy model, it's returned as-is, without calling ``f``."

  (if (isinstance x hy.models.Sequence)
    ((type x)
      (f (tuple x))
      #** (cond
        (isinstance x hy.models.FString)
          {"brackets" x.brackets}
        (isinstance x hy.models.FComponent)
          {"conversion" x.conversion}
        True
          {}))
    x))


(defmacro with-gensyms [args #* body]

  #[[Evaluate ``body`` with each name in ``args`` (a list of symbols) bound to
  a gensym. The syntax ::

    (with-gensyms [a b c]
      …)

  is equivalent to ::

    (do
      (setv a (hy.gensym 'a))
      (setv b (hy.gensym 'b))
      (setv c (hy.gensym 'c))
      …)]]

  (setv syms [])
  (for [arg args]
    (.extend syms [arg `(hy.gensym '~arg)]))
  `(do
    (setv ~@syms)
    ~@body))


(defreader /

  #[[Read one identifier and interpret it as a one-shot import in the same
  way as :hy:class:`hy.I`. ::

    #/ os.curdir
      ; hy.I.os.curdir
      ; => "."
    (#/ os/path.basename "path/to/file")
      ; (hy.I.os/path/basename "path/to/file")
      ; => "file"]]

  (.slurp-space &reader)
  (setv [mod #* ident] (.split (.read-ident &reader) ".")
        imp `(hy.I ~(hy.mangle (.replace mod "/" "."))))
  (if ident
    `(. ~imp ~@(map hy.models.Symbol ident))
    imp))
