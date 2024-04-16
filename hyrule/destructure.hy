;;; Hy destructuring bind
"
This module is heavily inspired by destructuring from Clojure and provides very
similar semantics. It provides several macros that allow for destructuring within
their arguments.

Destructuring allows one to easily peek inside a data structure and assign names to values within. For example, ::

    (setv+ {[{name :name [weapon1 weapon2] :weapons} :as all-players] :players
            map-name :map
            :keys [tasks-remaining tasks-completed]}
           data)

would be equivalent to ::

    (setv map-name (.get data ':map)
          tasks-remaining (.get data ':tasks-remaining)
          tasks-completed (.get data ':tasks-completed)
          all-players (.get data ':players)
          name (.get (get all-players 0) ':name)
          weapon1 (get (.get (get all-players 0) ':weapons) 0)
          weapon2 (get (.get (get all-players 0) ':weapons) 1))

where ``data`` might be defined by ::

    (setv data {:players [{:name Joe :weapons [:sword :dagger]}
                          {:name Max :weapons [:axe :crossbow]}]
                :map \"Dungeon\"
                :tasks-remaining 4})

This is similar to unpacking iterables in Python, such as ``a, *b, c = range(10)``, however it also works on dictionaries, and has several special options.

.. warning::
   Variables which are not found in the expression are silently set to ``None`` if no default value is specified. This is particularly important with ``defn+`` and ``fn+``. ::

      (defn+ some-function [arg1
                            {subarg2-1 \"key\"
                             :or {subarg2-1 20}
                             :as arg2}
                            [subarg3-1
                             :& subargs3-2+
                             :as arg3]]
        {\"arg1\" arg1  \"arg2\" arg2  \"arg3\" arg3
         \"subarg2-1\" subarg2-1  \"subarg3-1\" subarg3-1  \"subargs3-2+\" subargs3-2+})

      (some-function 1 {\"key\" 2} [3 4 5])
      ; => {\"arg1\" 1  \"arg2\" {\"key\" 2}  \"arg3\" [3 4 5]
      ;     \"subarg2-1\" 2  \"subarg3-1\" 3  \"subargs3-2+\" [4 5]}

      (some-function 1 2 [])
      ; => {\"arg1\" 1  \"arg2\" None  \"arg3\" []
      ;     \"subarg2-1\" 20  \"subarg3-1\" None  \"subargs3-2+\" []}

      (some-function)
      ; => {\"arg1\" None  \"arg2\" None  \"arg3\" None
      ;     \"subarg2-1\" 20  \"subarg3-1\" None  \"subargs3-2+\" None}

   Note that variables with a default value from an ``:or`` special option will fallback to their default value instead of being silently set to ``None``.

Pattern types
~~~~~~~~~~~~~

Several kinds of patterns are understood.

Dictionary patterns
-------------------

Dictionary patterns are specified using dictionaries, where the keys corresponds to the symbols which are to be bound, and the values correspond to which key needs to be looked up in the expression for the given symbol. ::

    (setv+ {a :a b \"b\" c #(1 0)} {:a 1 \"b\" 2 #(1 0) 3})
    [a b c] ; => [1 2 3]

The keys can also be one of the following 4 special options: ``:or``, ``:as``, ``:keys``, ``:strs``.

- ``:or`` takes a dictionary of default values.
- ``:as`` takes a variable name which is bound to the entire expression.
- ``:keys`` takes a list of variable names which are looked up as keywords in the expression.
- ``:strs`` is the same as ``:keys`` but uses strings instead.

The ordering of the special options and the variable names doesn't matter, however each special option can be used at most once. ::

    (setv+ {:keys [a b] :strs [c d] :or {b 2 d 4} :as full} {:a 1 :b 2 \"c\" 3})
    [a b c d full] ; => [1 2 3 4 {:a 1 :b 2 \"c\" 3}]

Variables which are not found in the expression are set to ``None`` if no default value is specified.

List patterns
-------------

List patterns are specified using lists. The nth symbol in the pattern is bound to the nth value in the expression, or ``None`` if the expression has fewer than n values.

There are 2 special options: ``:&`` and ``:as``.

- ``:&`` takes a pattern which is bound to the rest of the expression. This pattern can be anything, including a dictionary, which allows for keyword arguments.
- ``:as`` takes a variable name which is bound to the entire expression.

If the special options are present, they must be last, with ``:&`` preceding ``:as`` if both are present. ::

    (setv+ [a b :& rest :as full] (range 5))
    [a b rest full] ; => [0 1 [2 3 4] [0 1 2 3 4]]

    (setv+ [a b :& {:keys [c d] :or {c 3}}] [1 2 :d 4 :e 5]
    [a b c d] ; => [1 2 3 4]

Note that this pattern calls ``list`` on the expression before binding the variables, and hence cannot be used with infinite iterators.

Iterator patterns
-----------------

Iterator patterns are specified using round brackets. They are the same as list patterns, but can be safely used with infinite generators. The iterator pattern does not allow for recursive destructuring within the ``:as`` special option.
"

(require
  hyrule.argmove [->>]
  hyrule.control [unless branch]
  hyrule.macrotools [defmacro!])
(import
  itertools [starmap chain count]
  functools [reduce]
  hy.pyops *
  hyrule.iterables [rest]
  hyrule.collections [by2s])

(defmacro setv+ [#* pairs]
  "Assignment with destructuring for both mappings and iterables.

  Destructuring equivalent of ``setv``. Binds symbols found in a pattern
  using the corresponding expression.

  Examples:
    ::

       (setv+ pattern_1 expression_1 ...  pattern_n expression_n)
  "
  (setv gsyms [])
  `(do
    (setv ~@(gfor [binds expr] (by2s pairs)
                  sym (destructure binds expr gsyms)
              sym))
    (del ~@gsyms)))

(defmacro dict=: [#* pairs]
  "Destructure into dict

  Same as ``setv+``, except returns a dictionary with symbols to be defined,
  instead of actually declaring them."
  (setv gsyms []
        result (hy.gensym 'dict=:))
  `(do
     (setv ~result {}
           ~@(gfor [binds expr] (by2s pairs)
                   [k v] (by2s [#* (destructure binds expr gsyms)])
                   syms [(if (in k gsyms) k `(get ~result '~k)) v]
               syms))
     (del ~@gsyms)
     ~result))

(defn destructure [binds expr [gsyms None]]
  "
  Destructuring bind.

  Implements the core logic, which would be useful for macros that want to make
  use of destructuring.

  Binding forms may be nested.
  :as and :& are magic in [] and () binds. See dest-list and dest-iter.
  :as :or :strs and :keys are magic in {} binds. See des-dict.

  In [] and () binds the magic keywords must come after the sequential symbols
  and :as must be last, if present.
  "
  (defn dispatch [f]
    (setv dcoll (hy.gensym f.__name__)
      result [dcoll expr]
      seen #{})
    (defn found [magic target]
      (when (= magic target)
        (when (in magic seen)
          (raise (SyntaxError (.format "Duplicate :{} in destructure."
                                       magic.name))))
         (.add seen magic)
         True))
    (unless (is gsyms None)
      (.append gsyms dcoll))
    (f dcoll result found binds gsyms))
  (branch (isinstance binds it)
       hy.models.Symbol [binds expr]
       hy.models.Dict (dispatch dest-dict)
       hy.models.Expression (dispatch dest-iter)
       hy.models.List (dispatch dest-list)
       else (raise (SyntaxError (+ "Malformed destructure. Unknown binding form: "
                             (repr binds))))))

(defn iterable->dict [xs]
  (if (% (len xs) 2)
    (raise (SyntaxError
             f"Cannot make dictionary out of odd-length iterable {xs}"))
    (dict (by2s xs))))

(defn dest-dict [ddict result found binds gsyms]
  "Destructuring bind for mappings.

  Binding forms may be nested.
  Targets from ``{}`` binds look up their value.
  For example, ``(destructure '{x :a  y :b} {:a 1  :b 2})``
  binds ``x`` to ``1`` and ``y`` to ``2``.
  To avoid duplication in common cases,
  the ``{:strs [foo bar]}`` option will look up \"foo\" and \"bar\"
  and bind them to the same name, just like ``{foo \"foo\" bar \"bar\"}``.
  Similarly, ``:keys [foo bar]`` works like ``{foo :foo bar :bar}``.
  Use the ``:as foo`` option to bind the whole mapping to ``foo``.
  Use the ``:or {foo 42}`` option to to bind ``foo`` to ``42`` if
  ``foo`` is requested, but not present in expr.
  "
  (setv binds (iterable->dict binds)
        default (iterable->dict (.get binds ':or '{})))
  (defn expand-lookup [target key]
    [target `(if (hasattr ~ddict "get")
               (.get ~ddict
                     ~(if (isinstance key hy.models.Keyword)
                          `(quote ~key) key)
                     ~(when (isinstance target hy.models.Symbol)
                            (.get default target)))
               ~(when (and (isinstance target hy.models.Symbol)
                           (not (is (.get default target) None)))
                   (.get default target)))])
  (defn get-as [to-key targets]
    (lfor t targets
          sym (expand-lookup t (to-key t))
      sym))
  (->> (.items binds)
       (starmap (fn [target lookup]
                  (branch (found target it)
                    ':or []
                    ':as [lookup `(if (hasattr ~ddict "get") ~ddict None)]
                    ':strs (get-as str lookup)
                    ':keys (get-as (fn [x] (hy.models.Keyword (hy.unmangle x))) lookup)
                    else (destructure #* (expand-lookup target lookup) gsyms))))
       ((fn [xs] (reduce + xs result)))))

(defn find-magics [bs [keys? False] [as? False]]
  (setv x (when bs (get bs 0))
        y (when (> (len bs) 1) (get bs 1)))
  (if (is x None)
    [[] []]
    (if (isinstance x hy.models.Keyword)
      (if (or (is y None) (isinstance y hy.models.Keyword))
        (raise (SyntaxError
                 (.format "Unpaired keyword :{} in list destructure"
                          x.name)))
        (if as?
          (raise
            (SyntaxError ":as must be final magic in sequential destructure"))
          (map + [[] [[x y]]] (find-magics (cut bs 2 None) True (= ':as x)))))
      (if keys?
        (raise (SyntaxError f"Non-keyword argument {x} after keyword"))
        (map + [[x] []] (find-magics (cut bs 1 None)))))))

(defn dest-list [dlist result found binds gsyms]
  "
  Destructuring bind for random-access sequences.

  Binding forms may be nested.
  Targets from ``[]`` binds are assigned by index order.
  Use ``:& bar`` option in binds to bind the remaining slice to ``bar``.
  The ``:&`` argument can also be recursively destructed asdfasdf.
  Use ``:as foo`` option in binds to bind the whole iterable to ``foo``.
  For example, try
  ``(destructure '[a b [c :& d :as q] :& {:keys [e f]} :as full]
                 [1 2 [3 4 5] :e 6 :f 7])``
  "
  (.append result `(try
                     (list ~(.pop result))
                     (except [e TypeError]
                       None)))
  (setv [bs magics] (find-magics binds)
        n (len bs)
        bres (lfor [i t] (enumerate bs)
               (destructure t `(.get (dict (enumerate (or ~dlist []))) ~i) gsyms))
        err-msg "Invalid magic option :{} in list destructure"
        mres (lfor [m t] magics
               (branch (found m it)
                 ':as [t dlist]
                 ':& (destructure t (if (isinstance t hy.models.Dict)
                                      `(if (not (is ~dlist None))
                                           (dict (zip
                                                   (cut ~dlist ~n None 2)
                                                   (cut ~dlist ~(+ n 1) None 2)))
                                           None)
                                      `(if (not (is ~dlist None))
                                           (cut ~dlist ~n None)
                                           None))
                                  gsyms)
                 else (raise (SyntaxError (.format err-msg m.name))))))
  (reduce + (chain bres mres) result))

(defn dest-iter [diter result found binds gsyms]
  "
  Destructuring bind for iterables.

  Binding forms may be nested.
  Unlike ``[]`` binds, ``()`` is safe for infinite iterators.
  Targets are assigned in order by pulling the next item from the iterator.
  Use the ``:&`` option to also return the remaining iterator.
  Use ``:as foo`` option in binds to bind a copy of the whole iterator using
  ``itertools.tee`` to ``foo``.
  For example, try ``(destructure '(a b c :& more :as it) (count))``.
  "
  (setv [bs magics] (find-magics binds)
        copy-iter (hy.gensym)
        tee (hy.gensym))
  (if (in ':as (sfor  [x #* _] magics  x))
    (.extend result [diter `(do
                              (import itertools [tee :as ~tee])
                              (setv [~diter ~copy-iter] (~tee ~diter))
                              ~diter)])
    (.append result `(iter ~(.pop result))))
  (reduce +
          (+ (lfor t bs (destructure t `(next ~diter None) gsyms))
             (lfor [m t] magics
               (branch (found m it)
                 ':& [t diter]
                 ':as [t copy-iter])))
          result))

(defn _expanded-setv [actual args kwargs]
  (hy.macroexpand
    `(setv+ ~actual (+ (list ~args)
                          (lfor [k v] (.items ~kwargs)
                                s [(hy.models.Keyword k) v]
                            s)))))

(defmacro! defn+ [fn-name args #* doc+body]
  "Define function `fn-name` with destructuring within `args`.

  Note that `#*` etc have no special meaning and are
  intepretted as any other argument.
  "
  (setv [doc body] (if (isinstance (get doc+body 0) str)
                     [(get doc+body 0) (rest doc+body)]
                     [None doc+body]))
  `(defn ~fn-name [#* ~g!args #** ~g!kwargs]
     ~doc
     ~(_expanded-setv args g!args g!kwargs)
     ~@body))

(defmacro! fn+ [args #* body]
  "Return anonymous function with destructuring within `args`

  Note that `*`, `/`, etc have no special meaning and are
  intepretted as any other argument.
  "
  `(fn [#* ~g!args #** ~g!kwargs]
     ~(_expanded-setv args g!args g!kwargs)
     ~@body))

(defmacro! defn/a+ [fn-name args #* doc+body]
  "Async variant of ``defn+``."
  (setv [doc body] (if (isinstance (get doc+body 0) str)
                     [(get doc+body 0) (rest doc+body)]
                     [None doc+body]))
  `(defn/a ~fn-name [#* ~g!args #** ~g!kwargs]
     ~doc
     ~(_expanded-setv args g!args g!kwargs)
     ~@body))

(defmacro! fn/a+ [args #* body]
  "Async variant of ``fn+``."
  `(fn/a [#* ~g!args #** ~g!kwargs]
     ~(_expanded-setv args g!args g!kwargs)
     ~@body))

(defmacro let+ [args #* body]
  "let macro with full destructuring with `args`"
  (when (% (len args) 2)
        (raise (ValueError "let bindings must be paired")))
  `(let ~(lfor [bs expr] (by2s args)
               sym (destructure bs expr)
           sym)
     ~@body))
