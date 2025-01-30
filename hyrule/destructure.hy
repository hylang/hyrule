;;; Hy destructuring bind
#[=[
This module is heavily inspired by destructuring from Clojure and provides
similar semantics. It provides several macros that allow for destructuring
within their arguments, not unlike iterable unpacking, as in ``(setv [a #* b c]
(range 10))``, but also functioning on dictionaries and allowing several special
options.

Destructuring allows you to easily peek inside a data structure and assign names to values within. For example, suppose you have a data structure like this::

    (setv data {:players [{:name "Joe" :weapons [:sword :dagger]}
                          {:name "Max" :weapons [:axe :crossbow]}]
                :map "Dungeon"
                :tasks-remaining 4})

You could manually write out a lot of assignments like this::

    (setv map-name (.get data ':map)
          tasks-remaining (.get data ':tasks-remaining)
          tasks-completed (.get data ':tasks-completed)
          all-players (.get data ':players)
          name (.get (get all-players 0) ':name)
          weapon1 (get all-players 0 :weapons 0)
          weapon2 (get all-players 0 :weapons 1))

Or, for the same result, you could use a destructuring macro::

    (setv+ {[{name :name [weapon1 weapon2] :weapons} :as all-players] :players
            map-name :map
            :keys [tasks-remaining tasks-completed]}
           data)

.. warning::
   Variables which are not found in the data are silently set to ``None`` if no default value is specified. This is particularly important with ``defn+`` and ``fn+``. ::

      (defn+ some-function [arg1
                            {subarg2-1 "key"
                             :or {subarg2-1 20}
                             :as arg2}
                            [subarg3-1
                             :& subargs3-2+
                             :as arg3]]
        {"arg1" arg1  "arg2" arg2  "arg3" arg3
         "subarg2-1" subarg2-1  "subarg3-1" subarg3-1  "subargs3-2+" subargs3-2+})

      (some-function 1 {"key" 2} [3 4 5])
      ; => {"arg1" 1  "arg2" {"key" 2}  "arg3" [3 4 5]
      ;     "subarg2-1" 2  "subarg3-1" 3  "subargs3-2+" [4 5]}

      (some-function 1 2 [])
      ; => {"arg1" 1  "arg2" None  "arg3" []
      ;     "subarg2-1" 20  "subarg3-1" None  "subargs3-2+" []}

      (some-function)
      ; => {"arg1" None  "arg2" None  "arg3" None
      ;     "subarg2-1" 20  "subarg3-1" None  "subargs3-2+" None}

   Notice how a variable with a default value from an ``:or`` special option will fall back to that value instead of ``None``.

Pattern types
~~~~~~~~~~~~~

Several kinds of patterns are understood.

Dictionary patterns
-------------------

Dictionary patterns are specified using a :class:`hy.models.Dict`, where the keys corresponds to the symbols to be bound, and the values correspond to keys to be looked up in the data. ::

    (setv+ {a :a  b "b"  c #(1 0)}
           {:a 1  "b" 2  #(1 0) 3})
    [a b c] ; => [1 2 3]

A key in a dictionary pattern can also be one of the following special options:

- ``:or`` takes a dictionary of default values.
- ``:as`` takes a symbol, which is bound to the entire input dictionary.
- ``:keys`` takes a list of symbols, which are looked up as keywords in the data.
- ``:strs`` works like ``:keys``, but looks up strings instead of keywords.

For example::

    (setv+ {:keys [a b]  :strs [c d]  :or {b 2 d 4}  :as full}
           {:a 1  :b 2  "c" 3})
    [a b c d full] ; => [1 2 3 4 {:a 1  :b 2  "c" 3}]

The ordering of the special options and the ordinary variable names doesn't matter, but each special option can be used at most once.

If a lookup fails, and the symbol to be bound to doesn't have an associated default, ``None`` is bound instead::

   (setv+ {out "a"} {})
   (is out None) ; => True

List patterns
-------------

List patterns are specified with a :class:`hy.models.List`. The ``n``\th symbol in the pattern is bound to the ``n``\th value in the data, or ``None`` if the data has fewer than ``n`` values.

List patterns support these special options:

- ``:&`` takes a pattern, which is bound to the rest of the data. This pattern can be anything, including a dictionary pattern, which allows for keyword arguments.
- ``:as`` takes a symbol, which is bound to the whole input iterable.

If the special options are present, they must be last, with ``:&`` preceding ``:as`` if both are present. ::

    (setv+ [a b :& rest :as full] (range 5))
    [a b rest full] ; => [0 1 [2 3 4] [0 1 2 3 4]]

    (setv+ [a b :& {:keys [c d] :or {c 3}}] [1 2 :d 4 :e 5]
    [a b c d] ; => [1 2 3 4]

Note that list patterns call ``list`` on the data before binding the variables, so they can't be used with infinite iterators.

Iterator patterns
-----------------

Iterator patterns are specified with a :class:`hy.models.Expression`. They work the same as list patterns except that they only consume as much of the input as is required for matching, so they can be safely used with infinite iterators. ``:rest`` and ``:as`` create iterables instead of lists, and no recursive destructuring within ``:as`` is allowed. ::

    (import itertools [count islice])
    (setv+ (a b :& rest :as full) (count))
    [a b] ; => [0 1]
    (list (islice rest 5)) ; => [2 3 4 5 6]
    (list (islice full 5)) ; => [0 1 2 3 4]
]=]

(require
  hyrule.argmove [->>]
  hyrule.control [unless branch]
  hyrule.macrotools [defmacro! def-gensyms])
(import
  itertools [starmap chain count]
  functools [reduce]
  hy.pyops *
  hyrule.collections [by2s]
  hyrule.macrotools [parse-defn-like])

(defmacro setv+ [#* pairs]
#[=[

Take pairs of destructuring patterns and input data structures, assign to variables in the current scope as specified by the patterns, and return ``None``. ::

    (setv+ {a "apple"  b "banana"}  {"apple" 1  "banana" 2}
           [c d] [3 4])
    [a b c d] ; => [1 2 3 4]]=]
  (setv gsyms [])
  `(do
    (setv ~@(gfor [binds expr] (by2s pairs)
                  sym (destructure binds expr gsyms)
              sym))
    (del ~@gsyms)))

(defmacro dict=: [#* pairs]
  #[[

Take pairs of destructuring patterns and input data structures, and return a dictionary of bindings, where the keys are :class:`hy.models.Symbol` objects. The syntax is the same as that of :hy:func:`setv+`. ::

    (dict=: {a "apple"  b "banana"}
            {"apple" 1  "banana" 2})
      ; => {'a 1  'b 2}]]

  (setv gsyms [])
  (def-gensyms result)
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
    (def-gensyms dcoll)
    (setv result [dcoll expr]
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
  (setv [bs magics] (find-magics binds))
  (def-gensyms copy-iter tee)
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

(defmacro defn+ [#* args]
  "As :hy:func:`defn`, but the lambda list is destructured as a list pattern. The usual special parameter names in lambda lists, such as ``#*``, aren't special here. No type annotations are allowed are in the lambda list, but a return-value annotation for the whole function is allowed."
  (destructuring-fn 'defn args))

(defmacro fn+ [#* args]
  "A version of :hy:func:`fn` that destructures like :hy:func:`defn+`."
  (destructuring-fn 'fn args))

(defn destructuring-fn [like args]
  (setv [headers params doc body] (parse-defn-like like args))
  (def-gensyms args kwargs)
  `(~@headers [#* ~args #** ~kwargs]
     ~doc
     ~(_expanded-setv params args kwargs)
     ~@body))

(defmacro let+ [args #* body]
  "A version of :hy:func:`let` that allows destructuring patterns in place of plain symbols for binding."
  (when (% (len args) 2)
        (raise (ValueError "let bindings must be paired")))
  `(let ~(lfor [bs expr] (by2s args)
               sym (destructure bs expr)
           sym)
     ~@body))
