(import
  functools [partial]
  hyrule.iterables [coll?])


(defreader s
  #[[Read one form and interpret it like an index argument of
  :hy:func:`ncut`. The "s" stands for "slice". ::

   (setv x (list (range 10)))
   (get x #s 1:4:2)
     ; …is equivalent to…
   (ncut x 1:4:2)
     ; …is equivalent to…
   (get x (slice 1 4 2))]]
  (setv key (.parse-one-form &reader))
  (if (isinstance key hy.models.Expression) (_parse-indexing `(: ~@key))
      (_parse-indexing key)))


(defn assoc [coll #* kvs #** kwargs]
  #[[Associate key-value pairs by assigning to elements of ``coll``. Thus, ::

    (assoc coll  k1 v1  k2 v2  k3 v3)

  is equivalent to ::

    (setv (get coll k1) v1)
    (setv (get coll k2) v2)
    (setv (get coll k3) v3)

  except ``coll`` is evaluated exactly once. Notice that this implies
  the return value is ``None``, not ``coll`` or one of the newly
  assigned elements.

  Keyword arguments work like positional arguments with the keyword
  used as a string key, subject to Hy's usual mangling rules. Thus,
  ``(assoc coll :foo-bar 1)`` is equivalent to ``(assoc coll "foo_bar"
  1)`` or ``(setv (get coll "foo_bar") 1)``. Assignments for keyword
  arguments occur after (and thus may override) assignments for
  positional arguments.]]

  (when (% (len kvs) 2)
    (raise (ValueError "`assoc` takes an odd number of arguments (not counting `#** kwargs`)")))
  (for [[k v] (by2s kvs)]
    (setv (get coll k) v))
  (for [[k v] (.items kwargs)]
    (setv (get coll k) v)))


(defmacro ncut [seq key1 #* keys]

  #[=[Shorthand for ``(get seq …)`` with various kinds of complex indices as used
  by `NumPy <https://numpy.org/doc/stable/user/basics.indexing.html>`__ and
  `pandas <https://pandas.pydata.org/pandas-docs/stable/user_guide/indexing.html>`__.
  Thus ``ncut`` provides an *n*-dimensional equivalent of :hy:func:`cut <hy.pyops.cut>`.

  Simple uses are identical to :hy:func:`get <hy.pyops.get>`::

    (ncut x i)  ; (get x i)

  When multiple arguments are provided, they're combined into a tuple::

    (ncut x i1 i2)  ; (get x #(i1 i2))

  A keyword, or a symbol containing a colon, is understood as
  shorthand for a :class:`slice` of integer literals. An omitted
  integer is understood as ``None``, as in Python's own slicing
  syntax. ::

    (ncut x :)      ; (get x (slice None None))
    (ncut x 1:2)    ; (get x (slice 1 2))
    (ncut x 1:)     ; (get x (slice 1 None))
    (ncut x 1:2:3)  ; (get x (slice 1 2 3))
    (ncut x ::2)    ; (get x (slice None None 2))

  An expression of the form ``(: … )`` is understood as ``(slice …)``::

    (ncut x (: a b))  ; (get x (slice a b))

  Here are some executable examples::

    (ncut (list (range 10)) 2:8:2)
      ; => [2 4 6]

    (import numpy :as np)
    (setv a (.reshape (np.arange 36) #(6 6)))
    (ncut a 3: #(0 2 5))
      ; => array([[18, 20, 23],
      ;           [24, 26, 29],
      ;           [30, 32, 35]])
    (ncut a ... 0)
      ; => array([ 0,  6, 12, 18, 24, 30])

    (import pandas :as pd)
    (setv df (pd.DataFrame [[1 2 3 4] [5 6 7 8]]
                           :columns (list "ABCD")))
    (ncut df.loc : ["B" "A"])
      ; =>    B  A
      ;    0  2  1
      ;    1  6  5]=]

  `(get ~seq ~(if keys
               `#(~@(map _parse-indexing #(key1 #* keys)))
               (_parse-indexing key1))))


(defn _parse-indexing [sym]
    (cond
      (and (isinstance sym hy.models.Expression) (= (get sym 0) :))
        `(slice ~@(cut sym 1 None))

      (and (isinstance sym #(hy.models.Keyword hy.models.Symbol))
            (in ":" (str sym)))
        (try
           `(slice ~@(lfor
             index (.split (str sym) ":")
             (when index (int index))))
           (except [ValueError] sym))

      True
        sym))


(defn by2s [x]
  #[[Returns the given iterable in pairs.
    (list (by2s (range 6))) => [#(0 1) #(2 3) #(4 5)] #]]
  (setv x (iter x))
  (while True
    (try
      (yield #((next x) (next x)))
      (except [StopIteration]
        (break)))))
