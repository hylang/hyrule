(import
  functools [partial]
  hyrule.iterables [coll?])


(defreader s
  "Shorthand tag macro for constructing slices using Python's sugared form.

  Examples:
    ::

       => #s 1:4:2
       (slice 1 4 2)
       => (get [1 2 3 4 5] #s 2::2)
       [3 5]

    Numpy makes use of ``Ellipsis`` in its slicing semantics so they can also be
    constructed with this macro in their sugared ``...`` form.
    ::

       => #s ...
       Ellipsis

    Slices can technically also contain strings (something pandas makes use of
    when slicing by string indices) and because Hy allows colons in identifiers,
    to construct these slices we have to use the form ``(...)``:
    ::

       => #s(\"colname\" 1 2)
       (slice \"colname\" 1 2)
  "
  (setv key (.parse-one-form &reader))
  (if (isinstance key hy.models.Expression) (_parse-indexing `(: ~@key))
      (_parse-indexing key)))


(defmacro assoc [coll k1 v1 #* other-kvs]
  "Associate key/index value pair(s) to a collection `coll` like a dict or list.

  ``assoc`` is used to associate a key with a value in a dictionary or to set an
  index of a list to a value. It takes at least three parameters: the *data
  structure* to be modified, a *key* or *index*, and a *value*. If more than
  three parameters are used, it will associate in pairs.

  Examples:
    ::

       => (do
       ...   (setv collection {})
       ...   (assoc collection \"Dog\" \"Bark\")
       ...   (print collection))
       {\"Dog\" \"Bark\"}

    ::

       => (do
       ...   (setv collection {})
       ...   (assoc collection \"Dog\" \"Bark\" \"Cat\" \"Meow\")
       ...   (print collection))
       {\"Cat\" \"Meow\"  \"Dog\" \"Bark\"}

    ::

       => (do
       ...   (setv collection [1 2 3 4])
       ...   (assoc collection 2 None)
       ...   (print collection))
       [1 2 None 4]

  .. note:: ``assoc`` modifies the datastructure in place and returns ``None``.
  "
  (when (% (len other-kvs) 2)
        (raise (ValueError "`assoc` takes an odd number of arguments")))
  (setv c (if other-kvs
            (hy.gensym "c")
            coll))
  `(setv ~@(+ (if other-kvs
                [c coll]
                [])
              (lfor [i x] (enumerate (+ #(k1 v1) other-kvs))
                    (if (% i 2) x `(get ~c ~x))))))


(defmacro ncut [seq key1 #* keys]
  "N-Dimensional ``cut`` macro with shorthand slice notation.

  Libraries like ``numpy`` and ``pandas`` extend Python's sequence
  slicing syntax to work with tuples to allow for elegant handling of
  multidimensional arrays (numpy) and multi-axis selections (pandas).
  A key in ``ncut`` can be any valid kind of index; specific,
  ranged, a numpy style mask. Any library can make use of tuple based
  slicing, so check with each lib for what is and isn't valid.

  Args:
    seq: Slicable sequence
    key1: A valid sequence index. What is valid can change from library to
      library.
    *keys: Additional indices. Specifying more than one index will expand
      to a tuple allowing multi-dimensional indexing.

  Examples:
    Single dimensional list slicing
    ::

       => (ncut (list (range 10)) 2:8:2)
       [2 4 6]

    numpy multidimensional slicing:
    ::

       => (setv a (.reshape (np.arange 36) #(6 6)))
       => a
       array([[ 0,  1,  2,  3,  4,  5],
              [ 6,  7,  8,  9, 10, 11],
              [12, 13, 14, 15, 16, 17],
              [18, 19, 20, 21, 22, 23],
              [24, 25, 26, 27, 28, 29],
              [30, 31, 32, 33, 34, 35]])
       => (ncut a #(0 1 2 3 4) #(1 2 3 4 5))
       array([ 1,  8, 15, 22, 29])
       => (ncut a 3: #(0 2 5))
       array([[18, 20, 23],
              [24, 26, 29],
              [30, 32, 35]])
       => (ncut a 1:-1:2 3:5)
       array([[ 9, 10],
              [21, 22]])
       => (ncut a ::2 3 None)
       array([[ 3],
              [15],
              [27]])
       => (ncut a ... 0)
       array([ 0,  6, 12, 18, 24, 30])

    Because variables can have colons in Hy (eg: ``abc:def`` is a valid identifier),
    the sugared slicing form only allows numeric literals. In order to construct slices
    that involve names and/or function calls, the form ``(: ...)`` can be used in an
    ``ncut`` expresion as an escape hatch to ``slice``:
    ::

       => (setv abc:def -2)
       => (hy.macroexpand '(ncut a abc:def (: (sum [1 2 3]) None abc:def)))
       (get a #(abc:def (slice (sum [1 2 3]) None abc:def)))

    Pandas allows extensive slicing along single or multiple axes:
    ::

       => (setv s1 (pd.Series (np.random.randn 6) :index (list \"abcdef\")))
       => s1
       a    0.687645
       b   -0.598732
       c   -1.452075
       d   -0.442050
       e   -0.060392
       f    0.440574
       dtype: float64

       => (ncut s1 (: \"c\" None 2))
       c   -1.452075
       e   -0.060392
       dtype: float64

    ::

       => (setv df (pd.DataFrame (np.random.randn 8 4)
                                 :index (pd.date-range \"1/1/2000\" :periods 8)
                                 :columns (list \"ABCD\")))
       => df
                          A         B         C         D
       2000-01-01 -0.185291 -0.803559 -1.483985 -0.136509
       2000-01-02 -3.290852 -0.688464  2.715168  0.750664
       2000-01-03  0.771222 -1.170541 -1.015144  0.491510
       2000-01-04  0.243287  0.769975  0.473460  0.407027
       2000-01-05 -0.857291  2.395931 -0.950846  0.299086
       2000-01-06 -0.195595  0.981791 -0.673646  0.637218
       2000-01-07 -1.022636 -0.854971  0.603573 -1.169342
       2000-01-08 -0.494866  0.783248 -0.064389 -0.960760

       => (ncut df.loc : [\"B\" \"A\"])
                          B         A
       2000-01-01 -0.803559 -0.185291
       2000-01-02 -0.688464 -3.290852
       2000-01-03 -1.170541  0.771222
       2000-01-04  0.769975  0.243287
       2000-01-05  2.395931 -0.857291
       2000-01-06  0.981791 -0.195595
       2000-01-07 -0.854971 -1.022636
       2000-01-08  0.783248 -0.494866

  .. note::

     For more info on the capabilities of multiindex slicing, check with the respective
     library.

     - `Pandas <https://pandas.pydata.org/pandas-docs/stable/user_guide/indexing.html>`_
     - `Numpy <https://numpy.org/doc/stable/reference/arrays.indexing.html>`_
  "
  `(get ~seq ~(if keys
               `#(~@(map _parse-indexing #(key1 #* keys)))
               (_parse-indexing key1))))


(defn _parse-indexing [sym]
    (cond
      (and (isinstance sym hy.models.Expression) (= (get sym 0) :))
        `(slice ~@(cut sym 1 None))

      (= sym '...)
        'Ellipsis

      (and (isinstance sym #(hy.models.Keyword hy.models.Symbol))
            (in ":" (str sym)))
        (try
           `(slice ~@(lfor
             index (.split (str sym) ":")
             (when index (int index))))
           (except [ValueError] sym))

      True
        sym))


(defn postwalk [f form]
  "Performs depth-first, post-order traversal of ``form``. Calls ``f`` on
  each sub-form, uses ``f`` 's return value in place of the original.

  Examples:
    ::

       => (import hyrule.contrib.walk [postwalk])
       => (setv trail '([1 2 3] [4 [5 6 [7]]]))
       => (defn walking [x]
       ...   (print \"Walking\" x :sep \"\\n\")
       ...   x)
       => (postwalk walking trail)
       Walking
       1
       Walking
       2
       Walking
       3
       Walking
       hy.models.Expression([
         hy.models.Integer(1),
         hy.models.Integer(2),
         hy.models.Integer(3)])
       Walking
       4
       Walking
       5
       Walking
       6
       Walking
       7
       Walking
       hy.models.Expression([
         hy.models.Integer(7)])
       Walking
       hy.models.Expression([
         hy.models.Integer(5),
         hy.models.Integer(6),
         hy.models.List([
           hy.models.Integer(7)])])
       Walking
       hy.models.Expression([
         hy.models.Integer(4),
         hy.models.List([
           hy.models.Integer(5),
           hy.models.Integer(6),
           hy.models.List([
             hy.models.Integer(7)])])])
       Walking
       hy.models.Expression([
         hy.models.List([
           hy.models.Integer(1),
           hy.models.Integer(2),
           hy.models.Integer(3)]),
         hy.models.List([
           hy.models.Integer(4),
           hy.models.List([
             hy.models.Integer(5),
             hy.models.Integer(6),
             hy.models.List([
               hy.models.Integer(7)])])])])
       '([1 2 3] [4 [5 6 [7]]]))
  "
  (walk (partial postwalk f) f form))


(defn prewalk [f form]
  "Performs depth-first, pre-order traversal of ``form``. Calls ``f`` on
  each sub-form, uses ``f`` 's return value in place of the original.

  Examples:
    ::

       => (import hyrule.contrib.walk [prewalk])
       => (setv trail '([1 2 3] [4 [5 6 [7]]]))
       => (defn walking [x]
       ...  (print \"Walking\" x :sep \"\\n\")
       ...  x)
       => (prewalk walking trail)
       Walking
       hy.models.Expression([
         hy.models.List([
           hy.models.Integer(1),
           hy.models.Integer(2),
           hy.models.Integer(3)]),
         hy.models.List([
           hy.models.Integer(4),
           hy.models.List([
             hy.models.Integer(5),
             hy.models.Integer(6),
             hy.models.List([
               hy.models.Integer(7)])])])])
       Walking
       hy.models.List([
         hy.models.Integer(1),
         hy.models.Integer(2),
         hy.models.Integer(3)])
       Walking
       1
       Walking
       2
       Walking
       3
       Walking
       hy.models.List([
         hy.models.Integer(4),
         hy.models.List([
           hy.models.Integer(5),
           hy.models.Integer(6),
           hy.models.List([
             hy.models.Integer(7)])])])
       Walking
       4
       Walking
       hy.models.List([
         hy.models.Integer(5),
         hy.models.Integer(6),
         hy.models.List([
           hy.models.Integer(7)])])
       Walking
       5
       Walking
       6
       Walking
       hy.models.List([
         hy.models.Integer(7)])
       Walking
       7
       '([1 2 3] [4 [5 6 [7]]])
  "
  (walk (partial prewalk f) (fn [x] x) (f form)))


(defn walk [inner outer form]
  "``walk`` traverses ``form``, an arbitrary data structure. Applies
  ``inner`` to each element of form, building up a data structure of the
  same type.  Applies ``outer`` to the result.

  Examples:
    ::

       => (import hyrule.contrib.walk [walk])
       => (setv a '(a b c d e f))
       => (walk ord (fn [x] x)  a)
       '(97 98 99 100 101 102)

    ::

       => (walk ord (fn [x] (get x 0)) a)
       97
  "
  (cond
    (isinstance form hy.models.Expression)
      (outer (hy.models.Expression (map inner form)))
    (or (isinstance form #(hy.models.Sequence list)))
      ((type form) (outer (hy.models.Expression (map inner form))))
    (coll? form)
      (walk inner outer (list form))
    True
      (outer form)))

(defn by2s [x]
  #[[Returns the given iterable in pairs.
    (list (by2s (range 6))) => [#(0 1) #(2 3) #(4 5)] #]]
  (setv x (iter x))
  (while True
    (try
      (yield #((next x) (next x)))
      (except [StopIteration]
        (break)))))
