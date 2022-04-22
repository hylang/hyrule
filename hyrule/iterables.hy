(import
  collections.abc [Iterable]
  itertools [islice tee])


(defn butlast [coll]
  "Returns an iterator of all but the last item in *coll*.

  Examples:
    ::

       => (list (butlast (range 10)))
       [0 1 2 3 4 5 6 7 8]

    ::

       => (list (butlast [1]))
       []

    ::

       => (list (butlast []))
       []

    ::

       => (import itertools [count islice])
       => (list (islice (butlast (count 10)) 0 5))
       [10 11 12 13 14]
  "
  (drop-last 1 coll))


(defn coll? [coll]
  "Returns ``True`` if *x* inherits from ``Iterable`` but not ``str``
  or ``bytes``.

  Examples:
    ::

       => (coll? [1 2 3 4])
       True

    ::

       => (coll? {\"a\" 1 \"b\" 2})
       True

    ::

       => (coll? \"abc\")
       False
  "
  (and
    (isinstance coll Iterable)
    (not (isinstance coll #(str bytes)))))


(defn distinct [coll]
  "Return a generator from the original collection `coll` with no duplicates.

  Examples:
    ::

       => (list (distinct [ 1 2 3 4 3 5 2 ]))
       [1 2 3 4 5]

    ::

       => (list (distinct []))
       []

    ::

       => (list (distinct (iter [ 1 2 3 4 3 5 2 ])))
       [1 2 3 4 5]
  "
  (setv seen (set) citer (iter coll))
  (for [val citer]
    (when (not-in val seen)
      (yield val)
      (.add seen val))))


(defn drop-last [n coll]
  "Return a sequence of all but the last `n` elements in `coll`.

  Returns an iterator of all but the last *n* items in *coll*. Raises
  ``ValueError`` if *n* is negative.

  Examples:
    ::

       => (list (drop-last 5 (range 10 20)))
       [10 11 12 13 14]

    ::

       => (list (drop-last 0 (range 5)))
       [0 1 2 3 4]

    ::

       => (list (drop-last 100 (range 100)))
       []

    ::

       => (import itertools [count islice])
       => (list (islice (drop-last 100 (count 10)) 5))
       [10 11 12 13 14]
  "
  (setv [copy1 copy2] (tee coll))
  (gfor  [x _] (zip copy1 (islice copy2 n None))  x))


(defn flatten [coll]
  "Return a single flat list expanding all members of `coll`.

  Returns a single list of all the items in *coll*, by flattening all
  contained lists and/or tuples.

  Examples:
    ::

       => (flatten [1 2 [3 4] 5])
       [1 2 3 4 5]

    ::

       => (flatten [\"foo\" #(1 2) [1 [2 3] 4] \"bar\"])
       [\"foo\" 1 2 1 2 3 4 \"bar\"]
  "
  (if (coll? coll)
    (_flatten coll [])
    (raise (TypeError (.format "{0!r} is not a collection" coll)))))

(defn _flatten [coll result]
  (if (coll? coll)
    (do (for [b coll]
          (_flatten b result)))
    (.append result coll))
  result)


(defn rest [coll]
  "Get all the elements of `coll`, except the first.

  ``rest`` takes the given collection and returns an iterable of all but the
  first element.

  Examples:
    ::

       => (list (rest (range 10)))
       [1 2 3 4 5 6 7 8 9]

    Given an empty collection, it returns an empty iterable::

       => (list (rest []))
       []
  "
  (islice coll 1 None))
