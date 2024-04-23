(import
  collections.abc [Iterable]
  itertools [islice tee])


(defn butlast [coll]
  "Return an iterator of all but the last item in ``coll``. ::

    (list (butlast (range 5)))  ; => [0 1 2 3]

  When ``coll`` is empty, the new iterator will also be empty."

  (drop-last 1 coll))


(defn coll? [x]
  #[[Return ``True`` if ``x`` inherits from :class:`collections.abc.Iterable`
  but not ``str`` or ``bytes``. ::

    (coll? ["abc"])         ; True
    (coll? {"a" 1 "b" 2})   ; True
    (coll? "abc")           ; False ]]

  (and
    (isinstance x Iterable)
    (not (isinstance x #(str bytes)))))


(defn distinct [coll]
  "Return an iterator from the original iterable ``coll`` with no
  duplicates. Duplicates are detected by calling :hy:func:`in
  <hy.pyops.in>` on a :class:`set`. Elements will be produced in order
  of their first appearance in ``coll``. ::

    (list (distinct [1 2 3 4 3 5 0 2]))  ; => [1 2 3 4 5 0]"

  (setv seen (set) citer (iter coll))
  (for [val citer]
    (when (not-in val seen)
      (yield val)
      (.add seen val))))


(defn drop-last [n coll]
  "Return an iterator of all but the last ``n`` elements in ``coll``.
  ``n`` must be nonnegative. ::

    (list (drop-last 3 (range 10)))  ; => [0 1 2 3 4 5 6]"

  (setv [copy1 copy2] (tee coll))
  (gfor  [x _] (zip copy1 (islice copy2 n None))  x))


(defn flatten [coll]
  #[=[Recurisvely collect all the elements and subelements of ``coll`` into a
  single list. :hy:func:`coll?` is used to decide whether objects should be
  descended into. ::

    (flatten ["foo" #(1 2) [1 [2 3] 4] "bar"])
      ; => ["foo" 1 2 1 2 3 4 "bar"]]=]
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
  "Return an iterator of all the elements of ``coll`` excepting the first. ::

    (list (rest (range 5)))  ; => [1 2 3 4]

  When ``coll`` is empty, the new iterator will also be empty."

  (islice coll 1 None))


(defn thru [a [b None] [step 1]]
  "A doubly inclusive version of :py:class:`range`. It takes the same
  arguments as ``range``, but includes the endpoint (given a
  compatible start point and step size). ::

    (thru 3)
      ; => [0 1 2 3]
    (thru 0 10 2)
      ; => [0 2 4 6 8 10]
    (thru 0 9 2)
      ; => [0 2 4 6 8]"

  (when (is b None)
    (setv [a b] [0 a]))
  (range a (+ b (if (> step 0) 1 -1)) step))
