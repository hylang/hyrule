(defclass Sequence []
  "A wrapper for iterables that caches values and supports :py:func:`len`, indexing, and slicing. For example::

    (setv s (Sequence (gfor  x (range 10)  (** x 2))))
    (print (len s))           ; => 10
    (print (get s 2))         ; => 4
    (print (get s -1))        ; => 81
    (print (list (cut s 3)))  ; => [0, 1, 4]

  ``Sequence`` supports infinite iterables, but trying to compute the length of such a sequence or look up a negative index will of course fail."

  (defn __init__ [self iterable]
    (setv self.it (iter iterable))
    (setv self.cache []))

  (defn _wrap [self n]
    (if (< n 0)
      (+ n (len self))
      n))

  (defn __getitem__ [self ix]

    (when (hasattr ix "start")
      ; `ix` is a `slice` object.
      (return (Sequence ((fn []
        (setv step (if (is ix.step None) 1 ix.step))
        (setv n (cond
          (is-not ix.start None) (._wrap self ix.start)
          (< step 0)             (- (len self) 1)
          True                   0))
        (setv stop (cond
          (is-not ix.stop None) (._wrap self ix.stop)
          (< step 0)            -1
          True                  Inf))
        (while (if (< step 0) (> n stop) (< n stop))
          (yield (get self n))
          (+= n step)))))))

    ; Otherwise, `ix` should be an integer.
    (setv ix (._wrap self ix))
    (when (< ix 0)
      (end-sequence))
    ; Build up the cache until we have the element we need.
    (while (<= (len self.cache) ix)
      (.append self.cache (try
        (next self.it)
        (except [StopIteration]
          (end-sequence)))))
    (get self.cache ix))

  (defn __len__ [self]
    (try
      (for [n (hy.I.itertools.count (len self.cache))]
        (get self n))
      (except [IndexError]
        (return n))))

  (setv max-items-in-repr 10)

  (defn __repr__ [self]
    (setv items (cut self (+ self.max-items-in-repr 1)))
    (.format "Sequence([{}{}])"
      (.join ", " (map repr (cut items self.max-items-in-repr)))
      (if (> (len items) self.max-items-in-repr) ", ..." ""))))

(defmacro seq [param #* seq-code]
  "Define a :hy:class:`Sequence` with code to compute the *n*-th element, where *n* starts from 0. The first argument is a literal list with a symbol naming the parameter, like the lambda list of :hy:func:`defn`, and the other arguments are the code to be evaluated. ::

    (setv s (seq [n] (** n 2)))
    (print (get s 2))  ; => 4

  You can define the function recursively by getting previous elements of the sequence::

    (setv fibonacci (seq [n]
      (if (< n 2)
        n
        (+
          (get fibonacci (- n 1))
          (get fibonacci (- n 2))))))
    (print (list (cut fibonacci 7)))  ; => [0, 1, 1, 2, 3, 5, 8]

  To define a finite sequence, call :hy:func:`end-sequence` when the argument is too large::

    (setv s (seq [n]
      (if (< n 5)
        (** n 2)
        (end-sequence))))
    (print (list s))  ; => [0, 1, 4, 9, 16]"
  `(hy.I.hyrule.Sequence (gfor
    ~(get param 0) (hy.I.itertools.count)
    (do ~@seq-code))))

(defmacro defseq [seq-name param #* seq-code]
  "Shorthand for assigning :hy:func:`seq` to a symbol. ``(defseq sequence-name [n] ...)`` is equivalent to ``(setv sequence-name (seq [n] ...))``."
  `(setv ~seq-name (hy.R.hyrule.seq ~param ~@seq-code)))

(do-mac
  (setv code #[[(raise (IndexError "sequence index out of range"))]])
  `(defn end-sequence []
    ~f"Shorthand for ``{code}``."
    ~(hy.read code)))
