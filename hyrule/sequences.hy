"The sequences module contains a few macros for declaring sequences that are
evaluated only as much as the client code requires. Unlike generators, they
allow accessing the same element multiple times. They cache calculated values,
and the implementation allows for recursive definition of sequences without
resulting in recursive computation.

The simplest sequence can be defined as ``(seq [n] n)``. This defines a sequence
that starts as ``[0 1 2 3 ...]`` and continues forever. In order to define a
finite sequence, you need to call ``end-sequence`` to signal the end of the
sequence::

   (seq [n]
        \"sequence of 5 integers\"
        (cond (< n 5) n
              True (end-sequence)))

This creates the following sequence: ``[0 1 2 3 4]``. For such a sequence,
``len`` returns the amount of items in the sequence and negative indexing is
supported. Because both of these require evaluating the whole sequence, calling
one on an infinite sequence would take forever (or at least until available
memory has been exhausted).

Sequences can be defined recursively. For example, the Fibonacci sequence could
be defined as::

   (defseq fibonacci [n]
     \"infinite sequence of fibonacci numbers\"
     (cond (= n 0) 0
           (= n 1) 1
           True (+ (get fibonacci (- n 1))
                   (get fibonacci (- n 2)))))

This results in the sequence ``[0 1 1 2 3 5 8 13 21 34 ...]``.
"


(defclass Sequence []
  "A container type for lazy sequences."

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
  "Creates a sequence defined in terms of ``n``.

  Examples:
    => (seq [n] (* n n))
  "
  `(hy.I.hyrule.Sequence (gfor
    ~(get param 0) (hy.I.itertools.count)
    (do ~@seq-code))))

(defmacro defseq [seq-name param #* seq-code]
  "Creates a sequence defined in terms of ``n`` and assigns it to a given name.

  Examples:
    => (defseq numbers [n] n)
  "
  `(setv ~seq-name (hy.R.hyrule.seq ~param ~@seq-code)))

(defn end-sequence []
  "Signals the end of a sequence when an iterator reaches the given point of the sequence.

  Internally, this is done by raising
  ``IndexError``, catching that in the iterator, and raising
  ``StopIteration``

  Examples:
    ::

       => (seq [n] (if (< n 5) n (end-sequence)))"
  (raise (IndexError "sequence index out of range")))
