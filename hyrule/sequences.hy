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
        (if (< n 5) n (end-sequence)))

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

(import
 itertools [islice]
 hyrule.misc [inc])

(require hyrule.macrotools [defmacro/g!])

(defclass Sequence []
  "Container for construction of lazy sequences."

  (defn __init__ [self func]
    (setv (. self func) func)
    (setv (. self cache) []))

  (defn __getitem__ [self n]
    (if (isinstance n slice) 
      (do
        (when (= n.step 0) (raise (ValueError "slice step cannot be zero")))
        ; If any of the components are negative, the full Sequence must be
        ; evaluated so it can be iterated in reverse or with reference to the
        ; end of the Sequence
        (if (or (and (not? None n.start) (< n.start 0))
                (and (not? None n.stop) (< n.stop 0))
                (and (not? None n.step) (< n.step 0)))
          (do
            ; Force evaluation of the Sequence
            (len self)
            (iter (get self.cache n)))
          (islice (iter self) n.start n.stop n.step)))

      (do (when (< n 0)
            ; Call (len) to force the whole
            ; sequence to be evaluated.
            (len self))
          (while (<= (len self.cache) n)
            (.append self.cache (.func self (len self.cache))))
          (get self.cache n))))

  (defn __iter__ [self]
    (setv index 0)
    (try (while True
           (yield (get self index))
           (setv index (inc index)))
         (except [IndexError]
                 (return))))

  (defn __len__ [self]
    (setv index (len self.cache))
    (try (while True
           (get self index)
           (setv index (inc index)))
         (except [IndexError]
                 (len self.cache))))

  (setv max-items-in-repr 10)

  (defn __str__ [self]
    (setv items (list (islice self (inc self.max-items-in-repr))))
    (.format (if (> (len items) self.max-items-in-repr)
               "[{0}, ...]"
               "[{0}]")
             (.join ", " (map str items))))

  (defn __repr__ [self]
    (.__str__ self)))

(defmacro/g! seq [param #* seq-code]
  "Creates a sequence defined in terms of ``n``.

  Examples:
    => (seq [n] (* n n))
  "
  `(do
     (import hyrule.sequences [Sequence :as ~g!Sequence])
     (~g!Sequence (fn ~param (do ~@seq-code)))))

(defmacro/g! defseq [seq-name param #* seq-code]
  "Creates a sequence defined in terms of ``n`` and assigns it to a given name.

  Examples:
    => (defseq numbers [n] n)
  "
  `(do
     (import hyrule.sequences [Sequence :as ~g!Sequence])
     (setv ~seq-name (~g!Sequence (fn ~param (do ~@seq-code))))))

(defn end-sequence []
  "Signals the end of a sequence when an iterator reaches the given point of the sequence.

  Internally, this is done by raising
  ``IndexError``, catching that in the iterator, and raising
  ``StopIteration``

  Examples:
    ::

       => (seq [n] (if (< n 5) n (end-sequence)))"
  (raise (IndexError "list index out of range")))
