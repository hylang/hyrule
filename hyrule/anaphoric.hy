"Hyrule's anaphoric macros can make functional programming more concise and
easier to read. An anaphoric macro assigns values to designated symbols
(typically ``it``) that may be used in the code passed to the macro."


(require
  hyrule.macrotools [defmacro!]
  hyrule.argmove [->])


(defmacro ap-if [test true-value [false-value None]]
  #[[As :hy:func:`if`, but the result of the test form is named ``it`` in
  the subsequent forms, and the else-clause is optional. ::

      (import os)
      (ap-if (.get os.environ "PYTHONPATH")
         (print "Your PYTHONPATH is" it))]]
  `(let [it ~test]
     (if it ~true-value ~false-value)))


(defmacro ap-each [xs #* body]
  "Evaluate the body forms for each element ``it`` of ``xs`` and return
  ``None``. ::

    (ap-each [1 2 3] (print it))"
  `(let [it None] (for [it ~xs] ~@body)))


(defmacro ap-each-while [xs form #* body]
  "As :hy:func:`ap-each`, but the form ``pred`` is run before the body forms on
  each iteration, and the loop ends if ``pred`` is false. ::

    (ap-each-while [1 2 3 4 5 6] (< it 4) (print it))
      ; Prints only 1, 2, and 3"
  `(let [it None]
    (for [it ~xs]
      (when (not ~form)
        (break))
      ~@body)))


(defmacro ap-map [form xs]
  "Create a generator like :py:func:`map` that yields each result of ``form``
  evaluated with ``it`` bound to successive elements of ``xs``. ::

    (list (ap-map (* it 2) [1 2 3]))  ; => [2 4 6]"
  `(gfor  it ~xs  ~form))


(defmacro ap-map-when [predfn rep xs]
  "As :hy:func:`ap-map`, but the predicate function ``predfn`` (yes, that's a
  function, not an anaphoric form) is applied to each ``it``, and the anaphoric
  mapping form ``rep`` is only applied if the predicate is true. Otherwise,
  ``it`` is yielded unchanged. ::

    (list (ap-map-when (fn [x] (% x 2)) (* it 2) [1 2 3 4]))
      ; => [2 2 6 4]
    (list (ap-map-when (fn [x] (= (% x 2) 0)) (* it 2) [1 2 3 4]))
      ; => [1 4 3 8]"
  `(gfor  it ~xs  (if (~predfn it) ~rep it)))


(defmacro ap-filter [form xs]
  "The :py:func:`filter` equivalent of :hy:func:`ap-map`.

  ::

    (list (ap-filter (> (* it 2) 6) [1 2 3 4 5]))
      ; => [4 5]"
  `(gfor  it ~xs  :if ~form  it))


(defmacro ap-reject [form xs]
  "Shorthand for ``(ap-filter (not form) xs)``. See :hy:func:`ap-filter`."
  `(gfor  it ~xs  :if (not ~form)  it))


(defmacro ap-dotimes [n #* body]
  "Shorthand for ``(ap-each (range n) body…)``. See :hy:func:`ap-each`."
  `(let [it None]
    (for [it (range ~n)]
      ~@body)))


(defmacro ap-first [form xs]
  "Evaluate the predicate ``form`` for each element ``it`` of ``xs``. When
  the predicate is true, stop and return ``it``. If the predicate is never
  true, return ``None``. ::

    (ap-first (> it 5) (range 10))  ; => 6"
  `(next
    (gfor  it ~xs  :if ~form  it)
    None))


(defmacro ap-last [form xs]
  "Evaluate the predicate ``form`` for every element ``it`` of ``xs``.
  Return the last element for which the predicate is true, or ``None`` if
  there is no such element. ::

    (ap-last (> it 5) (range 10))  ; => 9"
  (setv x (hy.gensym))
  `(let [it None]
    (setv ~x None)
    (for  [it ~xs  :if ~form]
      (setv ~x it))
    ~x))


(defmacro! ap-reduce [form o!xs [initial-value None]]
  "This macro is an anaphoric version of :py:func:`functools.reduce`. It works
  as follows:

  - Bind ``acc`` to the first element of ``xs``, bind ``it`` to the
    second, and evaluate ``form``.
  - Bind ``acc`` to the result, bind ``it`` to the third value of ``xs``,
    and evaluate ``form`` again.
  - Bind ``acc`` to the result, and continue until ``xs`` is exhausted.

  If ``initial-value`` is supplied, the process instead begins with
  ``acc`` set to ``initial-value`` and ``it`` set to the first element of
  ``xs``. ::

    (ap-reduce (+ it acc) (range 10))  ; => 45"
  `(let [acc None  it None]
    (setv acc ~(if (is initial-value None)
      `(do
        (setv ~g!xs (iter ~g!xs))
        (next ~g!xs))
      initial-value))
    (for [it ~g!xs]
      (setv acc ~form))
    acc))

(defmacro ap-when [test-form #* body]
  #[[As :hy:func:`when <hy.core.macros.when>`, but the result of the test
  form is named ``it`` in the subsequent forms. ::

    (setv variable -1)
    (ap-when (+ variable 2)
      (setv result it)
      (print it))
    (print result)]]
  `(let [it ~test-form]
     (when it ~@body)))

(defmacro ap-with [form #* body]
  "Shorthand for ``(with [it form] body…)``. See :hy:func:`with`."
  `(with [it ~form]
     ~@body))

(defreader %
  "Define an anonymous function with an implicit parameter list,
  similarly to Clojure's literal anonymous functions. A single form is
  read and interpreted as the body of the function. The first
  parameter is named ``%1``, the second ``%2``, and so on. ::

    (list (map #%(+ %1 3) (range 5)))  ; => [3 4 5 6 7]

  The number of parameters is set by the largest ``%i`` symbol that
  appears in the code::

    (setv f #%(+ %1 %3))
    (f 1 10 100)  ; => 101

  Use ``%*`` for a ``#* args`` parameter and ``%**`` for a ``#**
  kwargs`` parameter::

    (#%[%1 %*] 1 2 3)  ; => [1 #(2 3)]

  The implementation searches the input recursively for ``%``-symbols and doesn't attempt to detect nested ``#%`` calls, so nested calls are of limited value."
  (import hyrule [flatten inc])
  (setv expr (.parse-one-form &reader))
  (setv %symbols (sfor a (flatten [expr])
                       :if (and (isinstance a hy.models.Symbol)
                                (.startswith a '%))
                       (-> a
                           (.split "." :maxsplit 1)
                           (get 0)
                           (cut 1 None))))
  `(fn [;; generate all %i symbols up to the maximum found in expr
        ~@(gfor i (range 1 (-> (lfor a %symbols
                                     :if (.isdigit a)
                                     (int a))
                               (or #(0))
                               max
                               inc))
                (hy.models.Symbol (+ "%" (str i))))
        ;; generate the #* parameter only if '%* is present in expr
        ~@(when (in "*" %symbols)
                '(#* %*))
        ;; similarly for #** and %**
        ~@(when (in "**" %symbols)
                '(#** %**))]
     ~expr))
