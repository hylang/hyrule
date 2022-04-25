(require
  hyrule.macrotools [defmacro/g!])

(import
  hy.scoping [ScopeLet]
  hyrule.collections [by2s])


(defmacro comment [#* body]
  "Ignores body and always expands to None

  The ``comment`` macro ignores its body and always expands to ``None``.
  Unlike linewise comments, the body of the ``comment`` macro must
  be grammatically valid Hy, so the compiler can tell where the comment ends.
  Besides the semicolon linewise comments,
  Hy also has the ``#_`` discard prefix syntax to discard the next form.
  This is completely discarded and doesn't expand to anything, not even ``None``.

  Examples:
    ::

        => (print (comment <h1>Surprise!</h1>
        ...                <p>You'd be surprised what's grammatically valid in Hy.</p>
        ...                <p>(Keep delimiters in balance, and you're mostly good to go.)</p>)
        ...        \"Hy\")
        None Hy

    ::

        => (print #_(comment <h1>Surprise!</h1>
        ...                  <p>You'd be surprised what's grammatically valid in Hy.</p>
        ...                  <p>(Keep delimiters in balance, and you're mostly good to go.)</p>))
        ...        \"Hy\")
        Hy"
  None)


(defn constantly [value]
  "Create a new function that always returns `value` regardless of its input.

  Create a new function that always returns the given value, regardless of
  the arguments given to it.

  Examples:
    ::

        => (setv answer (constantly 42))
        => (answer)
        42

    ::

        => (answer 1 2 3)
        42

    ::

        => (answer 1 :foo 2)
        42
  "
  (fn [#* args #** kwargs]
    value))


(defn dec [n]
  "Decrement `n` by 1.

  Returns one less than *x*. Equivalent to ``(- x 1)``. Raises ``TypeError``
  if *x* is not numeric.

  Examples:
    ::

        => (dec 3)
        2

    ::

        => (dec 0)
        -1

    ::

        => (dec 12.3)
        11.3
  "
  (- n 1))


(defn inc [n]
  "Increment `n` by 1.

  Returns one more than *x*. Equivalent to ``(+ x 1)``. Raises ``TypeError``
  if *x* is not numeric.

  Examples:
    ::

       => (inc 3)
       4

    ::

       => (inc 0)
       1

    ::

       => (inc 12.3)
       13.3
  "
  (+ n 1))


(defmacro of [base #* args]
  "Shorthand for indexing for type annotations.

  If only one arguments are given, this expands to just that argument. If two arguments are
  given, it expands to indexing the first argument via the second. Otherwise, the first argument
  is indexed using a tuple of the rest.

  ``of`` has three forms:

  - ``(of T)`` will simply become ``T``.
  - ``(of T x)`` will become ``(get T x)``.
  - ``(of T x y ...)`` (where the ``...`` represents zero or more arguments) will become
    ``(get T #(x y ...))``.

  Examples:
    ::

       => (of str)
       str

    ::

       => (of List int)
       List[int]

    ::

       => (of Set int)
       Set[int]

    ::

       => (of Dict str str)
       Dict[str, str]

    ::

       => (of Tuple str int)
       Tuple[str, int]

    ::

       => (of Callable [int str] str)
       Callable[[int, str], str]
  "
  (if
    (not args) base
    (if (= (len args) 1)
        `(get ~base ~@args)
        `(get ~base #(~@args)))))


(defn parse-args [spec [args None] #** parser-args]
  "Return arguments namespace parsed from *args* or ``sys.argv`` with
  :py:meth:`argparse.ArgumentParser.parse_args` according to *spec*.

  *spec* should be a list of arguments which will be passed to repeated
  calls to :py:meth:`argparse.ArgumentParser.add_argument`.  *parser-args*
  may be a list of keyword arguments to pass to the
  :py:class:`argparse.ArgumentParser` constructor.

  Examples:
    ::

       => (parse-args [[\"strings\" :nargs \"+\" :help \"Strings\"]
       ...             [\"-n\" \"--numbers\" :action \"append\" :type int :help \"Numbers\"]]
       ...            [\"a\" \"b\" \"-n\" \"1\" \"-n\" \"2\"]
       ...            :description \"Parse strings and numbers from args\")
       Namespace(numbers=[1, 2], strings=['a', 'b'])
  "
  (import argparse)
  (setv parser (argparse.ArgumentParser #** parser-args))
  (for [arg spec]
    (setv positional-arguments []
          keyword-arguments []
          value-of-keyword? False)
    (for [item arg]
      (if value-of-keyword?
          (.append (get keyword-arguments -1) item)
          (if (isinstance item hy.models.Keyword)
              (.append keyword-arguments [item.name])
              (.append positional-arguments item)))
      (setv value-of-keyword? (and
        (not value-of-keyword?)
        (isinstance item hy.models.Keyword))))
    (parser.add-argument #* positional-arguments #** (dict keyword-arguments)))
  (.parse-args parser args))


(defmacro profile/calls [#* body]
  "``profile/calls`` allows you to create a call graph visualization.
  **Note:** You must have `Graphviz <http://www.graphviz.org/>`_
  installed for this to work.

  Examples:
    ::

       => (require hyrule.contrib.profile [profile/calls])
       => (profile/calls (print \"hey there\"))
  "
  `(do
     (import pycallgraph [PyCallGraph]
             pycallgraph.output [GraphvizOutput])
     (with [(PyCallGraph :output (GraphvizOutput))]
           ~@body)))


(defmacro/g! profile/cpu [#* body]
  "Profile a bit of code

  Examples:
    ::

       => (require hyrule.contrib.profile [profile/cpu])
       => (profile/cpu (print \"hey there\"))

    .. code-block:: bash

      hey there
      <pstats.Stats instance at 0x14ff320>
                2 function calls in 0.000 seconds

        Random listing order was used

        ncalls  tottime  percall  cumtime  percall filename:lineno(function)        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
            1    0.000    0.000    0.000    0.000 {print}

  "
  `(do
     (import cProfile pstats)

     (import io [StringIO])

     (setv ~g!hy-pr (.Profile cProfile))
     (.enable ~g!hy-pr)
     (do ~@body)
     (.disable ~g!hy-pr)
     (setv ~g!hy-s (StringIO))
     (setv ~g!hy-ps
           (.sort-stats (pstats.Stats ~g!hy-pr :stream ~g!hy-s)))
     (.print-stats ~g!hy-ps)
     (print (.getvalue ~g!hy-s))))


(defn xor [a b]
  "Perform exclusive or between `a` and `b`.

  ``xor`` performs the logical operation of exclusive OR. It takes two arguments.
  If exactly one argument is true, that argument is returned. If neither is true,
  the second argument is returned (which will necessarily be false). Otherwise,
  when both arguments are true, the value ``False`` is returned.

  Examples:
    ::

       => [(xor 0 0) (xor 0 1) (xor 1 0) (xor 1 1)]
       [0 1 1 False]
  "
  (if (and a b)
    False
    (or a b)))

(defmacro smacrolet [bindings #* body]
  "symbol macro let.

  Replaces symbols in body, but only where it would be a valid let binding. The
  bindings pairs the target symbol and the expansion form for that symbol

  Examples:

    ::

       (smacrolet [b c]
         (defn foo [a [b 1]]
           (* b (+ a 1)))
         (* b (foo 7)))

    Would compile to::

       (defn foo [a [b 1]]
         (* b (+ a 1)))
       (* c (foo 7))

    Notice that the ``b`` symbol defined by the ``defn`` remains unchanged as it
    is not a valid ``let`` binding. Only the top level ``b`` sym has been
    replaced with ``c``
  "
  (when (% (len bindings) 2)
    (raise (ValueError "bindings must be paired")))

  (setv scope (.scope.create &compiler ScopeLet))
  (for [[target value] (by2s bindings)]
    (when (not (isinstance value hy.models.Symbol))
      (raise (ValueError "Bind target value must be a Symbol")))
    (when (not (isinstance target hy.models.Symbol))
      (raise (ValueError "Bind target must be a Symbol")))
    (when (in '. target)
      (raise (ValueError "Bind target must not contain a dot")))
    (.add scope target (str value)))
  (with [scope]
    (.compile &compiler `(do ~@body))))
