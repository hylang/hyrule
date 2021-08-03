(require
  hyrule.macrotools [defmacro/g!])
(import
  hyrule.collections [prewalk]
  hyrule.misc [inc])


(defmacro cfor [f #* generator]
  #[[syntactic sugar for passing a ``generator`` expression to the callable ``f``

  Its syntax is the same as :ref:`generator expression <py:genexpr>`, but takes
  a function ``f`` that the generator will be immedietly passed to. Equivalent
  to ``(f (gfor ...))``.

  Examples:
  ::
     => (cfor tuple x (range 10) :if (% x 2) x)
     (, 1 3 5 7 9)

  The equivalent in python would be:

     >>> tuple(x for x in range(10) if is_odd(x))

  Some other common functions that take iterables::

     => (cfor all x [1 3 8 5] (< x 10))
     True

     => (with [f (open "AUTHORS")]
     ...  (cfor max
     ...        author (.splitlines (f.read))
     ...        :setv name (.group (re.match r"\* (.*?) <" author) 1)
     ...        :if (name.startswith "A")
     ...        (len name)))
     20 ;; The number of characters in the longest author's name that starts with 'A'
  ]]
  `(~f (gfor ~@generator)))


(defn _do-n [count-form body]
  `(for [~(hy.gensym) (range ~count-form)]
    ~@body))


(defmacro do-n [count-form #* body]
  "Execute `body` a number of times equal to `count-form` and return
  ``None``. (To collect return values, use :hy:macro:`list-n`
  instead.) Negative values of the count are treated as 0.

  This macro is implemented as a :hy:macro:`for` loop, so you can use
  :hy:macro:`break` and :hy:macro:`continue` in the body.

  ::

     => (do-n 3 (print \"hi\"))
     hi
     hi
     hi
  "
  (_do-n count-form body))


(defmacro defmain [args #* body]
  "Write a function named \"main\" and do the 'if __main__' dance.

  The ``defmain`` macro defines a main function that is immediately called
  with ``sys.argv`` as arguments if and only if this file is being executed
  as a script.  In other words, this:

  Examples:
    ::

       => (defmain [#* args]
       ...  (do-something-with args))

    is the equivalent of:

    .. code-block:: python

       => def main(*args):
       ...    do_something_with(args)
       ...    return 0
       ...
       ... if __name__ == \"__main__\":
       ...     import sys
       ...     retval = main(*sys.argv)
       ...
       ...     if isinstance(retval, int):
       ...         sys.exit(retval)

    Note that as you can see above, if you return an integer from this
    function, this will be used as the exit status for your script.
    (Python defaults to exit status 0 otherwise, which means everything's
    okay!) Since ``(sys.exit 0)`` is not run explicitly in the case of a
    non-integer return from ``defmain``, it's a good idea to put ``(defmain)``
    as the last piece of code in your file.

    If you want fancy command-line arguments, you can use the standard Python
    module ``argparse`` in the usual way::

       => (import argparse)
       => (defmain [#* _]
       ...   (setv parser (argparse.ArgumentParser))
       ...   (.add-argument parser \"STRING\"
       ...     :help \"string to replicate\")
       ...   (.add-argument parser \"-n\" :type int :default 3
       ...     :help \"number of copies\")
       ...   (setv args (parser.parse_args))
       ...   (print (* args.STRING args.n))
       ...   0)
"
  (setv retval (hy.gensym)
        restval (hy.gensym))
  `(when (= __name__ "__main__")
     (import sys)
     (setv ~retval ((fn [~@(or args `[#* ~restval])] ~@body) #* sys.argv))
     (if (isinstance ~retval int)
       (sys.exit ~retval))))


(defmacro lif [#* args]
  "Like `if`, but anything that is not None is considered true.

  For those that prefer a more Lispy ``if`` clause, we have
  ``lif``. This *only* considers ``None`` to be false! All other
  \"false-ish\" Python values are considered true.

  Examples:
    ::

       => (lif True \"true\" \"false\")
       \"true\"

    ::

       => (lif False \"true\" \"false\")
       \"true\"

    ::

       => (lif 0 \"true\" \"false\")
       \"true\"

    ::

       => (lif None \"true\" \"false\")
       \"false\"
  "
  (setv n (len args))
  (if n
      (if (= n 1)
          (get args 0)
          `(if (is-not ~(get args 0) None)
               ~(get args 1)
               (lif ~@(cut args 2 None))))))


(defmacro list-n [count-form #* body]
  "Like :hy:macro:`do-n`, but the results are collected into a list.

  ::

    => (setv counter 0)
    => (list-n 5 (+= counter 1) counter)
    [1 2 3 4 5]
  "
  (setv l (hy.gensym))
  `(do
    (setv ~l [])
    ~(_do-n count-form [`(.append ~l (do ~@body))])
    ~l))


(defmacro/g! loop [bindings #* body]
  "The loop/recur macro allows you to construct functions that use
  tail-call optimization to allow arbitrary levels of recursion.

  ``loop`` establishes a recursion point. With ``loop``, ``recur``
  rebinds the variables set in the recursion point and sends code
  execution back to that recursion point. If ``recur`` is used in a
  non-tail position, an exception is raised. which
  causes chaos.

  Usage: ``(loop bindings #* body)``

  Examples:
    ::

       => (require hyrule.contrib.loop [loop])
       => (defn factorial [n]
       ...  (loop [[i n] [acc 1]]
       ...    (if (= i 0)
       ...      acc
       ...      (recur (dec i) (* acc i)))))
       => (factorial 1000)"
  (setv [fnargs initargs] (if bindings (zip #* bindings) [[] []]))
  (setv new-body (prewalk
    (fn [x] (if (= x 'recur) g!recur-fn x))
    body))
  `(do
    (import hyrule.control [_trampoline :as ~g!t])
    (setv ~g!recur-fn (~g!t (fn [~@fnargs] ~@new-body)))
    (~g!recur-fn ~@initargs)))

(defn _trampoline [f]
  "Wrap f function and make it tail-call optimized."
  ;; Takes the function "f" and returns a wrapper that may be used for tail-
  ;; recursive algorithms. Note that the returned function is not side-effect
  ;; free and should not be called from anywhere else during tail recursion.

  (setv result None)
  (setv active False)
  (setv accumulated [])

  (fn [#* args]
    (nonlocal active)
    (.append accumulated args)
    (when (not active)
      (setv active True)
      (while (> (len accumulated) 0)
        (setv result (f #* (.pop accumulated))))
      (setv active False)
      result)))


(defmacro unless [test #* body]
  "Execute `body` when `test` is false

  The ``unless`` macro is a shorthand for writing an ``if`` statement that checks if
  the given conditional is ``False``. The following shows the expansion of this macro.

  Examples:
    ::

       => (unless conditional statement)
       (if conditional
         None
         (do statement))"
  `(if (not ~test) (do ~@body)))
