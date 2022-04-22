(require
  hyrule.macrotools [defmacro/g! defmacro!])
(import
  hyrule.collections [prewalk by2s]
  hyrule.iterables [coll?]
  hyrule.misc [inc])


(defmacro branch [tester #* rest]
  #[[Evaluate a test form with the symbol ``it`` bound to each of several case
  forms in turn. If the result is true, the result form associated with the
  matching case is evaluated and returned; no later cases are evaluated. If no
  case matches, return ``None``. The general syntax is::

      (branch TEST
        CASE-1 RESULT-1
        CASE-2 RESULT-2
        …)

  For example,
  ::

      (branch (in (.lower my-char) it)
        "aeiou" "it's a vowel"
        "xyz"   "it's one of those last few")

  is equivalent to
  ::

      (cond
        (in (.lower my-char) "aeiou") "it's a vowel"
        (in (.lower my-char) "xyz")   "it's one of those last few")

  If you miss Common Lisp's ``typecase`` macro, here's how you can use
  ``branch`` to branch by type in the same way::

     (branch (isinstance my-value it)
       str           "It's a string"
       bytes         "It's a bytes object"
       #(int float) "It's numeric")

  A case form that is exactly the symbol ``else`` is treated specially. In this
  case, the test form isn't evaluated, and is treated as if it returned true.
  ::

      (branch (= it my-value)
        True  "Correct"
        False "Wrong"
        else  "Not actually Boolean")

  ``branch`` won't complain if you add more cases after the ``else`` case, even
  additional ``else`` cases, but they'll all be unreachable.

  If there are no case forms, the test form won't be evaluated, so the whole
  ``branch`` is a no-op.

  :hy:func:`ebranch` is a convenience macro for when you want the no-match case
  to raise an error. :hy:func:`case` and :hy:func:`ecase` are convenience
  macros for the special case of checking equality against a single test
  value.]]
  (_branch tester rest))

(defmacro ebranch [tester #* rest]
  #[[As :hy:func:`branch`, but if no case matches, raise ``ValueError`` instead
  of returning ``None``. The name is an abbreviation for "error branch".]]
  (_branch tester (+ rest
    #('else '(raise (ValueError "ebranch: No branch matched"))))))

(defn _branch [tester rest]
  (when (% (len rest) 2)
    (raise (TypeError "each case-form needs a result-form")))
  `(let [it None]
    (cond ~@(sum
      (gfor [case result] (by2s rest) [
        (if (= case 'else)
          'True
          `(do
            (setv it ~case)
            ~tester))
        result])
      []))))


(defmacro case [key #* rest]
  #[[Like the Common Lisp macro of the same name. Evaluate the first argument,
  called the key, and compare it with ``=`` to each of several case forms in
  turn. If the key is equal to a case, the associated result form is evaluated
  and returned; no later cases are evaluated. If no case matches, return
  ``None``. The general syntax is::

      (case KEY
        CASE-1 RESULT-1
        CASE-2 RESULT-2
        …)

  For example, you could translate direction names to vectors like this::

      (case direction
        "north" [ 0  1]
        "south" [ 0 -1]
        "east"  [-1  0]
        "west"  [ 1  0])

  Thus, ``(case KEY …)`` is equivalent to ``(branch (= it KEY) …)``, except
  ``KEY`` is evaluated exactly once, regardless of the number of cases.

  Like :hy:func:`branch`, ``case`` treats the symbol ``else`` as a default
  case, and it has an error-raising version, :hy:func:`ecase`.

  ``case`` can't check for collection membership like the Common Lisp version;
  for that, use ``(branch (in KEY it) …)``. It also can't pattern-match; for
  that, see :hy:func:`match`.]]
  (_case key rest))

(defmacro ecase [key #* rest]
  #[[As :hy:func:`case`, but if no case matches, raise ``ValueError`` instead
  of returning ``None``.]]
  (_case key (+ rest
    #('else '(raise (ValueError "ecase: No test value matched"))))))

(defn _case [key rest]
  ; The implementation is quite similar to `branch`, but we evaluate
  ; the key exactly once.
  (when (% (len rest) 2)
    (raise (TypeError "each test-form needs a result-form")))
  (setv x (hy.gensym "case-key"))
  `(do
    (setv ~x ~key)
    (cond ~@(sum
      (gfor [test-value result] (by2s rest) [
        (if (= test-value 'else)
          'True
          `(= ~x ~test-value))
        result])
      []))))


(defmacro cfor [f #* generator]
  #[[syntactic sugar for passing a ``generator`` expression to the callable ``f``

  Its syntax is the same as :ref:`generator expression <py:genexpr>`, but takes
  a function ``f`` that the generator will be immedietly passed to. Equivalent
  to ``(f (gfor ...))``.

  Examples:

  ::
     => (cfor tuple x (range 10) :if (% x 2) x)
     #(1 3 5 7 9)

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

  This macro is implemented as a :hy:func:`for` loop, so you can use
  :hy:func:`break` and :hy:func:`continue` in the body.

  ::

     => (do-n 3 (print \"hi\"))
     hi
     hi
     hi
  "
  (_do-n count-form body))


(defmacro defmain [args #* body]
  #[[Define a function to be called when :attr:`__name__` equals ``"__main__"``
  (see :mod:`__main__`). ``args`` is the function's lambda list, which will be
  matched against :data:`sys.argv`. Recall that the first element of
  ``sys.argv`` is always the name of the script being invoked, whereas the rest
  are command-line arguments. If ``args`` is ``[]``, this will be treated like
  ``[#* _]``, so any command-line arguments (and the script name) will be
  allowed, but ignored.

  If the defined function returns an :class:`int`, :func:`sys.exit` is called
  with that integer as the return code.

  If you want fancy command-line arguments, you can use the standard Python
  module :mod:`argparse` in the usual way, because ``defmain`` doesn't change
  ``sys.argv``. See also :hy:func:`parse-args <hyrule.misc.parse-args>`.
  ::

      (import argparse)
      (defmain []
        (setv parser (argparse.ArgumentParser))
        (.add-argument parser "STRING"
          :help "string to replicate")
        (.add-argument parser "-n" :type int :default 3
          :help "number of copies")
        (setv args (.parse-args parser))
        (print (* args.STRING args.n))
        0)]]
  (setv retval (hy.gensym)
        restval (hy.gensym))
  `(when (= __name__ "__main__")
     (import sys)
     (setv ~retval ((fn [~@(or args `[#* ~restval])] ~@body) #* sys.argv))
     (when (isinstance ~retval int)
       (sys.exit ~retval))))


(defmacro lif [#* args]
  #[[A "Lispy if" similar to :hy:func:`if` and :hy:func:`cond
  <hy.core.macros.cond>`. Its most notable property is that it tests
  the condition with ``(is-not condition None)`` instead of ``(bool
  condition)``, so values such as the integer 0, the empty string, and
  :py:keyword:`False` are considered true, not false. The general
  syntax is
  ::

      (lif
        condition1 result1
        condition2 result2
        …
        else-value)

  which is equivalent to
  ::

      (cond
        (is-not condition1 None) result1
        (is-not condition2 None) result2
        …
        True                     else-value)

  When no condition matches and there is no else-value, the result is ``None``.]]
  (setv n (len args))
  (when n
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
  #[[Shorthand for ``(when (not test) …)``, i.e., ``(if (not test) (do
  …) None)``. See :hy:func:`if`.
  ::

      (unless ok
        (print "Failed.")
        (exit 1))]]
  `(when (not ~test) ~@body))


(defmacro block [#* body]
  #[[A macro that allows you to jump outside of a list of forms, like
  the Common Lisp special operator of the same name. The body forms
  are executed until ``(block-ret VALUE)`` is reached. The block
  returns ``VALUE``, or the value of the last form, if execution
  reached the end instead of being terminated by ``block-ret``.
  ``VALUE`` is optional and defaults to ``None``. One use of ``block``
  is to jump out of nested loops::

      (block (for [x (range 5)]
        (setv y x)
        (while y
          (print x y)
          (when (and (= x 3) (= y 1))
            (block-ret))
          (-= y 1))))

  Blocks can be named by using a literal keyword or ``None`` as the
  first body form. Then you can use ``(block-ret-from NAME VALUE)`` to
  specify which block to jump out of in a nested sequence of blocks::

     (setv x "")
     (block :a
       (block :b
         (block :c
           (+= x "p")
           (block-ret-from :b)
           (+= x "q"))
         (+= x "r"))
       (+= x "s"))
     (print x)   ; => "ps"

  An anonymous block is treated as being named ``None``, and
  ``(block-ret)`` is actually short for ``(block-ret-from None)``.

  Block names are matched lexically at the time ``block`` is
  macro-expanded. ``(block-ret-from :foo)`` outside of a block named
  ``:foo`` is an error. Inner blocks names shadow outer blocks of the
  same name, so ``block-ret`` will apply to the innermost of a series
  of nested anonymous blocks.

  There are no macros or functions named ``block-ret`` or
  ``block-ret-from``, since these forms are processed entirely by
  ``block``. ``block-ret`` and ``block-ret-from`` should not be
  confused with Hy's built-in ``return``, which produces a true Python
  return statement. ``block`` is implemented with exception-handling
  rather than functions, so it doesn't create a new scope as ``fn``
  and ``defn`` do.]]
  (block-f body {} (hy.gensym "br") True))

(defn block-f [body tags BR [top False]]

  (setv tag 'None)
  (when (and body (or
      (= (get body 0) 'None)
      (isinstance (get body 0) hy.models.Keyword)))
    (setv [tag #* body] body))
  (setv  exc (hy.gensym "e")  block-name (str (hy.gensym "block-")))
  (setv tags {#** tags  tag block-name})

  `(do
    ~(when top `(import hyrule.control [BlockRet :as ~BR]))
    (try
      ~@(block-walker body tags BR)
      (except [~exc ~BR]
        (if (= ~block-name (. ~exc block-name))
          (. ~exc value)
          (raise))))))

(defn block-walker [x tags BR]
  (cond

    (and (isinstance x hy.models.Expression) x
        (= (get x 0) 'block))
      (block-f (cut x 1 None) tags BR)

    (and (isinstance x hy.models.Expression) x
        (in (get x 0) ['block-ret 'block-ret-from])) (do
      (setv block-ret-from? (= (get x 0) 'block-ret-from))
      (if block-ret-from?
        (unless (in (len x) [2 3])
          (raise (TypeError "`block-ret-from` takes two arguments (one optional)")))
        (unless (in (len x) [1 2])
          (raise (TypeError "`block-ret` takes one optional argument"))))
      (setv tag (if block-ret-from? (get x 1) 'None))
      (unless (or (= tag 'None) (isinstance tag hy.models.Keyword))
        (raise (ValueError f"`block-ret-from` target must be a literal keyword or None")))
      (unless (in tag tags)
        (raise (ValueError f"Unmatched block tag: {tag}")))
      `(raise (~BR
        ~(get tags tag)
        ~(if (> (len x) (if block-ret-from? 2 1))
          (get x -1)
          'None))))

    (coll? x)
      ((type x) (gfor  elem x  (block-walker elem tags BR)))

    True
      x))

(defclass BlockRet [Exception]
  (setv __init__ (fn [self block-name value]
    (setv self.block-name block-name)
    (setv self.value value))))
