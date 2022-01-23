(require
  hyrule.macrotools [defmacro/g! defmacro!])
(import
  hyrule.anaphoric [recur-sym-replace]
  hyrule.collections [prewalk by2s]
  hyrule.iterables [coll?]
  hyrule.misc [inc])


(defmacro branch [tester #* rest]
  (_branch tester rest))

(defmacro ebranch [tester #* rest]
  (_branch tester (+ rest
    (, 'else '(raise (ValueError "ebranch: No branch matched"))))))

(defn _branch [tester rest]
  (when (% (len rest) 2)
    (raise (TypeError "each case-form needs a result-form")))
  (setv it (hy.gensym "branch-it"))
  `(cond ~@(gfor [case result] (by2s rest) `[
    ~(if (= case 'else)
      'True
      `(do
        (setv ~it ~case)
        ~(recur-sym-replace {'it it} tester)))
    ~result])))


(defmacro case [key #* rest]
  (_case key rest))

(defmacro ecase [key #* rest]
  (_case key (+ rest
    (, 'else '(raise (ValueError "ecase: No test value matched"))))))

(defn _case [key rest]
  ; The implementation is quite similar to `branch`, but we evaluate
  ; the key exactly once.
  (when (% (len rest) 2)
    (raise (TypeError "each test-form needs a result-form")))
  (setv x (hy.gensym "case-key"))
  `(do
    (setv ~x ~key)
    (cond ~@(gfor [test-value result] (by2s rest) `[
      ~(if (= test-value 'else)
        'True
        `(= ~x ~test-value))
      ~result]))))


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
     (if (isinstance ~retval int)
       (sys.exit ~retval))))


(defmacro! ifp [o!pred o!expr #* clauses]
  "Takes a binary predicate ``pred``, an expression ``expr``, and a set of
  clauses. Each clause can be of the form ``cond res`` or ``cond :>> res``. For
  each clause, if ``(pred cond expr)`` evaluates to true, returns ``res`` in
  the first case or ``(res (pred cond expr))`` in the second case. If the last
  clause is just ``res``, then it is returned as a default, else ``None.``

  Examples:
    ::

       => (ifp = 4
       ...   3 :do-something
       ...   5 :do-something-else
       ...   :no-match)
       :no-match

    ::

       => (ifp (fn [x f] (f x)) ':a
       ...   {:b 1} :>> inc
       ...   {:a 1} :>> dec)
       0
  "
  (defn emit [pred expr args]
    (setv n (if (and (> (len args) 1) (= :>> (get args 1))) 3 2)
          [clause more] [(cut args n) (cut args n None)]
          n (len clause)
          test (hy.gensym))
    (cond
      [(= 0 n) `(raise (TypeError (+ "no option for " (repr ~expr))))]
      [(= 1 n) (get clause 0)]
      [(= 2 n) `(if (~pred ~(get clause 0) ~expr)
                    ~(get clause -1)
                    ~(emit pred expr more))]
      [True `(do
               (setv ~test (~pred ~(get clause 0) ~expr))
               (if ~test
                   (~(get clause -1) ~test)
                   ~(emit pred expr more)))]))
  `~(emit g!pred g!expr clauses))


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

    [(and (isinstance x hy.models.Expression) x
        (= (get x 0) 'block))
      (block-f (cut x 1 None) tags BR)]

    [(and (isinstance x hy.models.Expression) x
        (in (get x 0) ['block-ret 'block-ret-from]))
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
          'None)))]

    [(coll? x)
      ((type x) (gfor  elem x  (block-walker elem tags BR)))]

    [True
      x]))

(defclass BlockRet [Exception]
  (setv __init__ (fn [self block-name value]
    (setv self.block-name block-name)
    (setv self.value value))))
