"This module provides macros similar to Clojure's threading macros,
also known as arrow macros."


(import
  hyrule.iterables [rest]
  itertools [chain])


(eval-and-compile
  (defn _dotted [node]
    "Helper function to turn '.name forms into '(.name) forms"
    (if (and (isinstance node hy.models.Expression)
             (= (get node 0) '.))
      `(~node)
      node)))


(defmacro -> [head #* args]

  #[[Evaluate the first :ref:`expression <hy:expressions>` in ``args``
  with ``head`` inserted as the second argument, then the next
  expression with that result as its second argument, then the next
  expression with *that* result as its second argument, and so on. In
  other words, ::

    (-> x (foo a b) (bar c d))

  is equvalent to ::

    (do
      (setv value x)
      (setv value (foo value a b))
      (setv value (bar value c d))
      value)

  but without actually using an intermediate variable. For example::

    (-> (+ 4 6) (print 5))
      ; Prints "10 5"

  Arguments of the form ``.foo`` are automatically expanded to ``(.foo)``::

    (-> " hello " .upper .strip)  ; => "HELLO"

  And if an argument ``arg`` isn't an expression, it's expanded to
  ``(arg)``::

   (-> "0" int bool)  ; => False]]

  (setv ret head)
  (for [node args
       :setv node (_dotted node)]
    (setv ret (if (isinstance node hy.models.Expression)
                  `(~(get node 0) ~ret ~@(rest node))
                  `(~node ~ret))))
  ret)


(defmacro ->> [head #* args]
  #[[As :hy:func:`->`, but each result is placed as the last argument of
  each expression instead of the second. ::

    (->> "a" (+ "b" "c"))  ; => "bca" ]]
  (setv ret head)
  (for [node args
       :setv node (_dotted node)]
    (setv ret (if (isinstance node hy.models.Expression)
                  `(~@node ~ret)
                  `(~node ~ret))))
  ret)


(defmacro as-> [head name #* rest]

  #[[Assign ``head`` to the symbol ``name`` and evaluate each form in
  ``rest``. After each evaluation, the result is reassigned to
  ``name``. ::

    (as-> "a" it
          (+ "b" it "c")
          (.upper it)
          (print it))
      ; Prints "BAC".

  Thus ``as->`` is analogous to e.g. :hy:func:`->`, but the threading
  point is set per form by name, rather than always being the second
  argument.

  Here's a more complex example, which prints a sentence from a Project
  Gutenberg e-book::

    (import
      re
      urllib.request [urlopen])

    (as-> (urlopen "https://www.gutenberg.org/ebooks/219.txt.utf-8") it
          (.read it)
          (.decode it "UTF-8")
          (re.search r"\. ([^.]+ paw-strokes [^.]+\.)" it)
          (.group it 1)
          (.replace it "\r\n" " ")
          (print it))]]

  `(do (setv
         ~name ~head
         ~@(sum (gfor  x rest  [name x]) []))
     ~name))


(defmacro some-> [head #* args]
  #[[As :hy:func:`->`, but if an intermediate result is ``None``, all
  further forms are ignored. ::

    (defn lookup [char]
      (.get {"a" 1 "b" 2} char))
    (some-> "q" lookup (print "is the value"))
      ; Prints nothing, since `(lookup "q")` returns `None`.]]
  (setv val (hy.gensym))
  `(cond (is (setx ~val ~head) None) None
         ~@(chain.from_iterable (gfor node args
           [`(is (setx ~val (hy.R.hyrule.-> ~val ~node)) None) None]))
         True ~val))


(defmacro doto [form #* expressions]

  "As :hy:func:`->`, but instead of the return value of each expression being
  passed to the next, a single object (obtained by evaluating the original
  first argument ``form``) is used every time. In other words, ::

    (doto x (foo a b) (bar c d))

  is equvalent to ::

    (do
      (setv value x)
      (foo value a b)
      (bar value c d)
      value)

  Thus, ``doto`` is useful when ``value`` is an object that gets mutated by
  each expression::

    (doto [] (.append 1) (.append 2) (.reverse))  ; => [2 1]"

  (setv f (hy.gensym))
  (defn build-form [expression]
    (setv expression (_dotted expression))
    (if (isinstance expression hy.models.Expression)
      `(~(get expression 0) ~f ~@(rest expression))
      `(~expression ~f)))
  `(do
     (setv ~f ~form)
     ~@(map build-form expressions)
     ~f))
