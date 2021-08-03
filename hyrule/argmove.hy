(import
  hyrule.iterables [rest])


(defmacro -> [head #* args]
  "Thread `head` first through the `rest` of the forms.

  ``->`` (or the *threading macro*) is used to avoid nesting of expressions. The
  threading macro inserts each expression into the next expression's first argument
  place. The following code demonstrates this:

  Examples:
    ::

       => (defn output [a b] (print a b))
       => (-> (+ 4 6) (output 5))
       10 5
  "
  (setv ret head)
  (for [node args]
    (setv ret (if (isinstance node hy.models.Expression)
                  `(~(get node 0) ~ret ~@(rest node))
                  `(~node ~ret))))
  ret)


(defmacro ->> [head #* args]
  "Thread `head` last through the `rest` of the forms.

  ``->>`` (or the *threading tail macro*) is similar to the *threading macro*, but
  instead of inserting each expression into the next expression's first argument,
  it appends it as the last argument. The following code demonstrates this:

  Examples:
    ::

       => (defn output [a b] (print a b))
       => (->> (+ 4 6) (output 5))
       5 10
  "
  (setv ret head)
  (for [node args]
    (setv ret (if (isinstance node hy.models.Expression)
                  `(~@node ~ret)
                  `(~node ~ret))))
  ret)


(defmacro as-> [head name #* rest]
  "Beginning with `head`, expand a sequence of assignments `rest` to `name`.

  Each assignment is passed to the subsequent form. Returns the final assignment,
  leaving the name bound to it in the local scope.

  This behaves similarly to other threading macros, but requires specifying
  the threading point per-form via the name, rather than fixing to the first
  or last argument.

  Examples:
    example how ``->`` and ``as->`` relate::

       => (as-> 0 it
       ...      (inc it)
       ...      (inc it))
       2

    ::

       => (-> 0 inc inc)
       2

    create data for our cuttlefish database::

       => (setv data [{:name \"hooded cuttlefish\"
       ...             :classification {:subgenus \"Acanthosepion\"
       ...                              :species \"Sepia prashadi\"}
       ...             :discovered {:year 1936
       ...                          :name \"Ronald Winckworth\"}}
       ...            {:name \"slender cuttlefish\"
       ...             :classification {:subgenus \"Doratosepion\"
       ...                              :species \"Sepia braggi\"}
       ...             :discovered {:year 1907
       ...                          :name \"Sir Joseph Cooke Verco\"}}])

    retrieve name of first entry::

       => (as-> (get data 0) it
       ...      (:name it))
       \"hooded cuttlefish\"

    retrieve species of first entry::

       => (as-> (get data 0) it
       ...      (:classification it)
       ...      (:species it))
       \"Sepia prashadi\"

    find out who discovered slender cuttlefish::

       => (as-> (filter (fn [entry] (= (:name entry)
       ...                           \"slender cuttlefish\")) data) it
       ...      (get it 0)
       ...      (:discovered it)
       ...      (:name it))
       \"Sir Joseph Cooke Verco\"

    more convoluted example to load web page and retrieve data from it::

       => (import urllib.request [urlopen])
       => (as-> (urlopen \"http://docs.hylang.org/en/stable/\") it
       ...      (.read it)
       ...      (.decode it \"utf-8\")
       ...      (lfor  x it  :if (!= it \"Welcome\")  it)
       ...      (cut it 30)
       ...      (.join \"\" it))
       \"Welcome to Hy’s documentation!\"

  .. note::

    In these examples, the REPL will report a tuple (e.g. `('Sepia prashadi',
    'Sepia prashadi')`) as the result, but only a single value is actually
    returned.
  "
  `(do (setv
         ~name ~head
         ~@(sum (gfor  x rest  [name x]) []))
     ~name))


(defmacro doto [form #* expressions]
  "Perform possibly mutating `expressions` on `form`, returning resulting obj.

  ``doto`` is used to simplify a sequence of method calls to an object.

  Examples:
    ::

       => (doto [] (.append 1) (.append 2) .reverse)
       [2 1]

    ::

       => (setv collection [])
       => (.append collection 1)
       => (.append collection 2)
       => (.reverse collection)
       => collection
       [2 1]
  "
  (setv f (hy.gensym))
  (defn build-form [expression]
    (if (isinstance expression hy.models.Expression)
      `(~(get expression 0) ~f ~@(rest expression))
      `(~expression ~f)))
  `(do
     (setv ~f ~form)
     ~@(map build-form expressions)
     ~f))
