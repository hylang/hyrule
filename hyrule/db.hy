(require
  hyrule.macrotools [def-gensyms])
(import
  contextlib [contextmanager]
  re
  sqlite3
  hyrule.collections [by2s])


(export
  :objects [AttributeRow sqlite-db sqlq]
  :macros [sqlexec])


(defclass AttributeRow [sqlite3.Row]

  #[[A subclass of :class:`sqlite3.Row` that also lets you fetch values of the tuple with attribute names. ::

    (setv db (hy.I.sqlite3.connect ":memory:"))
    (setv db.row-factory AttributeRow)
    (.execute db "create table T(foo text)")
    (.execute db "insert into T values ('hello')")
    (setv [row] (.execute db "select * from T"))
    (print row.foo)  ; => hello

  Real preexisting attributes, like ``keys``, have priority over this syntactic sugar, so you'll still need ``(get row "keys")`` or ``(:keys row)`` to get an element named "keys".]]

  (defn __getattr__ [self name]
    (get self name)))

(defn sqlite-db [
    [database ":memory:"]
    [isolation-level None]
    [row-factory AttributeRow]
    [foreign-keys True]
    #** kwargs]

  #[[Create a connection to a SQLite database, via :func:`sqlite3.connect`, that automatically closes when used as a context manager. ::

       (with [db (sqlite-db "mydatabase.sqlite")]
         (.execute db "create table T(n integer primary key) strict")
         (.execute db "insert into T values (1)"))

     By contrast, ``(with [db (sqlite3.connect …)] …)`` opens and closes a transaction, but leaves the database connection itself open.

     Hyrule's ``sqlite-db`` also has different default behavior from ``sqlite3.connect`` in a few ways, for the sake of convenience. The behavior is set by the following parameters. Any remaining keyword arguments are passed through to ``sqlite3.connect``.

     - ``database``: As ``sqlite3.connect``, but there's now a default argument, which opens an in-memory database.
     - ``isolation-level``: As ``sqlite3.connect``, but the default is ``None``, obtaining the autocommit behavior that's also the default of the ``sqlite3`` command-line program and various other standard interfaces but that Python's ``sqlite3`` library departs from.
     - ``row-factory``: This is assigned to the ``row_factory`` attribute of the connection. The default is :hy:class:`AttributeRow`. Use ``None`` to get the behavior of an unset ``row_factory``.
     - ``foreign-keys``: If true, ``(.execute db "pragma foreign_keys = true")`` is called after creating the connection, enabling `SQLite's foreign-key support <https://sqlite.org/foreignkeys.html>`_.]]

  (_sqlite-db database isolation-level row-factory foreign-keys kwargs))

(defn [contextmanager] _sqlite-db [database isolation-level row-factory foreign-keys kwargs]

  (setv db None)

  (try

    (setv db (hy.I.sqlite3.connect
      :database database
      :isolation-level isolation-level
      #** kwargs))
    (setv db.row-factory row-factory)
    (when foreign-keys
      (.execute db "pragma foreign_keys = true"))
    (yield db)

    (finally
      (when (is-not db None)
        (.close db)))))


(defn sqlq [x]
  "Quote ``x`` as a SQL identifier. ``x`` is stringified and then
  surrounded by double quotes, with any internal double quotes
  duplicated to escape them."
  (+ "\"" (.replace (str x) "\"" "\"\"") "\""))


(defn _values-string [names]
  (.format "({}) values ({})"
    (.join ", " (map sqlq names))
    (.join ", " (* ["?"] (len names)))))


(defn _sql-interpolate [many? fstring [rest None]]
  (setv preamble [])
  (setv params [])
  (setv seen-dotvalues? False)
  (setv sql (cond
    (isinstance fstring hy.models.String)
      fstring
    (isinstance fstring hy.models.FString)
      (hy.models.FString (gfor
        part fstring
        (cond

          (isinstance part hy.models.String)
            part
          ; Otherwise, `part` should be an `FComponent`.

          (and (= (len part) 2) (re.match "r[: ]?" (get part 1))) (do
            (setv format-spec (re.sub "^r *:?" "" (get part 1)))
            (hy.models.FComponent :conversion part.conversion (+
              [(get part 0)]
              (if format-spec [format-spec] []))))

          (is-not part.conversion None)
            (raise (ValueError "conversion specifier not allowed"))

          (= (len part) 1) (do
            (when many?
              (raise (ValueError "`{FORM}` not allowed with `:many`")))
            (.append params (get part 0))
            "?")

          (and (= (len part) 2) (= (get part 1) '"=")) (do
            (when many?
              (raise (ValueError "`{FORM :=}` not allowed with `:many`")))
            (.append params (get part 0))
            (+ (sqlq (get part 0)) " = ?"))

          (and (= (len part) 2) (= (get part 1) '"q"))
            (hy.models.FComponent [`(hy.I.hyrule.sqlq ~(get part 0))])

          (and (= (len part) 2) (= (get part 1) '"values")) (do
            (when (!= (get part 0) '...)
              (raise (ValueError "`:values` requires `...`")))
            (when seen-dotvalues?
              (raise (ValueError "only one `{... :values}` field is allowed")))
            (setv seen-dotvalues? True)
            (if many?
              (do
                (assert (not (or preamble params)))
                (when (!= (len rest) 1)
                  (raise (ValueError "`{... :values}` with `:many` requires exactly one iterable of mappings")))
                (def-gensyms items item keys k)
                (setv preamble [`(setv
                  ~items [#* ~(get rest 0)]
                  ~keys [#* (.keys (get ~items 0))])])
                (setv params `(gfor
                  ~item ~items
                  (lfor  ~k ~keys  (get ~item ~k))))
                (hy.models.FComponent [
                  `(hy.I.hyrule.db._values-string ~keys)]))
              (do
                (setv [keys values] (zip #* (by2s rest)))
                (.extend params values)
                (_values-string (lfor  k keys  k.name)))))

          True
             (raise ValueError))))

    True
      (raise ValueError)))

  #(preamble sql params))

(defmacro sqlexec [arg1 #* rest]

  #[===[``sqlexec`` is a macro for writing and executing SQL statements with f-strings. It uses parametrization and quoting to avoid `SQL injection <https://en.wikipedia.org/wiki/SQL_injection>`_. A simple example is::

      (sqlexec db f"select * from Users where name = {customer} and age > {(* 2 limit)}")
      ; equivalent to
      (.execute db "select * from Users where name = ? and age > ?"
        [customer (* 2 limit)])

  As you can see, each field is replaced with "?", and then the form to be interpolated becomes an argument of ``.execute``.

  Use the format specifier ``=`` for the common case of a SQL equation where the Hy- and SQL-level identifiers coincide::

      (sqlexec db f"select * from Users where {name :=}")
      ; equivalent to
      (.execute db #[[select * from Users where "name" = ?]] [name])

  Notice that the interpolated name is quoted (with :hy:func:`sqlq`), and not mangled. The format specifier ``q`` quotes and interpolates a string, which is useful for parts of a SQL statement not usually allowed to be parametric, such as table names::

      (sqlexec db f"select * from {table :q}")
      ; equivalent to
      (.execute db f"select * from {(sqlq table)}")
      ; which, given `(setv table "Users")`, is equivalent to
      (.execute db #[[select * from "Users"]])

  You can effectively escape a replacement field, so it's interpreted as in a typical f-string, with the format specifier ``r``, for "raw". Another format specifier (prepended with ``:``) can be added after if you wish. Obviously, you aren't protected from SQL injection in this case. ::

      (sqlexec db f"select * from {table :r :20}")
      ; equivalent to
      (.execute db f"select * from {table :20}")

  Finally, ``sqlexec`` allows a special replacement field ``{... :values}`` to easily construct a value expression for SQL ``insert`` statements. It uses keyword arguments to set names and values::

      (sqlexec db f"insert into Users {... :values}"
        :name "Bob"
        :age (* 12 months))
      ; equivalent to
      (sqlexec db #[[insert into Users ("name", "age") values (?, ?)]]
        ["Bob" (* 12 months)])

  If you have an iterable collection of dictionaries that you'd like to use with ``.executemany``—or if you just don't know the column names that you want to use until runtime—add the literal keyword ``:many`` as the first argument to ``sqlexec``. The set of keys of the first dictionary in the iterable is used to set the column names (other keys in further dictionaries are ignored). Thus, the collection of dictionaries must not be empty. ::

      (sqlexec :many db f"insert into Users {... :values}"
        [
          (dict :name "Akbar" :age 35)
          (dict :name "Jeff" :age 35)])
      ; equivalent to:
      (.executemany db #[[insert into Users ("name", "age") values (?, ?)]]
        [
          ["Akbar" 35]
          ["Jeff" 35]])

  No more than one ``{... :values}`` field may appear in a single ``sqlexec``. In ``:many`` mode, ``{FORM}`` and ``{FORM :=}`` fields are not allowed, only ``{FORM :q}``, ``{FORM :r}``, and ``{... :values}``.]===]

  (setv [f db fstring #* rest] (if (= arg1 ':many)
    ['executemany #* rest]
    ['execute arg1 #* rest]))
  (setv [preamble sql params] (_sql-interpolate (= f 'executemany) fstring rest))
  `(do ~@preamble ((. ~db ~f) ~sql ~params)))
