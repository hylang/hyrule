(require
  hyrule [sqlexec])
(import
  sqlite3
  pytest
  hyrule [sqlite-db sqlq]
  hyrule.db [_sql-interpolate])


(defn test-sqlite-db [tmp-path]

  (defmacro test [args #* body]
    `(with [db (sqlite-db ~@args)]
      (.execute db "create table A(n integer primary key) strict")
      (.execute db "insert into A values (1)")
      (.execute db "create table B(n integer primary key references A(n)) strict")
      ~@body))

  ; Test the parameters `database` and `isolation-level`.
  (setv p (/ tmp-path "mydb.sqlite3"))
  (defn A-values []
    (with [db (sqlite-db :database p)]
      (lfor  [x] (.execute db "select n from A order by n")  x)))
  (test [:database p]
    (.execute db "insert into A values (2)"))
  (assert (= (A-values) [1 2]))
  (with [db (sqlite-db :database p :isolation-level "DEFERRED")]
    (.execute db "insert into A values (3)")
    (.commit db)
    (.execute db "insert into A values (4)"))
      ; This is not committed, so it's lost.
  (assert (= (A-values) [1 2 3]))

  ; Test that an early error is raised properly.
  (with [e (pytest.raises sqlite3.OperationalError)]
    (test [:database "/invalid_directory_name/invalid_file_name"]))
  (assert (in (get e.value.args 0) [
    "unable to open database file"
    "Could not open database"]))

  ; Test the parameter `row-factory`.
  (test []
    (setv [row] (.execute db "select * from A"))
    (assert (= (list (.keys row)) ["n"]))
    (assert (= (:n row) 1))
    (assert (= row.n 1))
    (with [(pytest.raises IndexError)]
      ; Our row class derives from `sqlite3.Row`, which produces
      ; `IndexError` rather than `KeyError`, oddly enough.
      row.foobar))
  (test [:row-factory None]
    (setv [row] (.execute db "select * from A"))
    (assert (is (type row) tuple)))
  (test [:row-factory sqlite3.Row]
    (setv [row] (.execute db "select * from A"))
    (assert (is (type row) sqlite3.Row)))

  ; Test the parameter `foreign-keys`.
  (test []
    (with [(pytest.raises sqlite3.IntegrityError)]
      (.execute db "insert into B values (5)")))
  (test [:foreign-keys False]
    (.execute db "insert into B values (5)")))


(defn test-sqlq []
  (assert (= (sqlq #[[]]) #[[""]]))
  (assert (= (sqlq #[[hello]]) #[["hello"]]))
  (assert (= (sqlq #[[select]]) #[["select"]]))
  (assert (= (sqlq #[[some "internal" quotes]]) #[["some ""internal"" quotes"]])))


(defn test-sql-interpolate []

  (defn f [#* args]
    (setv [preamble fstring params] (_sql-interpolate False #* args))
    [preamble (.join "" (map str fstring)) params])

    ; Test `{FORM}` and `{FORM :=}`.
  (assert (=
    (f
      'f"update T set {v1 :=}, v2 = {(+ v1 1)} where {v2 :=} and {v4 :=}")
    [
      []
      #[[update T set "v1" = ?, v2 = ? where "v2" = ? and "v4" = ?]]
      ['v1 '(+ v1 1) 'v2 'v4]]))

  ; Test `{... :values}`.
  (assert (=
    (f
      'f"insert into T {... :values}"
      '[:foo 1 :bar 2 :abso💯lutely "great job"])
    [
      []
      #[[insert into T ("foo", "bar", "abso💯lutely") values (?, ?, ?)]]
      ['1 '2 '"great job"]]))
  (with [e (pytest.raises ValueError)]
    (f
      'f"insert into T aaa {... :values} bbb {... :values}"
      '[:foo 1 :bar 2]))
  (assert (= e.value.args #("only one `{... :values}` field is allowed")))

  ; Test `:q`.
  (setv [preamble fstring params] (_sql-interpolate False
    'f"select * from {table :q} where x = {foo}"))
  (assert (= preamble []))
  (assert (=
    (hy.as-model fstring)
    'f"select * from {(hy.I.hyrule.sqlq table)} where x = ?"))
  (assert (= params ['foo]))

  ; Test `:r`.
  (setv [_ fstring _] (_sql-interpolate False
    'f"select * from {table :r} where x = {foo :r :bar}"))
  (assert (=
    (hy.as-model fstring)
    'f"select * from {table} where x = {foo :bar}"))

  (with [db (sqlite-db :row-factory None)]

    ; Use a weird table name and weird column names to test escaping.
    (.execute db (.join " " [#[[create table "My ""Cool"" Table"(]]
      #[["and" integer primary key,]]
      #[["select" integer,]]
      #[["3); drop table Accounts; --" text)]]]))

    ; Test `{... :values}`.
    (sqlexec db
      #[f[insert into "My ""Cool"" Table" {... :values}]f]
      :and 15 :select 16)
    (assert (=
      (list (.execute db #[[select "and", "select" from "My ""Cool"" Table"]]))
      [#(15 16)]))

    ; Test a plain string, in place of an f-string.
    (assert (=
      (list (sqlexec db #[[select "and" from "My ""Cool"" Table"]]))
      [#(15)]))

    ; Test `{FORM}` and `{FORM :=}`.
    (setv select 16)
    (sqlexec db
      #[f[update "My ""Cool"" Table" set "3); drop table Accounts; --" = {"where"} where {select :=}]f])
    (assert (=
      (list (.execute db #[[select * from "My ""Cool"" Table"]]))
      [#(15 16 "where")]))

    ; Test `:q`.
    ; (This feature is useful for loops, since it resolves the quote
    ; at run-time.)
    (setv table #[[My "Cool" Table]])
    (setv column "select")
    (assert (=
      (list (sqlexec db
        f"select * from {table :q} where {column :q} = {select}"))
      [#(15 16 "where")]))

    ; Test `:many`.
    (defn my-generator []
      (yield (dict :and 0 :select 10))
      (yield (dict :and 1 :select 11))
      (yield (dict :and 2 :select 12)))
    (sqlexec :many db
      #[f[insert into "My ""Cool"" Table" {... :values}]f]
      (my-generator))
    (assert (=
      (list (.execute db #[[select * from "My ""Cool"" Table" order by "and"]]))
      [
        #(0 10 None)
        #(1 11 None)
        #(2 12 None)
        #(15 16 "where")]))))
