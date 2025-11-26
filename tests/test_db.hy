(import
  sqlite3
  pytest
  hyrule [sqlite-db])


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
