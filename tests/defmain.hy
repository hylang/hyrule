(require hyrule [defmain])


(defn test-defmain []
  (global __name__)
  (setv oldname __name__)
  (setv __name__ "__main__")

  (defn main [x]
    (print (isinstance x int))
    x)

  (try
    (defmain [#* args]
      (main 42))
    (assert False)
    (except [e SystemExit]
      (assert (= (str e) "42"))))

  ;; Try a `defmain` without args
  (try
    (defmain []
      (main 42))
    (assert False)
    (except [e SystemExit]
      (assert (= (str e) "42"))))

  ;; Try a `defmain` with only one arg
  (import sys)
  (setv oldargv sys.argv)
  (try
    (setv sys.argv [1])
    (defmain [x]
      (main x))
    (assert False)
    (except [e SystemExit]
      (assert (= (str e) "1"))))

  (setv sys.argv oldargv)
  (setv __name__ oldname))
