(require hyrule [defmain])

(import
  shlex
  os
  subprocess [Popen PIPE])


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


(defn run-cmd [cmd [stdin-data None] [expect 0]]
    (setv p (Popen
      (shlex.split cmd)
      :stdin (and stdin-data PIPE)
      :stdout PIPE
      :universal_newlines True))
    (setv [output _] (.communicate p stdin_data))
    (assert (= (.wait p) expect))
    output)


(defn test-bin-hy-main []
  (assert (in "Hello World" (run-cmd
    "hy tests/resources/bin/main.hy"))))


(defn test-bin-hy-main-args []
  (assert (in "<test|123|-m|-B|9>" (run-cmd
    "hy tests/resources/bin/main.hy test 123 -m -B 9"))))


(defn test-bin-hy-main-exitvalue []
  (run-cmd "hy tests/resources/bin/main.hy exit1" :expect 1))


(defn test-bin-hy-module-main []
  (assert (in "Hello World" (run-cmd
    "hy -m tests.resources.bin.main"))))


(defn test-bin-hy-module-main-args []
  (assert (in "<test|123|-B>" (run-cmd
    "hy -m tests.resources.bin.main test 123 -B"))))


(defn test-bin-hy-module-main-exitvalue []
  (run-cmd "hy -m tests.resources.bin.main exit1" :expect 1))
