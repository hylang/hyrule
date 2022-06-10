(require hyrule [defmain])

(import
  sys
  shlex
  subprocess [Popen PIPE]
  pytest)


(defn test-defmain []
  (global __name__)
  (setv oldname __name__)
  (setv oldargs sys.argv)
  (try
    (setv __name__ "__main__")
    (tdefmain-inner)
    (finally (setv
      __name__ oldname
      sys.argv oldargs))))

(defn tdefmain-inner []

  (defn main [x]
    (assert (isinstance x int))
    x)

  (with [e (pytest.raises SystemExit)]
    (defmain [#* args]
      (main 42)))
  (assert (= e.value.code 42))

  ;; Try a `defmain` without args
  (with [e (pytest.raises SystemExit)]
    (defmain []
      (main 42)))
  (assert (= e.value.code 42))

  ;; Try a `defmain` with only one arg
  (setv sys.argv [1])
  (with [e (pytest.raises SystemExit)]
    (defmain [x]
      (main x)))
  (assert (= e.value.code 1)))


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
  (run-cmd "hy tests/resources/bin/main.hy exit47" :expect 47))


(defn test-bin-hy-module-main []
  (assert (in "Hello World" (run-cmd
    "hy -m tests.resources.bin.main"))))


(defn test-bin-hy-module-main-args []
  (assert (in "<test|123|-B>" (run-cmd
    "hy -m tests.resources.bin.main test 123 -B"))))


(defn test-bin-hy-module-main-exitvalue []
  (run-cmd "hy -m tests.resources.bin.main exit47" :expect 47))
