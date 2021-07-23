(defn test-with-gensym []
  (import ast)
  (import hy.compiler [hy-compile])
  (import hy.lex [hy-parse])
  (setv macro1 "(defmacro nif [expr pos zero neg]
      (with-gensyms [a]
        `(do
           (setv ~a ~expr)
           (cond [(> ~a 0) ~pos]
                 [(= ~a 0) ~zero]
                 [(< ~a 0) ~neg]))))

    (print (nif (inc -1) 1 0 -1))
    ")
  ;; expand the macro twice, should use a different
  ;; gensym each time
  (setv _ast1 (hy-compile (hy-parse macro1) __name__))
  (setv _ast2 (hy-compile (hy-parse macro1) __name__))
  (setv s1 (ast.unparse _ast1))
  (setv s2 (ast.unparse _ast2))
  (assert (in (hy.mangle "_a\uffff") s1))
  (assert (in (hy.mangle "_a\uffff") s2))
  (assert (not (= s1 s2))))


(defn test-defmacro/g! []
  (import ast)
  (import hy.compiler [hy-compile])
  (import hy.lex [hy-parse])
  (setv macro1 "(defmacro/g! nif [expr pos zero neg]
        `(do
           (setv ~g!res ~expr)
           (cond [(> ~g!res 0) ~pos]
                 [(= ~g!res 0) ~zero]
                 [(< ~g!res 0) ~neg])))

    (print (nif (inc -1) 1 0 -1))
    ")
  ;; expand the macro twice, should use a different
  ;; gensym each time
  (setv _ast1 (hy-compile (hy-parse macro1) __name__))
  (setv _ast2 (hy-compile (hy-parse macro1) __name__))
  (setv s1 (ast.unparse _ast1))
  (setv s2 (ast.unparse _ast2))
  (assert (in (hy.mangle "_res\uffff") s1))
  (assert (in (hy.mangle "_res\uffff") s2))
  (assert (not (= s1 s2)))

  ;; defmacro/g! didn't like numbers initially because they
  ;; don't have a startswith method and blew up during expansion
  (setv macro2 "(defmacro/g! two-point-zero [] `(+ (float 1) 1.0))")
  (assert (hy-compile (hy-parse macro2) __name__)))


(defn test-defmacro! []
  ;; defmacro! must do everything defmacro/g! can
  (import ast)
  (import hy.compiler [hy-compile])
  (import hy.lex [hy-parse])
  (setv macro1 "(defmacro! nif [expr pos zero neg]
        `(do
           (setv ~g!res ~expr)
           (cond [(> ~g!res 0) ~pos]
                 [(= ~g!res 0) ~zero]
                 [(< ~g!res 0) ~neg])))

    (print (nif (inc -1) 1 0 -1))
    ")
  ;; expand the macro twice, should use a different
  ;; gensym each time
  (setv _ast1 (hy-compile (hy-parse macro1) __name__))
  (setv _ast2 (hy-compile (hy-parse macro1) __name__))
  (setv s1 (ast.unparse _ast1))
  (setv s2 (ast.unparse _ast2))
  (assert (in (hy.mangle "_res\uffff") s1))
  (assert (in (hy.mangle "_res\uffff") s2))
  (assert (not (= s1 s2)))

  ;; defmacro/g! didn't like numbers initially because they
  ;; don't have a startswith method and blew up during expansion
  (setv macro2 "(defmacro! two-point-zero [] `(+ (float 1) 1.0))")
  (assert (hy-compile (hy-parse macro2) __name__))

  (defmacro! foo! [o!foo] `(do ~g!foo ~g!foo))
  ;; test that o! becomes g!
  (assert (= "Hy" (foo! "Hy")))
  ;; test that o! is evaluated once only
  (setv foo 40)
  (foo! (+= foo 1))
  (assert (= 41 foo))
  ;; test optional args
  (defmacro! bar! [o!a [o!b 1]] `(do ~g!a ~g!a ~g!b ~g!b))
  ;; test that o!s are evaluated once only
  (bar! (+= foo 1) (+= foo 1))
  (assert (= 43 foo))
  ;; test that the optional arg works
  (assert (= (bar! 2) 1)))


(defn test-lif []
  ;; None is false
  (assert (= (lif None "true" "false") "false"))

  ;; But everything else is True!  Even falsey things.
  (assert (= (lif True "true" "false") "true"))
  (assert (= (lif False "true" "false") "true"))
  (assert (= (lif 0 "true" "false") "true"))
  (assert (= (lif "some-string" "true" "false") "true"))
  (assert (= (lif "" "true" "false") "true"))
  (assert (= (lif (+ 1 2 3) "true" "false") "true"))
  (assert (= (lif None "true" "false") "false"))
  (assert (= (lif 0 "true" "false") "true"))

  ;; Test ellif [sic]
  (assert (= (lif None 0
                  None 1
                  0 2
                  3)
             2)))


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
