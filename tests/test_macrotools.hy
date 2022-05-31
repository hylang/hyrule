(require
  hyrule [defmacro! defmacro/g! with-gensyms ->])
(import
  hyrule [macroexpand-all])


(defn test-with-gensym []
  (import ast)
  (import hy.compiler [hy-compile])
  (setv macro1 "(defmacro nif [expr pos zero neg]
      (with-gensyms [a]
        `(do
           (setv ~a ~expr)
           (cond (> ~a 0) ~pos
                 (= ~a 0) ~zero
                 (< ~a 0) ~neg))))

    (print (nif (inc -1) 1 0 -1))
    ")
  ;; expand the macro twice, should use a different
  ;; gensym each time
  (setv _ast1 (hy-compile (hy.read-many macro1) __name__))
  (setv _ast2 (hy-compile (hy.read-many macro1) __name__))
  (setv s1 (ast.unparse _ast1))
  (setv s2 (ast.unparse _ast2))
  (assert (in (hy.mangle "_a\uffff") s1))
  (assert (in (hy.mangle "_a\uffff") s2))
  (assert (not (= s1 s2))))


(defn test-defmacro/g! []
  (import ast)
  (import hy.compiler [hy-compile])
  (setv macro1 "(defmacro/g! nif [expr pos zero neg]
        `(do
           (setv ~g!res ~expr)
           (cond (> ~g!res 0) ~pos
                 (= ~g!res 0) ~zero
                 (< ~g!res 0) ~neg)))

    (print (nif (inc -1) 1 0 -1))
    ")
  ;; expand the macro twice, should use a different
  ;; gensym each time
  (setv _ast1 (hy-compile (hy.read-many macro1) __name__))
  (setv _ast2 (hy-compile (hy.read-many macro1) __name__))
  (setv s1 (ast.unparse _ast1))
  (setv s2 (ast.unparse _ast2))
  (assert (in (hy.mangle "_res\uffff") s1))
  (assert (in (hy.mangle "_res\uffff") s2))
  (assert (not (= s1 s2)))

  ;; defmacro/g! didn't like numbers initially because they
  ;; don't have a startswith method and blew up during expansion
  (setv macro2 "(defmacro/g! two-point-zero [] `(+ (float 1) 1.0))")
  (assert (hy-compile (hy.read-many macro2) __name__)))


(defn test-defmacro! []
  ;; defmacro! must do everything defmacro/g! can
  (import ast)
  (import hy.compiler [hy-compile])
  (setv macro1 "(defmacro! nif [expr pos zero neg]
        `(do
           (setv ~g!res ~expr)
           (cond (> ~g!res 0) ~pos
                 (= ~g!res 0) ~zero
                 (< ~g!res 0) ~neg)))

    (print (nif (inc -1) 1 0 -1))
    ")
  ;; expand the macro twice, should use a different
  ;; gensym each time
  (setv _ast1 (hy-compile (hy.read-many macro1) __name__))
  (setv _ast2 (hy-compile (hy.read-many macro1) __name__))
  (setv s1 (ast.unparse _ast1))
  (setv s2 (ast.unparse _ast2))
  (assert (in (hy.mangle "_res\uffff") s1))
  (assert (in (hy.mangle "_res\uffff") s2))
  (assert (not (= s1 s2)))

  ;; defmacro/g! didn't like numbers initially because they
  ;; don't have a startswith method and blew up during expansion
  (setv macro2 "(defmacro! two-point-zero [] `(+ (float 1) 1.0))")
  (assert (hy-compile (hy.read-many macro2) __name__))

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


(defmacro foo-walk []
  42)

(defn test-macroexpand-all []
  ;; make sure a macro from the current module works
  (assert (= (macroexpand-all '(foo-walk))
             '42))
  (assert (= (macroexpand-all '(-> 1 a))
             '(a 1)))
  ;; macros within f-strings should also be expanded
  ;; related to https://github.com/hylang/hy/issues/1843
  (assert (= (macroexpand-all 'f"{(foo-walk)}")
             'f"{42}"))
  (assert (= (macroexpand-all 'f"{(-> 1 a)}")
             'f"{(a 1)}"))

  (defmacro require-macro []
    `(do
       (require tests.resources.macros [test-macro :as my-test-macro])
       (my-test-macro)))

  (assert (= (get (macroexpand-all '(require-macro)) -1)
             '(setv blah 1))))
