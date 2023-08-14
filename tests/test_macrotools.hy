(require
  hyrule [defmacro! defmacro/g! with-gensyms ->]
         :readers [/])
(import
  pytest
  hyrule [macroexpand-all])


(defmacro example--with-gensyms []
  (with-gensyms [a]
    `(setv ~a 1)))
(defmacro/g! example--defmacro/g! []
  `(setv ~g!res 1))
(defmacro! example--defmacro! []
  `(setv ~g!res 1))

(defn test-gensym-tools []
  (defn check []
    ; `C` should have two distinct attributes.
    (assert (=
      (len (sfor  a (dir C)  :if (not (.startswith a "__"))  a))
      2)))

  (defclass C [] (example--with-gensyms) (example--with-gensyms))
  (check)
  (defclass C [] (example--defmacro/g!) (example--defmacro/g!))
  (check)
  (defclass C [] (example--defmacro!) (example--defmacro!))
  (check))


(defn test-defmacro/g! []
  ;; defmacro/g! didn't like numbers initially because they
  ;; don't have a startswith method and blew up during expansion
  (defmacro/g! two-point-zero []
    `(+ (float 1) 1.0))
  (assert (= (two-point-zero) 2.0)))


(defn test-defmacro! []

  (defmacro! foo! [o!foo]
    `(do ~g!foo ~g!foo))
  ;; test that o! becomes g!
  (assert (= "Hy" (foo! "Hy")))
  ;; test that o! is evaluated once only
  (setv foo 40)
  (foo! (+= foo 1))
  (assert (= 41 foo))
  ;; test optional args
  (defmacro! bar! [o!a [o!b 1]]
    `(do ~g!a ~g!a ~g!b ~g!b))
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

(defn test-slash-import []
  (defmacro no-name [name]
    `(with [(pytest.raises NameError)] ~name))

  (assert (= (#/ math.sqrt 4) 2))
  (assert (= (.sqrt #/ math 4) 2))
  (no-name math)
  (no-name sqrt)

  (setv math (type "Dummy" #() {"sqrt" "hello"}))
  (assert (= (#/ math.sqrt 4) 2))
  (assert (= math.sqrt "hello"))

  (defmacro frac [a b]
    `(#/ fractions.Fraction ~a ~b))
  (assert (= (* 6 (frac 1 3)) 2))
  (no-name fractions)
  (no-name Fraction)

  (assert (= #/ tests/resources/✈.🚆 "🚗"))

  ;; ensure `tests.resources` is currently in scope
  (import tests.resources)

  ;; delete tests.resources and all submodules
  (for [mod (.copy #/ sys.modules)]
    (when (.startswith mod "tests.resources.")
      (del (get #/ sys.modules mod))))
  (del (get #/ sys.modules "tests.resources"))
  (del tests.resources)

  ;; ensure the module is gone
  (with [(pytest.raises AttributeError)] #/ tests.resources)

  ;; check that we can directly access submodules
  (assert (= (type #/ tests/resources/macros) #/ types.ModuleType)))
