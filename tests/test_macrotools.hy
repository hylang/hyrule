(require
  hyrule [defmacro-kwargs defmacro! with-gensyms ->]
         :readers [/])
(import
  pytest
  hyrule [macroexpand-all map-hyseq map-model match-fn-params])


(defn test-defmacro-kwargs []

   (defmacro-kwargs m [a b [c "default-c"] #* rest #** kw]
     "m docstring"
     [a b c rest kw])
   (assert (=
     (m 1 2)
     [1 2 "default-c" #() {}]))
   (assert (=
     (m :b "bb" :a "aa" :foo "hello")
     ["aa" "bb" "default-c" #() {"foo" "hello"}]))
   (assert (= (. (get-macro m) __doc__) "m docstring"))

   ; Make sure we avoid spurious extra quoting.
   (defmacro-kwargs m2 [[x 15]]
     ; `x` should be an `int` here, not `hy.models.Integer`.
     (global x-is-plain-int?)
     (setv x-is-plain-int? (is (type x) int))
     x)
   (assert (= (m2) 15))
   (assert (do-mac x-is-plain-int?)))


(defmacro example--with-gensyms []
  (with-gensyms [a]
    `(setv ~a 1)))
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
  (defclass C [] (example--defmacro!) (example--defmacro!))
  (check))


(defn test-defmacro!-numbers []
  ;; defmacro! didn't like numbers initially because they
  ;; don't have a startswith method and blew up during expansion
  (defmacro! two-point-zero []
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


(defn test-defmacro!-returns []
  ;; https://github.com/hylang/hyrule/issues/76
  (defmacro! m [o!n]
    (return `(+ ~g!n ~g!n ~g!n)))
  (setv x 1)
  (defn f [] (nonlocal x) (+= x 1) x)
  (assert (= (m (f)) 6)))


(defn test-defmacro!-modify-args []
  ;; https://github.com/hylang/hyrule/issues/112
  ;; test that modifying args compiles
  (defmacro! foo [x]
    (+= x 1)
    x)
  (assert (= (foo 1) 2))
  ;; test optional/default args
  (defmacro! bar [x [y 0]]
    (+= y 1)
    y)
  (assert (= (bar 2) 1))
  (assert (= (bar 2 3) 4))
  ;; test unpack-iterable args
  (defmacro! baz [o!x #* ys]
    (+= ys #(3))
    `(+ ~g!x ~g!x ~(. ys [-1])))
  (setv z 9)
  (assert (= (baz (do (+= z 1) z) bing bang bong)) 23))


(defmacro foo-walk []
  42)

(defmacro require-macro []
    `(do
       (require tests.resources.macros [test-macro :as my-test-macro])
       (my-test-macro)))

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

  (assert (= (get (macroexpand-all '(require-macro)) -1)
             '(setv blah 1)))

  ; Avoid crashing on stub macros like `else`.
  (assert (= (macroexpand-all '(for [c "ab"] (else (foo-walk))))
             '(for [c "ab"] (else 42))))

  ; Check quoting.
  (assert (= (macroexpand-all ''(foo-walk))
             ''(foo-walk)))
  (assert (= (macroexpand-all `(do (foo-walk)))
             '(do 42)))
  (assert (= (macroexpand-all '`(do (foo-walk)))
             '`(do (foo-walk))))
  (assert (= (macroexpand-all '`(do ~(foo-walk)))
             '`(do ~42)))
  (assert (= (macroexpand-all '`(do (unquote_splice (foo-walk))))
             '`(do (unquote_splice 42)))))


(defn test-map-hyseq []

  ; If `x` isn't sequential (or not a model at all), `f` isn't called.
  (assert (=
    (map-hyseq
      3
      (fn [x] (raise ValueError)))
    3))
  (assert (=
    (map-hyseq
      [1 2]
      (fn [x] (raise ValueError)))
    [1 2]))

  ; `f` can be called when `x` is empty.
  (assert (=
    (map-hyseq
      '[]
      (fn [x] ['4]))
    '[4]))

  ; `f` gets the sequence contents as a tuple.
  (setv saw None)
  (assert (=
    (map-hyseq
      '{1 2  3 4}
      (fn [x]
        (nonlocal saw)
        (setv saw x)
        (gfor  e x  (hy.models.Integer (+ e 1)))))
    '{2 3  4 5}))
  (assert (= saw #('1 '2 '3 '4))))
  ; Preservation of sequence attributes is tested as part of testing
  ; `map-model`.


(defn test-map-model []

  ; When the callback returns `None`, the element is recursed into, or
  ; left alone if non-sequential.
  (assert (=
    (map-model
      '[foo "bar" 3 ["bing" baz]]
      (fn [x]
        (when (isinstance x hy.models.Symbol)
          (hy.models.Symbol (.upper (str x))))))
    '[FOO "bar" 3 ["bing" BAZ]]))

  ; `hy.as-model` is called on the input, as well as the callback's
  ; output.
  (assert (=
    (map-model
      ["hello"]
      (fn [x]
        (cond
          (= x "hello")  "wrong"
          (= x '"hello") "right")))
    '["right"]))
  ; Even if the outermost layer of the input is already a model.
  (setv seen [])
  (map-model
    (hy.models.List ["hello"])
    (fn [x]
      (.append seen (= x '["hello"]))
      1))
  (assert (= seen [True]))

  ; String and byte models aren't recursed into. (They're iterable,
  ; but not sequential models.)
  (assert (=
    (map-model
      '["a" "apple" #("a")]
      (fn [x]
        (when (= (str x) "a")
          "b")))
    '["b" "apple" #("b")]))

  ; We can recurse into f-strings, and their properties (like brackets
  ; and conversion specifiers) are preserved.
  (setv x (map-model
    '(+ #[f-x[a{1 !r :9}b{2 !r :9}c]f-x] "hello")
    (fn [x]
      (when (= x '2)
        '3))))
  (assert (= x '(+ #[f-x[a{1 !r :9}b{3 !r :9}c]f-x] "hello")))
  (assert (= (. x [1] brackets) "f-x"))
  (assert (= (. x [1] [1] conversion) "r"))

  ; Try a macro implemented with `map-model`.
  (defmacro lowercase-syms [#* body]
    (hy.I.hyrule.map-model `(do ~@body) (fn [x]
      (when (isinstance x hy.models.Symbol)
        (hy.models.Symbol (.lower (str x)))))))
  (lowercase-syms
    (SETV FOO 15)
    (+= FOO (ABS -5)))
  (assert (= foo 20)))


(defn test-match-fn-params []

  (defn f [args]
    (match-fn-params args '[a b [c "default-c"] #* rest #** kw]))
  (assert (=
    (f [1 2])
    (dict :a 1 :b 2 :c "default-c" :rest #() :kw {})))
  (assert (=
    (f '[1 2])
    (dict :a '1 :b '2 :c "default-c" :rest #() :kw {})))
  (assert (=
    (f '[1 2 3 4 (+ 4 1)])
    (dict :a '1 :b '2 :c '3 :rest #('4 '(+ 4 1)) :kw {})))
  (assert (=
    (f '[:a 1 :b 2 :c 3 :extra 4])
    (dict :a '1 :b '2 :c '3 :rest #() :kw {"extra" '4})))
  (assert (=
    (f '[:b 2 1])
    (dict :a '1 :b '2 :c "default-c" :rest #() :kw {})))
  (assert (=
    (f '[:b 2 :extra "foo" :a 1])
    (dict :a '1 :b '2 :c "default-c" :rest #() :kw {"extra" '"foo"})))
  (assert (=
    (f '[1 2 3 4 5 6 7 :x 10 :y 11])
    (dict :a '1 :b '2 :c '3 :rest #('4 '5 '6 '7) :kw {"x" '10 "y" '11})))

  ; Mangling
  (assert (=
    (match-fn-params
      '[1 :⬢ ☤ :⚘ 3 :☘ 4]
      '[a-b ⬢ #** ✈])
    (dict
      :a_b '1
      :hyx_Xblack_hexagonX '☤
      :hyx_XairplaneX {"hyx_XflowerX" '3 "hyx_XshamrockX" '4})))

  ; Unpacking
  (with [(pytest.raises TypeError :match "^unpacking is not allowed in `args`$")]
    (f '[1 2 3 #* [1 2]]))
  (with [(pytest.raises TypeError :match "^unpacking is not allowed in `args`$")]
    (f '[1 2 3 #** {"qq" 1 "xx" 2}]))

  ; A syntactically invalid parameter list
  (with [(pytest.raises hy.I.funcparserlib/parser.NoParseError)]
     (match-fn-params [1] '[a 3]))
  (assert (= (match-fn-params [1] '[a]) (dict :a 1))))


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
