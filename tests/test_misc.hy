(require
  hyrule [comment of pun smacrolet])
(import
  sys
  pytest
  typing [List Dict]
  hyrule [constantly dec inc import-path parse-args sign xor])


(defn test-constantly []
  (setv helper (constantly 42))

  (assert (= (helper) 42))
  (assert (= (helper 1 2 3) 42))
  (assert (= (helper 1 2 :foo 3) 42)))


(defn test-comment []
  (assert (is None (comment <h1>This is merely a comment</h1>
                            <p> Move along (nothing to see here)</p>))))


(defn test-dec []
  (assert (= 0 (dec 1)))
  (assert (= -1 (dec 0)))
  (assert (= 0 (dec (dec 2)))))


(defn test-inc []
  (assert (= 3 (inc 2)))
  (assert (= 0 (inc -1)))

  (defclass X [object]
    (defn __add__ [self other] (.format "__add__ got {}" other)))
  (assert (= (inc (X)) "__add__ got 1")))


(defn test-import-path [tmp-path]
  (setv mp (/ tmp-path "a.hy"))
  (.write-text mp "(setv foo 7)")

  (setv m (import-path mp "mymod"))
  (assert (= m.foo 7))
  (assert (= m.__name__ "mymod"))
  (assert (is (get sys.modules "mymod") m))

  (setv m2 (import-path mp))
  (assert (= m2.foo 7))
  (assert (is-not m2 m))
  (assert (in m2 (.values sys.modules)))

  (.write-text (/ tmp-path "b.py") "bar = 3")
  (assert (= (. (import-path (/ tmp-path "b.py")) bar) 3)))


(defn test-of []
  (assert (= (of str) str))
  (assert (= (of List int) (get List int)))
  (assert (= (of Dict str str) (get Dict #(str str)))))


(defn test-parse-args []
  ; https://github.com/hylang/hy/issues/1875
  (setv parsed-args (parse-args [["strings" :nargs "+" :help "Strings"]
                                 ["-n" :action "append" :type int :help "Numbers" "--numbers"]]
                                ["a" "b" "-n" "1" "--numbers" "2"]
                                :description "Parse strings and numbers from args"))
  (assert (= parsed-args.strings ["a" "b"]))
  (assert (= parsed-args.numbers [1 2])))


(defn test-xor []

  ; Test each cell of the truth table.
  (assert (= (xor False  False) False))
  (assert (= (xor False True)  True))
  (assert (= (xor True  False) True))
  (assert (= (xor True  True)  False))

  ; Same thing, but with numbers.
  (assert (= (xor 0 0) 0))
  (assert (= (xor 0 1) 1))
  (assert (= (xor 1 0) 1))
  (assert (= (xor 1 1) False))

  ; Of two distinct false values, the second is returned.
  (assert (= (xor False 0) 0))
  (assert (= (xor 0 False) False)))


(defn test-pun [] (pun
  (setv   adam 1  bob 2  chris 3  !bob 100)
  (assert (=
    [:!adam :!bob :!chris]
    [:adam 1 :bob 2 :chris 3]))
  (assert (=
    (dict :!adam :!bob :!chris)
    {"adam" 1  "bob" 2  "chris" 3}))
  (assert (=
    (dict :!adam :bob 4 :!chris)
    {"adam" 1  "bob" 4  "chris" 3}))
  (assert (=
    (dict :!adam :!!bob :!chris)
    {"adam" 1  (hy.mangle "!bob") 100  "chris" 3}))))


(defn test-sign []
  (assert (= (sign -9) -1))
  (assert (= (sign -0.1) -1))
  (assert (= (sign 0) 0))
  (assert (= (sign (hy.I.fractions.Fraction 2 3) 1)))
  (assert (= (sign (hy.I.decimal.Decimal 7.1) 1)))
  (with [(pytest.raises TypeError)]
    (sign "3"))
  (with [(pytest.raises TypeError)]
    (sign 3j)))


(defn test-smacrolet []
  (with [exc (pytest.raises UnboundLocalError)]
    (smacrolet [b c]
      b))
  (assert (or (in "cannot access local variable 'c' where it is not associated with a value" (str exc))
              (in "local variable 'c' referenced before assignment" (str exc))))
  (assert (not-in "b" (locals)))

  (setv c 42)
  (assert (= 42 (smacrolet [b c] b)))

  (smacrolet [b c]
    (defn afunc [a [b 1]] (+ a b)))
  (assert (= 2 (afunc 1)))

  (smacrolet [foo bar]
    (setv foo (fn [x] x)))
  (assert (= 1 (bar 1))))
