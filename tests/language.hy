(defn test-rest []
  (assert (= (list (rest [1 2 3 4 5])) [2 3 4 5]))
  (assert (= (list (islice (rest (count 8)) 3)) [9 10 11]))
  (assert (= (list (rest [])) [])))


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


(defn test-threading []
  (assert (= (-> (.upper "a b c d") (.replace "A" "X") (.split))
             ["X" "B" "C" "D"])))


(defn test-tail-threading []
  (assert (= (.join ", " (* 10 ["foo"]))
             (->> ["foo"] (* 10) (.join ", ")))))

(defn test-threading-in-macro []
  ; https://github.com/hylang/hy/issues/1537
  ; The macros need to be defined in another file or else the bug
  ; isn't visible in cb72a8c155ac4ef8e16afc63ffa80c1d5abb68a7
  (require tests.resources.macros)

  (tests.resources.macros.thread-set-ab)
  (assert (= ab 2))

  (tests.resources.macros.threadtail-set-cd)
  (assert (= cd 5)))


(defn test-threading-two []
  (assert (= (-> "a b c d" .upper (.replace "A" "X") .split)
             ["X" "B" "C" "D"])))


(defn test-as-threading []
  (setv data [{"name" "hooded cuttlefish"
               "classification" {"subgenus" "Acanthosepion"
                                "species" "Sepia prashadi"}
               "discovered" {"year" 1936
                            "name" "Ronald Winckworth"}}
              {"name" "slender cuttlefish"
               "classification" {"subgenus" "Doratosepion"
                                "species" "Sepia braggi"}
               "discovered" {"year" 1907
                            "name" "Sir Joseph Cooke Verco"}}])
  (assert (= (as-> (get data 0) x
                   (:name x))
             "hooded cuttlefish"))
  (assert (= (as-> (filter (fn [entry] (= (:name entry)
                           "slender cuttlefish")) data) x
                   (next x)
                   (:discovered x)
                   (:name x))
             "Sir Joseph Cooke Verco")))


(defn test-assoc []
  (setv vals {"one" "two"})
  (assoc vals "two" "three")
  (assert (= (get vals "two") "three")))


(defn test-multiassoc []
  (setv vals {"one" "two"})
  (assoc vals "two" "three" "four" "five")
  (assert (and (= (get vals "two") "three") (= (get vals "four") "five") (= (get vals "one") "two"))))


(defn test-assoc-eval-lvalue-once []
  ;; https://github.com/hylang/hy/issues/1068
  "`assoc` only evaluates its lvalue once"
  (setv counter [])
  (setv d {})
  (defn f []
    (.append counter 1)
    d)
  (assoc (f)  "a" 1  "b" 2  "c" 3)
  (assert (= d {"a" 1  "b" 2  "c" 3}))
  (assert (= counter [1])))


(defn test-of []
  (assert (= (of str) str))
  (assert (= (of List int) (get List int)))
  (assert (= (of Dict str str) (get Dict (, str str)))))
