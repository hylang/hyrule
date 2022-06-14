(require
  hyrule [assoc])


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
