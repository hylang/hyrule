(import
  hyrule [assoc])


(defn test-assoc []
  (setv vals {1 2})
  (assoc vals)
  (assert (= vals {1 2}))

  (setv vals {1 2})
  (assoc vals 3 4)
  (assert (= vals {1 2  3 4}))

  (setv vals {1 2})
  (assoc vals  3 4  5 6)
  (assert (= vals {1 2  3 4  5 6})))


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
