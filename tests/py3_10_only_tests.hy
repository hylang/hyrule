(require hy.contrib.walk [let])

(defn test-let-with-pattern-matching []
  (let [x [1 2 3]
        y (dict :a 1 :b 2 :c 3)
        b 1
        a 1
        _ 42]
    (assert (= [2 3]
               (match x
                      [1 #* x] x)))
    (assert (= [3 [1 2 3] [1 2 3]]
               (match x
                      [_ _ 3 :as a] :as b :if (= a 3) [a b x])))
    (assert (= [1 2]
               (match [1 2]
                      x x)))
    (assert (= 42
               (match [1 2 3]
                    _ _)))))
