(require
  hyrule [cfor do-n lif list-n unless])


(defn test-cfor []
  (assert (= (cfor tuple x (range 10) :if (% x 2) x) (, 1 3 5 7 9)))
  (assert (= (cfor all x [1 3 8 5] (< x 10))) True)
  (assert (= (cfor dict x "ABCD" [x True])
             {"A" True  "B" True  "C" True  "D" True})))


(defn test-do-n []
  (setv n 0)

  (do-n 1 (+= n 1))
  (assert (= n 1))
  (do-n 3 (+= n 1))
  (assert (= n 4))
  (do-n 0 (+= n 1))
  (assert (= n 4))
  (do-n -2 (+= n 1))
  (assert (= n 4))

  (do-n 2 (+= n 1) (+= n 2))
  (assert (= n 10))

  (do-n 2 (+= n 1) (+= n 2) (break))
  (assert (= n 13)))


(defn test-lif []
  ;; None is false
  (assert (= (lif None "true" "false") "false"))

  ;; But everything else is True!  Even falsey things.
  (for [x [True False 0 "some-string" "" (+ 1 2 3)]]
    (assert (= (lif x "true" "false") "true")))

  ;; Test ellif [sic]
  (setv x 0)
  (assert (= (lif None 0
                  None 1
                  x 2
                  3)
             2)))


(defn test-list-n []

  (assert (= (list-n 4 1) [1 1 1 1]))

  (setv l (list (range 10)))
  (assert (= (list-n 3 (.pop l)) [9 8 7])))


(defn test-unless []
  (assert (= (unless False 1) 1))
  (assert (= (unless False 1 2) 2))
  (assert (= (unless False 1 3) 3))
  (assert (= (unless True 2) None))
  (assert (= (unless (!= 1 2) 42) None))
  (assert (= (unless (!= 2 2) 42) 42)))
