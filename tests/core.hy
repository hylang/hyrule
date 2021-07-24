(defn assert-true [x]
  (assert (= True x)))

(defn assert-false [x]
  (assert (= False x)))

(defn assert-equal [x y]
  (assert (= x y)))

(defn assert-none [x]
  (assert (is x None)))


(defn test-doto []
  (setv collection [])
  (doto collection (.append 1) (.append 2) (.append 3))
  (assert-equal collection [1 2 3])
  (setv res (doto (set) (.add 2) (.add 1)))
  (assert-equal res (set [1 2]))
  (setv res (doto [] (.append 1) (.append 2) .reverse))
  (assert-equal res [2 1]))

(defn test-comment []
  (assert-none (comment <h1>This is merely a comment.</h1>
                        <p> Move along. (Nothing to see here.)</p>)))

(defn test-coll? []
  (assert-true (coll? [1 2 3]))
  (assert-true (coll? {"a" 1 "b" 2}))
  (assert-true (coll? (range 10)))
  (assert-false (coll? "abc"))
  (assert-false (coll? 1)))


(defn test-butlast []
  (assert-equal (list (butlast (range 10)))
                [0 1 2 3 4 5 6 7 8])
  (assert-equal (list (butlast [1]))
                [])
  (assert-equal (list (butlast []))
                [])
  ; with an infinite sequence
  (import itertools)
  (assert-equal (list (islice (butlast (itertools.count 10)) 5))
                [10 11 12 13 14]))


(defn test-dec []
  (assert-equal 0 (dec 1))
  (assert-equal -1 (dec 0))
  (assert-equal 0 (dec (dec 2))))


(defn test-distinct []
  (setv res (list (distinct [ 1 2 3 4 3 5 2 ])))
  (assert-equal res [1 2 3 4 5])
  ;; distinct of an empty list should be []
  (setv res (list (distinct [])))
  (assert-equal res [])
  ;; now with an iter
  (setv test_iter (iter [1 2 3 4 3 5 2]))
  (setv res (list (distinct test_iter)))
  (assert-equal res [1 2 3 4 5])
  ; make sure we can handle None in the list
  (setv res (list (distinct [1 2 3 2 5 None 3 4 None])))
  (assert-equal res [1 2 3 5 None 4]))


(defn test-drop-last []
  (assert-equal (list (drop-last 5 (range 10 20)))
                [10 11 12 13 14])
  (assert-equal (list (drop-last 0 (range 5)))
                [0 1 2 3 4])
  (assert-equal (list (drop-last 100 (range 100)))
                [])
  ; with an infinite sequence
  (import itertools)
  (assert-equal (list (islice (drop-last 100 (itertools.count 10)) 5))
                [10 11 12 13 14]))


(defn test-flatten []
  (setv res (flatten [1 2 [3 4] 5]))
  (assert-equal res [1 2 3 4 5])
  (setv res (flatten ["foo" (, 1 2) [1 [2 3] 4] "bar"]))
  (assert-equal res ["foo" 1 2 1 2 3 4 "bar"])
  (setv res (flatten [1]))
  (assert-equal res [1])
  (setv res (flatten []))
  (assert-equal res [])
  (setv res (flatten (, 1)))
  (assert-equal res [1])
  ;; test with None
  (setv res (flatten (, 1 (, None 3))))
  (assert-equal res [1 None 3])
  (try (flatten "foo")
       (except [e [TypeError]] (assert (in "not a collection" (str e)))))
  (try (flatten 12.34)
       (except [e [TypeError]] (assert (in "not a collection" (str e))))))


(defn test-inc []
  (assert-equal 3 (inc 2))
  (assert-equal 0 (inc -1))

  (defclass X [object]
    (defn __add__ [self other] (.format "__add__ got {}" other)))
  (assert-equal (inc (X)) "__add__ got 1"))


(defn test-parse-args []
  ; https://github.com/hylang/hy/issues/1875
  (setv parsed-args (parse-args [["strings" :nargs "+" :help "Strings"]
                                 ["-n" :action "append" :type int :help "Numbers" "--numbers"]]
                                ["a" "b" "-n" "1" "--numbers" "2"]
                                :description "Parse strings and numbers from args"))
  (assert-equal parsed-args.strings ["a" "b"])
  (assert-equal parsed-args.numbers [1 2]))


(defn test-constantly []
  (setv helper (constantly 42))

  (assert-true (= (helper) 42))
  (assert-true (= (helper 1 2 3) 42))
  (assert-true (= (helper 1 2 :foo 3) 42)))



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


(defn test-list-n []

  (assert (= (list-n 4 1) [1 1 1 1]))

  (setv l (list (range 10)))
  (assert (= (list-n 3 (.pop l)) [9 8 7])))


(defn test-cfor []
  (assert (= (cfor tuple x (range 10) :if (% x 2) x) (, 1 3 5 7 9)))
  (assert (= (cfor all x [1 3 8 5] (< x 10))) True)
  (assert (= (cfor dict x "ABCD" [x True])
             {"A" True  "B" True  "C" True  "D" True})))
