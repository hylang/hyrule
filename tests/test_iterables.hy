(import
  itertools [count islice]
  hyrule [butlast coll? distinct drop-last flatten rest thru]
  pytest)


(defn test-butlast []
  (assert (= (list (butlast (range 10)))
             [0 1 2 3 4 5 6 7 8]))
  (assert (= (list (butlast [1]))
             []))
  (assert (= (list (butlast []))
             []))
  ; with an infinite sequence
  (assert (= (list (islice (butlast (count 10)) 5))
             [10 11 12 13 14])))


(defn test-coll? []
  (assert (coll? [1 2 3]))
  (assert (coll? []))
  (assert (coll? {"a" 1 "b" 2}))
  (assert (coll? #{1 2}))
  (assert (coll? (range 10)))
  (assert (coll? (bytearray b"abc")))
  (assert (not (coll? "abc")))
  (assert (not (coll? b"abc")))
  (assert (not (coll? 1)))
  (assert (not (coll? None))))


(defn test-distinct []
  (setv res (list (distinct [ 1 2 3 4 3 5 2 ])))
  (assert (= res [1 2 3 4 5]))
  ;; distinct of an empty list should be []
  (setv res (list (distinct [])))
  (assert (= res []))
  ;; now with an iter
  (setv test_iter (iter [1 2 3 4 3 5 2]))
  (setv res (list (distinct test_iter)))
  (assert (= res [1 2 3 4 5]))
  ; make sure we can handle None in the list
  (setv res (list (distinct [1 2 3 2 5 None 3 4 None])))
  (assert (= res [1 2 3 5 None 4])))


(defn test-drop-last []
  (assert (= (list (drop-last 5 (range 10 20)))
             [10 11 12 13 14]))
  (assert (= (list (drop-last 0 (range 5)))
             [0 1 2 3 4]))
  (assert (= (list (drop-last 100 (range 100)))
             []))
  ; with an infinite sequence
  (assert (= (list (islice (drop-last 100 (count 10)) 5))
             [10 11 12 13 14])))


(defn test-flatten []
  (assert (=
    (flatten [1 2 [3 4] 5])
    [1 2 3 4 5]))
  (assert (=
    (flatten ["foo" #(1 2) [1 [2 3] 4] "bar"])
    ["foo" 1 2 1 2 3 4 "bar"]))
  (assert (= (flatten "foo") ["foo"]))
  (assert (= (flatten 12.34) [12.34]))
  (assert (= (flatten [1]) [1]))
  (assert (= (flatten []) []))
  (assert (= (flatten #(1)) [1]))
  (assert (= (flatten #(1 #(None 3))) [1 None 3]))
  (assert (= (flatten {"a" 1 "b" 2}) ["a" "b"])))


(defn test-rest []
  (assert (= (list (rest [1 2 3 4 5])) [2 3 4 5]))
  (assert (= (list (islice (rest (count 8)) 3)) [9 10 11]))
  (assert (= (list (rest [])) [])))


(defn test-thru []
  (assert (is (type (thru 5)) (type (range 5))))

  (defn check [args values]
    (assert (= (list (thru #* args)) values)))
  (check [3]       [0 1 2 3])
  (check [-1 3]    [-1 0 1 2 3])
  (check [-1]      [])
  (check [3 1]     [])
  (check [3 1 -1]  [3 2 1])
  (check [0 5 2]   [0 2 4])
  (check [0 6 2]   [0 2 4 6])
  (check [5 0 -2]  [5 3 1])
  (check [6 0 -2]  [6 4 2 0])

  (assert [(pytest.raises TypeError)]
    (thru))
  (assert [(pytest.raises TypeError)]
    (thru 3.0))
  (assert [(pytest.raises TypeError)]
    (thru "3"))
  (assert [(pytest.raises ValueError)]
    (thru 1 10 0)))
