(import
  itertools [count islice]
  hyrule [butlast coll? distinct drop-last flatten rest])


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
  (setv res (flatten [1 2 [3 4] 5]))
  (assert (= res [1 2 3 4 5]))
  (setv res (flatten ["foo" #(1 2) [1 [2 3] 4] "bar"]))
  (assert (= res ["foo" 1 2 1 2 3 4 "bar"]))
  (setv res (flatten [1]))
  (assert (= res [1]))
  (setv res (flatten []))
  (assert (= res []))
  (setv res (flatten #(1)))
  (assert (= res [1]))
  ;; test with None
  (setv res (flatten #(1 #(None 3))))
  (assert (= res [1 None 3]))
  (try (flatten "foo")
       (except [e [TypeError]] (assert (in "not a collection" (str e)))))
  (try (flatten 12.34)
       (except [e [TypeError]] (assert (in "not a collection" (str e))))))


(defn test-rest []
  (assert (= (list (rest [1 2 3 4 5])) [2 3 4 5]))
  (assert (= (list (islice (rest (count 8)) 3)) [9 10 11]))
  (assert (= (list (rest [])) [])))
