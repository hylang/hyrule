(require
  hyrule [loop])
(import
  math
  sys
  hyrule [inc dec recur]
  pytest)


(defn test-factorial []
  (assert (=
    (loop [[i 5] [acc 1]]
      (if (= i 0)
       acc
       (recur (dec i) (* acc i))))
    (math.factorial 5))))


(defn test-tco-sum []

  ; This plain old tail-recursive function should exceed Python's
  ; default maximum recursion depth.
  (defn non-tco-sum [x y]
    (cond
      (> y 0) (inc (non-tco-sum x (dec y)))
      (< y 0) (dec (non-tco-sum x (inc y)))
      True x))
  (with [(pytest.raises RecursionError)]
    (non-tco-sum 100 10,000))

  ; With `loop`, it should work.
  (defn tco-sum [x y]
    (loop [[x x] [y y]]
      (cond
        (> y 0) (recur (inc x) (dec y))
        (< y 0) (recur (dec x) (inc y))
        True x)))
  (assert (= (tco-sum 100 10,000) 10,100)))


(defn test-nested []
  (assert (=
    (loop [[x 1]]
      (if (< x 3)
        (recur (+ x 1))
        [x (loop [[y 1]]
          (if (< y 5)
            (recur (+ y 1))
            y))]))
    [3 5])))


(defn test-fancier-args []
  (assert (=
    (loop [[x 1] #* a #** b]
      (if (= x 1)
        (recur 2 3 4 :foo "bar")
        [x a b]))
    [2 #(3 4) {"foo" "bar"}])))


(defn test-recur-string []
  "`loop` shouldn't touch a string named `recur`."
  (assert (= (loop [] (+ "recur" "1")) "recur1")))
