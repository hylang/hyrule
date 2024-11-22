(require
  hyrule [loop])
(import
  sys
  hyrule [inc dec]
  pytest)


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


(defn test-recur-in-wrong-loc []
  (defn bad-recur [n]
    (loop [[i n]]
          (if (= i 0)
            0
            (inc (recur (dec i))))))

  (try
   (bad-recur 3)
   (except [e TypeError]
     (assert True))
   (else
    (assert False))))


(defn test-recur-string []
  "`loop` shouldn't touch a string named `recur`."
  (assert (= (loop [] (+ "recur" "1")) "recur1")))
