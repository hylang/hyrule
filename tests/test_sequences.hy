(require
  hyrule [seq defseq])
(import
  hyrule [end-sequence])


(defn test-get []
  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  (assert (= (get shorty  0) 0))
  (assert (= (get shorty  5) 5))
  (assert (= (get shorty -1) 9)))


(defn test-cut []

  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  (assert (= (list (cut shorty 2 6   )) [2 3 4 5]))
  (assert (= (list (cut shorty 2 8  2)) [2 4 6]))
  (assert (= (list (cut shorty 8 2 -2)) [8 6 4]))

  (assert (= (list (cut (seq [n] n) 5)) [0 1 2 3 4])))


(defn test-recursive []
  (defseq fibonacci [n]
    (cond (= n 0) 0
          (= n 1) 1
          True (+ (get fibonacci (- n 1))
                  (get fibonacci (- n 2)))))
  (assert (= (get fibonacci 0) 0))
  (assert (= (get fibonacci 1) 1))
  (assert (= (get fibonacci 40) 102,334,155))
  (assert (= (list (cut fibonacci 9)) [0 1 1 2 3 5 8 13 21])))


(defn test-nested-functions []
  (defseq fibonacci [n]
    (defn prev-sum []
      (+ (get fibonacci (- n 1))
         (get fibonacci (- n 2))))
    (cond (= n 0) 0
          (= n 1) 1
          True (prev-sum)))
  (assert (= (list (cut fibonacci 9)) [0 1 1 2 3 5 8 13 21])))
