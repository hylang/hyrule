(require
 hyrule [seq defseq])
(import
 hyrule [end-sequence inc dec rest])


(defn test-infinite-sequence-slice []
  (assert (= (list (cut (seq [n] n) 5))
             [0 1 2 3 4])))


(defn test-len-finite-sequence []
  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))

  (assert (= (len shorty) 10) "Sequence length did not match"))


(defn test-list-finite-sequence []
  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  
  (setv 0-to-9 (list (range 10)))

  (assert (= (list shorty)
             0-to-9)
          "Sequence collected to list did not match"))


(defn test-indexing-sequence []
  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  (setv 0-to-9 (list (range 10)))

  (for [i (range -10 10)]
       (assert (= (get shorty i) 
                  (get 0-to-9 i))
               f"Element {i} did not match")))


(defn test-slicing-sequence []
  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  (setv 0-to-9 (list (range 10)))

  (for [i (range -10 10)]
    ; start only
    (assert (= (list (cut shorty i None None))
               (cut 0-to-9 i None None)))
    ; stop only
    (assert (= (list (cut shorty None i None))
               (cut 0-to-9 None i None)))
    ; step only
    (when i (assert (= (list (cut shorty None None i))    
                       (cut 0-to-9 None None i))))

    (for [j (range -10 10)]
      ; start and stop
      (assert (= (list (cut shorty i j None))
                 (cut 0-to-9 i j None)))
      ; stop and step
      (when j  (assert (= (list (cut shorty None i j))   
                          (cut 0-to-9 None i j))))
      ; start and step
      (when j (assert (= (list (cut shorty i None j))   
                         (cut 0-to-9 i None j))))
      
      (for [k (range -10 10)]
        ; start, stop, and step
        (when k (assert (= (list (cut shorty i j k)) 
                           (cut 0-to-9 i j k))))))))


(defn test-recursive-sequence []
  (defseq fibonacci [n]
    (cond (= n 0) 0
          (= n 1) 1
          True (+ (get fibonacci (- n 1))
                  (get fibonacci (- n 2)))))
  (assert (= (get fibonacci 0)
             0)
          "first element of fibonacci didn't match")
  (assert (= (get fibonacci 1)
             1)
          "second element of fibonacci didn't match")
  (assert (= (get fibonacci 40)
             102334155)
          "40th element of fibonacci didn't match")
  (assert (= (list (cut fibonacci 9))
             [0 1 1 2 3 5 8 13 21])
          "taking 9 elements of fibonacci didn't match"))


(defn test-nested-functions []
  (defseq primes [n]
    "infinite sequence of prime numbers"
    (defn divisible? [n prevs]
      "is n divisible by any item in prevs?"
      (any (map (fn [x] (not (% n x))) prevs))) 
    (defn previous-primes [n]
      "previous prime numbers"
      (cut primes (dec n)))
    (defn next-possible-prime [n]
      "next possible prime after nth prime"
      (inc (get primes (dec n))))
    
    (if (= n 0) 2
        (do (setv guess (next-possible-prime n))
            (while (divisible? guess (previous-primes n))
              (setv guess (inc guess)))
            guess)))

  (assert (= (list (cut primes 10))
             [2 3 5 7 11 13 17 19 23 29])
          "prime sequence didn't match"))
