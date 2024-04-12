(require
  hyrule [seq defseq])
(import
  itertools [count combinations]
  hyrule [Sequence end-sequence])


(defn test-via-constructor []

  (setv s (Sequence (count 5)))
  (assert (= (get s 0) 5))
  (assert (= (get s 10) 15))

  (defn combs [] (combinations "abcde" 2))
  (setv s (Sequence (combs)))
  (assert (= (len s) (len (tuple (combs)))))

  (setv computed [])
  (setv s (Sequence ((fn []
    (for [n (count)]
      (.append computed n)
      (yield (* n 10)))))))
  (assert (= computed []))
  (assert (= (get s 3) 30))
  (assert (= computed [0 1 2 3]))
  (assert (= (get s 2) 20))
  (assert (= computed [0 1 2 3]))
  (assert (= (get s 5) 50))
  (assert (= computed [0 1 2 3 4 5])))


(defn test-list []
  ; https://github.com/hylang/hyrule/issues/63
  (assert (= (list (seq [n] (if (< n 5) n (end-sequence))))
             [0 1 2 3 4])))


(defn test-get []

  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  (assert (= (get shorty  0) 0))
  (assert (= (get shorty  5) 5))
  (assert (= (get shorty -1) 9))

  ; Try a negative index on a new sequence for
  ; https://github.com/hylang/hyrule/issues/63
  (assert (= (get (seq [n] (if (< n 10) n (end-sequence))) -1) 9))

  (assert (= (get (seq [n] n) 5) 5)))


(defn test-cut []

  (defseq shorty [n]
    (if (< n 10) n (end-sequence)))
  (defmacro cs [#* args]
    `(list (cut shorty ~@args)))
  (assert (= (cs  5           ) [0 1 2 3 4]))
  (assert (= (cs -8           ) [0 1]))
  (assert (= (cs  2    6      ) [2 3 4 5]))
  (assert (= (cs  6    2      ) []))
  (assert (= (cs  2    8     2) [2 4 6]))
  (assert (= (cs  8    2    -2) [8 6 4]))
  (assert (= (cs -2   -7    -2) [8 6 4]))
  ; https://github.com/hylang/hyrule/issues/65
  (assert (= (cs  5   None    ) [5 6 7 8 9]))
  (assert (= (cs -2   None    ) [8 9]))
  (assert (= (cs None  0    -1) [9 8 7 6 5 4 3 2 1]))
  (assert (= (cs None -3    -1) [9 8]))
  (assert (= (cs None None  -1) [9 8 7 6 5 4 3 2 1 0]))

  (assert (= (list (cut (seq [n] n) 5)) [0 1 2 3 4]))
  (assert (= (get (cut (seq [n] n) None None 2) 5) 10)))


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


(defn test-repr []
  (assert (= (repr (Sequence [])) "Sequence([])"))
  (assert (= (repr (Sequence [4, 5, 6])) "Sequence([4, 5, 6])"))
  (assert (= (repr (Sequence (count))) "Sequence([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ...])")))
