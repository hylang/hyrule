(require
  hyrule [block branch ebranch case ecase cfor do-n lif list-n unless])
(import
  pytest)


(defn test-block-simple []

  (assert (= (block (block-ret 1) 2) 1))
  (assert (is (block (block-ret) 2) None))
  (assert (= (block 1 2) 2))
  (assert (= (block (when False (block-ret 1)) 2) 2))
  (assert (is (block) None))

  (setv x "")
  (block
    (+= x "1")
    (block-ret)
    (+= x "2"))
  (assert (= x "1"))

  (setv x 1)
  (setv r (block
    (for [n [1 2 3 4 5]]
      (*= x n)
      (when (= n 3)
        (block-ret "done")))))
  (assert (= r "done"))
  (assert (= x (* 1 2 3)))

  (setv x "")
  (setv r (block
    (for [a "abc"]
      (for [z "xyz"]
        (+= x a z)
        (when (= (+ a z) "by")
          (block-ret "bye"))))))
  (assert (= r "bye"))
  (assert (= x "axayazbxby"))

  (setv l [])
  (block
    (defn f [x]
      (.append l x)
      (when (= x 5)
        (block-ret))
      (f (+ x 1))
      (.append l "a"))
    (f 1)
    (.append l "b"))
  (assert (= l [1 2 3 4 5]))

  (setv x "")
  (setv r (block :fish
    (+= x "a")
    (block-ret-from :fish "stick")
    (+= x "b")))
  (assert (= r "stick"))
  (assert (= x "a"))

  (with [e (pytest.raises hy.errors.HyMacroExpansionError)]
    (hy.eval '(block :whale (block-ret-from :fish))))
  (assert (in "Unmatched block tag: :fish" e.value.msg))

  (assert (= (block None (block-ret-from None 1) 2) 1))
  (assert (= (block None (block-ret 1) 2) 1))

  ; Matching of block names is lexical and checked at macro-expansion
  ; time, even if the code would never be executed.
  (with [e (pytest.raises hy.errors.HyMacroExpansionError)]
    (hy.eval '(when False
      (block :b
        (block-ret-from :a)))))
  (assert (in "Unmatched block tag: :a" e.value.msg))

  ; `block-ret` and `block-ret-from` are ordinary names outside of
  ; `block` and the heads of expressions.
  (setv  block-ret "tiger"   block-ret-from "lion")
  (block
    (+= block-ret "z")
    (block-ret "p")
    (+= block-ret "q"))
  (assert (= block-ret "tigerz"))
  (assert (= block-ret-from "lion")))


(defn test-block-nested []

   (setv x "")
   (block :a
     (block :b
       (block :c
         (+= x "p")
         (block-ret-from :b)
         (+= x "q"))
       (+= x "r"))
     (+= x "s"))
   (assert (= "ps"))

   (setv x "")
   (setv r (block :b1
     (+= x "0")
     (while True (block :b2
       (for [n "12345"]
         (+= x n)
         (when (= n "3")
           (if (< (len x) 5)
             (block-ret-from :b2)
             (block-ret-from :b1))))))
     (+= x "9")))
   (assert (= x "0123123"))

   ; Names of inner blocks shadow the names of outer blocks.
   (setv x "")
   (block :a
     (+= x "a")
     (block :a
       (+= x "b")
       (block-ret-from :a)
       (+= x "c"))
     (+= x "d"))
   (assert (= x "abd"))

   ; Same thing, but with anonymous blocks.
   (setv x "")
   (block
     (+= x "a")
     (block
       (+= x "b")
       (block-ret)
       (+= x "c"))
     (+= x "d"))
   (assert (= x "abd")))


(defn test-branch []

  (with [(pytest.raises hy.errors.HyMacroExpansionError)]
    (hy.eval '(branch)))

  (assert (is (branch 1) None))
  (assert (is (branch (raise (ValueError))) None))
  (assert (is (branch (assert False)) None))
  (assert (= (branch (assert False) else 1) 1))

  (defn f [x]
    (branch (in (.lower x) it)
      "aeiou" "vowel"
      "xyz"   "last"))
  (assert (= (f "i") "vowel"))
  (assert (= (f "x") "last"))
  (assert (is (f "j") None))

  (defn f [x]
    (branch (is (.get {True False  False None  None True} x "?") it)
      True  1
      False 2
      None  3
      else  4))
  (assert (= (f True) 2))
  (assert (= (f False) 3))
  (assert (= (f None) 1))
  (assert (= (f "xyzzy") 4))

  (setv out "")
  (branch (do (+= out "X") (= it "middle"))
    "top" (do
      (+= out "a")
      (+= out "b"))
    "middle" (do
      (+= out "c")
      (+= out "d"))
    "bottom" (do
      (+= out "e")
      (+= out "f")))
  (assert (= out "XXcd"))

  (setv out "")
  ; Clauses after an `else` are allowed, but have no effect.
  (branch (= it "not here")
    1    (+= out "a")
    2    (+= out "b")
    else (+= out "c")
    3    (+= out "d")
    else (+= out "e")
    else (+= out "f"))
  (assert (= out "c"))

  ; Each case is evaluated at most once, even if `it` appears several
  ; times in the matcher.
  (setv tested [])
  (defn atest [x]
    (.append tested x)
    x)
  (assert (= "d" (branch (and (> (* it it) 5) (< it 0))
    (atest 0) "a"
    (atest 2) "b"
    (atest 10) "c"
    (atest -10) "d"
    (atest -6) "e")))
  (assert (= tested [0 2 10 -10]))

  ; Test a `branch` nested in the tester of another `branch`.
  (setv l [])
  (setv out (branch
    (do
      (.append l it)
      (setv x it)
      (setv out (branch
        (do
          (.append l it)
          (= (- it 100) x))
         50 "q"
        102 "r"
         60 "z"))
      (.append l it)
      out)
    1 "a"
    2 "b"
    3 "c"))
  (assert (= out "b"))
  (print l)
  (assert (= l [
    1 50 102 60 1
    2 50 102 2]))

  ; Test `ebranch`.
  (defn f [x]
    (setv (cut tested) [])
    (ebranch (= it x)
      (atest 1) "a"
      (atest 2) "b"
      (atest 3) "c"))
  (assert (= (f 2) "b"))
  (assert (= tested [1 2]))
  (assert (= (f 1) "a"))
  (assert (= tested [1]))
  (with [e (pytest.raises ValueError)]
    (f 4))
  (assert (= e.value.args #("ebranch: No branch matched")))
  (assert (= tested [1 2 3])))


(defn test-case []

  (with [(pytest.raises hy.errors.HyMacroExpansionError)]
    (hy.eval '(case)))

  (assert (is (case 1) None))
  ; Unlike `branch`, `case` still evaluates its first argument
  ; when there are no case clauses
  (setv out "a")
  (assert (is (case (setv out "b")) None))
  (assert (= out "b"))

  (defn f [x]
    (case x
      1     1
      "1"   2
      "a"   3
      True  4
      False 5
      None  6))
  (assert (= (f 1) 1))
  (assert (= (f "1") 2))
  (assert (= (f "a") 3))
  (assert (= (f True) 1))
    ; `case` uses `=`, not `is`, and `(= True 1)`.
  (assert (= (f False) 5))
  (assert (= (f None) 6))
  (assert (is (f 2) None))

  (assert (= (case 5  1 "a"  2 "b"  3 "c"  else "z") "z"))

  ; No matter how many cases are checked, the key form is only
  ; evaluated once.
  (setv l [])
  (assert (= "c" (case (do (.append l 1) 3)
    1 "a"
    2 "b"
    3 "c"
    4 "d")))
  (assert (= l [1])))


(defn test-cfor []
  (assert (= (cfor tuple x (range 10) :if (% x 2) x) #(1 3 5 7 9)))
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
