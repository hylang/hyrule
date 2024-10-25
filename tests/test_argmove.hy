(import unittest [mock])
(require hyrule [-> ->> as-> some-> doto])


(defn test-threading []
  (assert (= (-> (.upper "a b c d") (.replace "A" "X") .split)
             ["X" "B" "C" "D"])))


(defn test-bare-dotted []

  (assert (= (-> "a b c d" str.upper) "A B C D"))

  (defclass C)
  (setv foo (C))
  (setv foo.bar (C))
  (setv foo.bar.baz (C))
  (setv foo.bar.baz.quux (fn [x] (* x 10)))
  (assert (= (-> 3 foo.bar.baz.quux) 30)))


(defn test-tail-threading []
  (assert (= (.split (.join ", " (* 10 ["foo"])))
             (->> ["foo"] (* 10) (.join ", ") .split))))


(defn test-threading-dotted-properties []
  (defclass A []
    (defn __init__ [self]
      (setv self.a 1)))
  (setv x (A))
  (assert (= x.a
             (-> x (. a)))))


(defn test-threading-in-macro []
  ; https://github.com/hylang/hy/issues/1537
  ; The macros need to be defined in another file or else the bug
  ; isn't visible in cb72a8c155ac4ef8e16afc63ffa80c1d5abb68a7
  (require tests.resources.macros)

  (tests.resources.macros.thread-set-ab)
  (assert (= ab 2))

  (tests.resources.macros.threadtail-set-cd)
  (assert (= cd 5)))


(defn test-threading-two []
  (assert (= (-> "a b c d" .upper (.replace "A" "X") .split)
             ["X" "B" "C" "D"])))


(defn test-as-threading []
  (setv data [{"name" "hooded cuttlefish"
               "classification" {"subgenus" "Acanthosepion"
                                "species" "Sepia prashadi"}
               "discovered" {"year" 1936
                            "name" "Ronald Winckworth"}}
              {"name" "slender cuttlefish"
               "classification" {"subgenus" "Doratosepion"
                                "species" "Sepia braggi"}
               "discovered" {"year" 1907
                            "name" "Sir Joseph Cooke Verco"}}])
  (assert (= (as-> (get data 0) x
                   (:name x))
             "hooded cuttlefish"))
  (assert (= (as-> (filter (fn [entry] (= (:name entry)
                           "slender cuttlefish")) data) x
                   (next x)
                   (:discovered x)
                   (:name x))
             "Sir Joseph Cooke Verco")))


(defn test-threading-some []
  ; the macro uses `cond`, so test for odd and even number of expressions
  (assert (= (some-> 1 (+ 2) (* 3)) 9))
  (assert (= (some-> 1 (+ 2) (* 3) (+ 1)) 10))

  ; test for non-expression form
  (defn inc [x] (+ x 1))
  (assert (= (some-> 1 inc) 2)))


(defn test-threading-some-circuit []
  (setv m (mock.MagicMock))

  ; test for null head
  (assert (is (some-> None m) None))
  (assert (= m.call_count 0))  ;; m is never called

  ; test short-circuit with a function for odd and even number of
  ; expressions
  (defn ret_none [a b] None)

  (setv m (mock.MagicMock))
  (assert (is (some-> 1 m (ret_none 3) m) None))
  (assert (= m.call_count 1))  ;; m is called once only, before `ret_none`

  (setv m (mock.MagicMock))
  (assert (is (some-> 1 m m (ret_none 3) m) None))
  (assert (= m.call_count 2)))  ;; m is called twice only, before `ret_none`


(defn test-doto []
  (setv collection [])
  (doto collection (.append 1) (.append 2) (.append 3))
  (assert (= collection [1 2 3]))
  (setv res (doto (set) (.add 2) (.add 1)))
  (assert (= res (set [1 2])))
  (setv res (doto [] (.append 1) (.append 2) .reverse))
  (assert (= res [2 1])))
