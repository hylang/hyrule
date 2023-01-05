(require hyrule [-> ->> as-> doto])


(defn test-threading []
  (assert (= (-> (.upper "a b c d") (.replace "A" "X") (.split))
             ["X" "B" "C" "D"])))


(defn test-tail-threading []
  (assert (= (.join ", " (* 10 ["foo"]))
             (->> ["foo"] (* 10) (.join ", ")))))


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
  (assert (= (-> "a b c d" (.upper) (.replace "A" "X") (.split))
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


(defn test-doto []
  (setv collection [])
  (doto collection (.append 1) (.append 2) (.append 3))
  (assert (= collection [1 2 3]))
  (setv res (doto (set) (.add 2) (.add 1)))
  (assert (= res (set [1 2])))
  (setv res (doto [] (.append 1) (.append 2) (.reverse)))
  (assert (= res [2 1])))
