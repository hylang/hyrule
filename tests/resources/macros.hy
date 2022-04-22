(require hyrule [-> ->>])


(defmacro test-macro []
  '(setv blah 1))


(defmacro thread-set-ab []
  (defn f [#* args] (.join "" (+ #("a") args)))
  (setv variable (hy.models.Symbol (-> "b" (f))))
  `(setv ~variable 2))


(defmacro threadtail-set-cd []
  (defn f [#* args] (.join "" (+ #("c") args)))
  (setv variable (hy.models.Symbol (->> "d" (f))))
  `(setv ~variable 5))
