(import
  functools [partial]
  pytest
  hyrule [postwalk prewalk walk rest])

(setv walk-form '(print {"foo" "bar"
                         "array" [1 2 3 [4]]
                         "something" (+ 1 2 3 4)
                         "quoted?" '(foo)
                         "fstring" f"this {pytest} is {formatted !s :>{(+ width 3)}}"}))

(setv walk-form-inc '(print {"foo" "bar"
                             "array" [2 3 4 [5]]
                             "something" (+ 2 3 4 5)
                             "quoted?" '(foo)
                             "fstring" f"this {pytest} is {formatted !s :>{(+ width 4)}}"}))

(defn collector [acc x]
  (.append acc x)
  None)

(defn inc-ints [x]
  (if (isinstance x int) (+ x 1) x))

(defn test-walk-identity []
  (assert (= (walk (fn [x] x) (fn [x] x) walk-form)
             walk-form)))

(defn test-walk []
  (setv acc [])
  (assert (= (list (walk (partial collector acc) (fn [x] x) walk-form))
             [None None]))
  (assert (= acc (list walk-form)))
  (setv acc [])
  (assert (= (walk (fn [x] x) (partial collector acc) walk-form)
             None))
  (assert (= acc [walk-form])))

(defn test-walk-iterators []
  (assert (= (walk (fn [x] (* 2 x)) (fn [x] x)
                   (rest [1 [2 [3 [4]]]]))
             [[2 [3 [4]] 2 [3 [4]]]])))

;; test that expressions within f-strings are also walked
;; https://github.com/hylang/hy/issues/1843
(defn test-walking-update []
  (assert (= (hy.as-model (prewalk inc-ints walk-form)) walk-form-inc))
  (assert (= (hy.as-model (postwalk inc-ints walk-form)) walk-form-inc)))
