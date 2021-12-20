(require
  hyrule [smacrolet])
(import
  sys)


(defn test-smacrolet []
  (setv form '(do
                (setv foo (fn [a [b 1]] (* b (inc a))))
                (* b (foo 7)))
        form1 (hy.macroexpand
                '(smacrolet [b c]
                   (setv foo (fn [a [b 1]] (* b (inc a))))
                   (* b (foo 7))))
        form2 (hy.macroexpand
                '(smacrolet [a c]
                   (setv foo (fn [a [b 1]] (* b (inc a))))
                   (* b (foo 7))))
        form3 (hy.macroexpand
                '(smacrolet [foo bar]
                   (setv foo (fn [a [b 1]] (* b (inc a))))
                   (* b (foo 7)))))
  (assert (= form1 '(do
                      (setv foo (fn [a [b 1]] (* b (inc a))))
                      (* c (foo 7)))))
  (assert (= form2 form))
  (assert (= form3 '(do
                      (setv bar (fn [a [b 1]] (* b (inc a))))
                      (* b (bar 7))))))
