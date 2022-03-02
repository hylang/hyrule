(require hyrule [defmain])

(defmain [#* args]
  (print (+ "<" (.join "|" (cut args 1 None)) ">"))
  (print "Hello World")
  (when (in "exit47" args)
    47))
