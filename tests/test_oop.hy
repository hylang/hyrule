(require
  hyrule [meth ameth])
(import
  pytest)


(defn example-decorator [f]
  (setv f.da "hello")
  f)


(defn test-meth []

  (defclass MM []
    (defn __matmul__ [self other]
      #("matmul" other)))

  (defclass Pony []
    (meth set-an-attr [value]
      "this is a @docstring"
      (setv @attr value))
    (meth use-an-attr []
      (+ @attr 5))
    (meth at-call []
      (@set-an-attr 8))
    (meth get-self []
      @,)
    (meth do-matmul []
      (@ (MM) 2)))

  (setv x (Pony))
  (assert (= x.set-an-attr.__doc__ "this is a @docstring"))
  (.set-an-attr x 1)
  (assert (= x.attr 1))
  (assert (= (.use-an-attr x) 6))
  (.at-call x)
  (assert (= x.attr 8))
  (assert (is (.get-self x) x))
  (assert (= (.do-matmul x) #("matmul" 2))))


(defn test-meth-decorated []

  (defclass Pony []
    (meth [classmethod] set-class-attr [value]
      (setv @attr value))
    (meth [example-decorator] set-instance-attr [value]
      (setv @attr value)))

  (assert (= Pony.set-instance-attr.da "hello"))
  (setv x (Pony))
  (.set-class-attr x 2)
  (assert (= x.attr 2))
  (assert (= Pony.attr 2))
  (.set-instance-attr x 1)
  (assert (= x.attr 1))
  (assert (= Pony.attr 2))
  (assert (= (. (Pony) attr) 2)))


(defn test-meth-init []

  (setv got None)

  (defclass Pony []
    (meth __init__
        [a1 @i1 [@i2 "i2-default"] [a2 "a2-default"] #* @ia #** @ikw]
      "docstring"
      (nonlocal got)
      (setv got [a1 @i1 i2 a2 @ia @ikw])
      (setv @i1 "override")))

  (assert (= Pony.__init__.__doc__ "docstring"))
  (setv x (Pony 1 2))
  (assert (= got [1 2 "i2-default" "a2-default" #() {}]))
  (assert (= x.i1 "override"))
  (assert (= x.i2 "i2-default"))
  (assert (= x.ia #()))
  (assert (= x.ikw {}))
  (assert (not (hasattr x "a1")))
  (assert (not (hasattr x "a2")))

  (setv x (Pony 1 2 3 4 5 6 7 :foo "bar"))
  (assert (= got [1 2 3 4 #(5 6 7) {"foo" "bar"}]))
  (assert (= x.i1 "override"))
  (assert (= x.i2 3))
  (assert (= x.ia #(5 6 7)))
  (assert (= x.ikw {"foo" "bar"})))


(defn test-meth-init-decorated []

  (defclass Pony []
    (meth [example-decorator] __init__ [@value]
      (setv @attr 2)))

  (assert (= Pony.__init__.da "hello"))
  (setv x (Pony 1))
  (assert (= x.value 1))
  (assert (= x.attr 2)))


(do-mac (when (>= hy.I.sys.version-info #(3 12))
'(defn test-meth-fancy []

  (defclass Pony []
    (meth
      :async
      [example-decorator]
      :tp [T]
      #^ (get list T) fancy-meth
      [a @b]
      [(- a @b)]))

  (setv p (Pony))
  (assert (= (hy.I.asyncio.run (.fancy-meth p 5 2)) [3]))
  (assert (= p.b 2))
  (assert (= Pony.fancy-meth.da "hello"))
  (assert (=
    (str (:return (hy.I.inspect.get-annotations Pony.fancy-meth)))
    "list[T]")))))


(defn test-ameth []

  (defclass Pony []
    (setv my-meth (ameth [value]
      (setv @attr value))))

  (setv x (Pony))
  (.my-meth x 1)
  (assert (= x.attr 1)))
