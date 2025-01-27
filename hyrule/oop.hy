(import
  hyrule.macrotools [map-model parse-defn-like])


(defmacro meth [#* args]
  #[[A replacement for :hy:func:`defn` that provides syntactic sugar for ``self``. As the name suggests, it's most useful for defining methods. The parameter list is automatically prepended with ``self``, and any reference to a symbol beginning with ``@`` is replaced such that ``@foo`` becomes ``self.foo``::

    (defclass BirdWatcher []

      (meth observe [bird]
        (@log bird)
        (setv @last-seen bird)
        @last-seen)

      (meth log [bird]
        (print "I just saw:" bird)))

    (setv x (BirdWatcher))
    (.observe x "sparrow")   ; I just saw: sparrow
    (.observe x "cardinal")  ; I just saw: cardinal
    (print x.last-seen)      ; cardinal

  ``@``-symbols that appear in the lambda list of the method are special: ``@foo`` is replaced with simply ``foo``, and the method body is prepended with ``(setv self.foo foo)``. This is convenient for parameters to ``__init__`` that set attributes of the same name::

    (defclass Rectangle []

      (meth __init__ [@width @height])
        ; Look Ma, no body!

      (meth area []
        (* @width @height)))

    (setv x (Rectangle 3 4))
    (print (.area x))  ; => 12

  The symbol ``@,`` is replaced with just plain ``self``. By contrast, the symbol ``@`` is left untouched, since it may refer to the Hy core macro :hy:func:`@ <hy.pyops.@>`.]]

  (_meth 'defn args))

(defmacro ameth [#* args]
  "Define an anonymous method. ``ameth`` is to :hy:func:`meth` as :hy:func:`fn` is to :hy:func:`defn`: it has the same syntax except that no method name (or decorators) are allowed."
  (_meth 'fn args))


(defn _meth [like args]
  (setv [headers params doc body] (parse-defn-like like args))
  (setv to-set [])
  (setv params (map-model params (fn [x]
    (when (and (isinstance x hy.models.Symbol) (.startswith x "@"))
      (setv x (hy.models.Symbol (cut x 1 None)))
      (.append to-set x)
      x))))
  (setv body (map-model body (fn [x]
    (when (and (isinstance x hy.models.Symbol) (.startswith x "@"))
      (cond
        (= x '@) '@
        (= x '@,) 'self
        True      `(. self ~(hy.models.Symbol (cut x 1 None))))))))
  `(~@headers [self ~@params]
     ~doc
    ~@(gfor
      sym to-set
      `(setv (. self ~sym) ~sym))
    ~@body))
