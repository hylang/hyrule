(require
  hyrule.collections [assoc]
  hyrule.control [unless])
(import
  functools [partial]
  itertools [islice]
  collections [OrderedDict]
  hy.reserved
  hyrule.collections [prewalk walk by2s]
  hyrule.iterables [coll?]
  hyrule.macrotools [macroexpand-all])


(defn call? [form]
  "Checks whether form is a non-empty hy.models.Expression"
  (and (isinstance form hy.models.Expression)
       form))

(setv _mangled-core-macros (frozenset
  (map hy.mangle (hy.reserved.macros))))


(defn lambda-list [form]
  "splits a fn argument list into sections based on &-headers.

  returns an OrderedDict mapping headers to sublists.
  Arguments without a header are under None.
  "
  (setv headers ['unpack-iterable '* 'unpack-mapping]
        sections (OrderedDict [(, None [])])
        vararg-types {'unpack-iterable (hy.models.Symbol "#*")
                      'unpack-mapping (hy.models.Symbol "#**")}
        header None)
  (for [arg form]
    (cond
      [(in arg headers)
       (do (setv header arg)
           (assoc sections header [])
           ;; Don't use a header more than once. It's the compiler's problem.
           (.remove headers header))]

      [(and (isinstance arg hy.models.Expression) (in (get arg 0) headers))
       (do (setv header (get arg 0))
           (assoc sections header [])
           ;; Don't use a header more than once. It's the compiler's problem.
           (.remove headers header)
           (.append (get sections header) arg))]

      [True (.append (get sections header) arg)]))
  sections)


(defn symbolexpand [form expander
                    [protected (frozenset)]
                    [quote-level 0]]
  (.expand (SymbolExpander form expander protected quote-level)))

(defclass SymbolExpander[]

  (defn __init__ [self form expander protected quote-level]
    (setv self.form form
          self.expander expander
          self.protected protected
          self.quote-level quote-level))

  (defn expand-symbols [self form [protected None] [quote-level None]]
    (if (is protected None)
        (setv protected self.protected))
    (if (is quote-level None)
        (setv quote-level self.quote-level))
    (symbolexpand form self.expander protected quote-level))

  (defn traverse [self form [protected None] [quote-level None]]
    (if (is protected None)
        (setv protected self.protected))
    (if (is quote-level None)
        (setv quote-level self.quote-level))
    (walk (partial symbolexpand
                   :expander self.expander
                   :protected protected
                   :quote-level quote-level)
          (fn [x] x)
          form))

  ;; manages quote levels
  (defn +quote [self [x 1]]
    `(~(self.head) ~@(self.traverse (self.tail)
                                    :quote-level (+ self.quote-level x))))

  (defn handle-dot [self]
    `(. ~(self.expand-symbols (get (self.tail) 0))
        ~@(walk (fn [form]
                  (if (isinstance form hy.models.Symbol)
                      form  ; don't expand attrs
                      (self.expand-symbols form)))
                (fn [x] x)
                (cut (self.tail) 1 None))))

  (defn head [self]
    (get self.form 0))

  (defn tail [self]
    (cut self.form 1 None))

  (defn handle-except [self]
    (setv tail (self.tail))
    ;; protect the "as" name binding the exception
    `(~(self.head) ~@(self.traverse tail (| self.protected
                                            (if (and tail (= (len (get tail 0)) 2))
                                                #{(get tail 0 0)}
                                                #{})))))

  (defn handle-match [self]
    ;; protect name bindings from match patterns
    (setv [expr #* cases] (self.tail)
          new-expr (self.expand-symbols expr)
          new-cases [])
    (defn traverse-clauses [args]
      (unless args
        (return))
      (setv a (next (islice args 1 None) None)
            index (cond
                    [(and (= a :as) (= (get args 3) :if)) 6]
                    [(in a (, :as :if)) 4]
                    [True 2])
            [clause more] [(cut args None index) (cut args index None)]
            protected #{}
            [pattern #* body] clause)

      (when (= (get body 0) :as)
        (protected.add (get body 1)))

      (defn handle-match-symbol [form]
        (if (in "." form)
            (self.expand-symbols form)
            (do
              (.add protected form)
              form)))

      (defn handle-match-call [form]
        (setv head (get form 0))
        (setv tail (cut form 1 None))
        (cond
          [(= head '.) (self.expand-symbols form)]
          [True `(~head ~@(traverse-pattern tail))]))

      (defn handle-pattern-form [form]
        (cond
          [(and (isinstance form hy.models.Symbol) (!= form '_))
            (handle-match-symbol form)]
          [(call? form)
            (handle-match-call form)]
          [(coll? form)
            (traverse-pattern form)]
          [True
            form]))

      (defn traverse-pattern [pattern]
        (walk handle-pattern-form (fn [x] x) pattern))

      (setv new-pattern (handle-pattern-form pattern))
      (setv new-body (self.expand-symbols body (| protected self.protected)))
      (.extend new-cases [new-pattern #* new-body])
      (traverse-clauses more))
    (traverse-clauses cases)
    `(~(self.head) ~new-expr ~@new-cases))

  (defn handle-args-list [self]
    (setv protected #{}
          argslist [])
    (for [[header section] (.items (lambda-list (get (.tail self) (if (= (self.head) (hy.models.Symbol "defn")) 1 0))))]
      (unless (in header [None 'unpack-iterable 'unpack-mapping])
          (.append argslist header))
      (cond [(in header [None '*])
             (for [pair section]
               (cond [(coll? pair)
                      (.add protected (get pair 0))
                      (.append argslist
                               `[~(get pair 0)
                                 ~(self.expand-symbols (get pair 1))])]
                     [True
                      (.add protected pair)
                      (.append argslist pair)]))]
            [(in header ['unpack-iterable 'unpack-mapping])
             (.update protected (gfor  [_ b #* _] section  b))
             (.extend argslist section)]))
    (, protected argslist))

  (defn handle-fn [self]
    (setv [protected argslist] (.handle-args-list self))
    (setv defn? (= (.head self) 'defn))
    `(; The operator
      ~(.head self)
      ; The name of the function, in the case of `defn`
      ~@(if defn?
        [(get (.tail self) 0)]
        [])
      ; The lambda list
      ~argslist
      ; The function body
      ~@(self.traverse
        (cut (.tail self) (if defn? 2 1) None)
        (| protected self.protected))))

  ;; don't expand symbols in quotations
  (defn handle-quoted [self]
    (if (call? self.form)
        (cond [(in (self.head) '[unquote unquote-splice]) (self.+quote -1)]
              [(= (self.head) 'quasiquote) (self.+quote)]
              [True (self.handle-coll)])
        (if (coll? self.form)
            (self.handle-coll)
            (self.handle-base))))

  ;; convert dotted names to the standard special form
  (defn convert-dotted-symbol [self]
    (self.expand-symbols `(. ~@(map hy.models.Symbol (.split self.form '.)))))

  (defn expand-symbol [self]
    (if (not-in self.form self.protected)
        (self.expander self.form)
        (self.handle-base)))

  (defn handle-symbol [self]
    (if (and self.form
             (not (.startswith self.form '.))
             (in '. self.form))
        (self.convert-dotted-symbol)
        (self.expand-symbol)))

  (defn handle-global [self]
    (.update self.protected (set (self.tail)))
    (self.handle-base))

  (defn handle-defclass [self]
    ;; don't expand the name of the class
    `(~(self.head) ~(get (self.tail) 0)
      ~@(self.traverse (cut (self.tail) 1 None))))

  (defn handle-special-form [self]
    ;; don't expand other special form symbols in head position
    `(~(self.head) ~@(self.traverse (self.tail))))

  (defn handle-base [self]
    self.form)

  (defn handle-coll [self]
    ;; recursion
    (self.traverse self.form))

  ;; We have to treat special forms differently.
  ;; Quotation should suppress symbol expansion,
  ;; and local bindings should shadow those made by let.
  (defn handle-call [self]
    (setv head (get self.form 0))
    (cond
      [(in head '[fn defn]) (self.handle-fn)]
      [(in head '[import
                  require
                  quote
                  eval-and-compile
                  eval-when-compile]) (self.handle-base)]
      [(= head 'except) (self.handle-except)]
      [(= head '.) (self.handle-dot)]
      [(= head 'defclass) (self.handle-defclass)]
      [(= head 'match) (self.handle-match)]
      [(= head 'quasiquote) (self.+quote)]
        ;; must be checked last!
      [(in (hy.mangle head) _mangled-core-macros)
        (self.handle-special-form)]
        ;; Not a special form. Traverse it like a coll
      [True (self.handle-coll)]))

  (defn expand [self]
    "the main entry point. Call this to do  the expansion"
    (setv form self.form)
    (cond
      [self.quote-level (self.handle-quoted)]
      [(isinstance form hy.models.Symbol) (self.handle-symbol)]
      [(call? form) (self.handle-call)]
      [(coll? form) (self.handle-coll)]
        ;; recursive base case--it's an atom. Put it back.
      [True (self.handle-base)])))
