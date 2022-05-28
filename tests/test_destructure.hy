(require
  hyrule [setv+ dict=: defn+ fn+ let+])
(import
  pytest
  collections.abc
  itertools [cycle count islice]
  hyrule [destructure])

(defn iterator? [x]
  (isinstance x collections.abc.Iterator))

(defn test-iter []
  ;; empty
  (setv+ () [])
  ;; basic
  (setv+ (a b c) [1 2 3])
  (assert (= [a b c]
             [1 2 3]))
  ;; nested
  (setv+ (a (b (c (d))) e) [11 [22 [33 [44]]] 55])
  (assert (= [a b c d e]
             [11 22 33 44 55]))
  ;; :&
  (setv+ (a b :& the-rest) "abcdefg")
  (assert (iterator? the-rest))
  (assert (= [a b (list the-rest)]
             ["a" "b" ["c" "d" "e" "f" "g"]]))
  ;; dict=:
  (assert (= (dict=: (a (b (c)) d)
                     [1 [2 [3 4 5] 6] 7 8])
             {'a 1  'b 2  'c 3  'd 7}))
  ;; dict=: :&
  (setv D (dict=: (a (b (c :& inner)) d :& outer)
                  [1 [2 [3 4 5] 6] 7 8]))
  (assert (= (lfor k "abcd" (get D (hy.models.Symbol k)))
             [1 2 3 7]))
  (assert (= (list (get D 'inner))
             [4 5]))
  (assert (= (list (get D 'outer))
             [8]))
  ;; infinite
  (setv+ (a b c) (cycle [1 2]))
  (assert (= [a b c]
             [1 2 1]))
  ;; infinite :&
  (setv+ (a b c :& the-rest) (count))
  (assert (= [a b c]
             [0 1 2]))
  (assert (= (next the-rest)
             3))
  (assert (= (list (islice the-rest 5))
             [4 5 6 7 8]))
  ;; :as
  (setv+ (a b c :& d :as e) (count))
  (assert (= [a b c]
             [0 1 2]))
  (assert (iterator? d))
  (assert (iterator? e))
  (assert (= [3 4 5] (list (islice d 3))))
  (assert (= [0 1 2 3 4 5] (list (islice e 6))))
  ;; missing
  (setv+ (a b c :& d :as e) (gfor i (range 2) i))
  (assert (= [a b c]
             [0 1 None]))
  (assert (= [] (list d)))
  (assert (= [0 1] (list e))))

(defn test-list []
  ;; empty
  (setv+ [] [])
  ;; basic
  (setv+ [a b c] [1 2 3])
  (assert (= [a b c]
             [1 2 3]))
  ;; missing
  (setv+ [a b c] [1 2])
  (assert (= [a b c] [1 2 None]))
  ;; nested
  (setv+ [a [b [c [d]]] e] [11 [22 [33 [44]]] 55])
  (assert (= [a b c d e]
             [11 22 33 44 55]))
  ;; :as
  (setv+ [a b c :as full] [0 1 2 3 4 5])
  (assert (= [a b c]
             [0 1 2]))
  (assert (= full [0 1 2 3 4 5]))
  ;; :& and :as
  (setv+ [a b :& the-rest :as full] "abcdefg")
  (assert (= [a b the-rest full]
             ["a" "b" ["c" "d" "e" "f" "g"] (list "abcdefg")]))
  ;; keyword destructuring
  (setv+ [a b c :& {d :d e :e :as kwargs}] [1 2 3 :d 4 :e 5])
  (assert (= [a b c d e kwargs]
             [1 2 3 4 5 {:d 4 :e 5}])))


(defn test-dict []
  ;; empty
  (setv+ {} {})
  ;; basic
  (setv+ {a :a  b 'b  c "c"} {:a 1  'b 2  "c" 3})
  (assert (= [a b c]
             [1 2 3]))
  (setv+ {A :a  B 'b  C "c"} {:a 11  'b 22  "c" 33})
  (assert (= [A B C]
             [11 22 33]))
  ;; missing
  (setv+ {a :a b :b c :c} {:a 1 :c 3})
  (assert (= [a b c] [1 None 3]))
  ;; constructed keys
  (setv+ {foo (frozenset [0 1])  bar #(0 1)  baz .3}
      {(frozenset [0 1]) "spam"  #(0 1) "eggs"  .3 "bacon"})
  (assert (= [foo bar baz]
             ["spam" "eggs" "bacon"]))
  ;; mangling
  (setv+ {:keys [a? -b? c->d]} {:a? 1 :-b? 2 :c->d 3})
  (assert (= [a? -b? c->d]
             [1 2 3]))
  (setv+ {:strs [hello? is_hello]} {"hello?" 1 "is_hello" 2})
  (assert (= [hello? is_hello] [2 2]))
  ;; nested
  (setv+ {a :a  {b :b  {c :c} :y } :x} {:a 11  :x {:b 22  :y {:c 33}}})
  (assert (= [a b c]
             [11 22 33]))
  ;; :as
  (setv+ {a :a  b :b  :as full} {:a 0  :b 1})
  (assert (= [a b]
             [0 1]))
  (assert (= full {:a 0  :b 1}))
  ;; :or
  (setv+ {a :a  :or {a "foo"}} {})
  (assert (= a "foo"))
  (setv+ {a :a  :or {a "foo"}} {:a "bar"})
  (assert (= a "bar"))
  ;; :or
  (setv+ {x 'x  y "y"  z :z  :or {x "foo"  y "bar"}}
         {'x "spam"  "y" "eggs"  :z "bacon"})
  (assert (= [x y z]
             ["spam" "eggs" "bacon"]))
  ;; :or :keys
  (setv+ {:keys [x y z]  :or {x "foo"  y "bar"}}
         {:x "spam"  :z "bacon"})
  (assert (= [x y z]
             ["spam" "bar" "bacon"]))
  ;; :or :strs
  (setv+ {:strs [x y z]  :or {x "foo"  y "bar"}}
         {"y" "eggs"  "z" "bacon"})
  (assert (= [x y z]
             ["foo" "eggs" "bacon"]))
  ;; :strs and :as
  (setv+ {:as full :strs [a b c]}
         {"a" "a"  "b" "b"  "c" "c"})
  (assert (= [a b c]
             ["a" "b" "c"]))
  (assert (= full {"a" "a"  "b" "b"  "c" "c"})))

(defn test-both []
  (setv data {"cells" [{"type" "x"  "count" 3}
                       {"type" "y"  "count" 6}]
              "format" ["pretty" "purple"]
              "options" "xyzq"})
  (setv+ {[{:strs [count type]}
           {y-count "count"}  :as cells] "cells"
          [style color] "format"
          [X :& the-rest] "options"
          foo "foo"
          bar "bar"
          :or {foo 42  options "a"}
          :as full}
         data)
  (assert (= [count type]
             [3 "x"]))
  (assert (= y-count 6))
  (assert (= cells (get data "cells")))
  (assert (= [style color]
             ["pretty" "purple"]))
  (assert (= [X the-rest]
             ["x" ["y" "z" "q"]]))
  (assert (= [foo bar] [42 None]))
  (assert (= full data)))

(defn test-dict=: []
  (setv data {"cells" [{"type" "x"  "count" 3}
                       {"type" "y"  "count" 6}]
              "format" ["pretty" "purple"]
              "options" "xyzq"})
  (setv destructured
        (dict=: {[{:strs [count type]}
                         {y-count "count"}  :as cells] "cells"
                [style color] "format"
                [X :& the-rest] "options"
                foo "foo"
                :or {foo 42  options "a"}
                :as full}
                data))
  (setv+ expected
         {'full {"cells" [{"type" "x"
                          "count" 3}
                         {"type" "y"
                          "count" 6}]
                 "format" ["pretty"
                          "purple"]
                 "options" "xyzq"}
          'foo 42
          'the-rest ["y" "z" "q"]
          'X "x"
          'color "purple"
          'style "pretty"
          'cells [{"type" "x"
                   "count" 3}
                  {"type" "y"
                   "count" 6}]
          'y-count 6
          'type "x"
          'count 3})
  (assert (= destructured expected)))

(defn test-errors []
  (with [(pytest.raises SyntaxError)]
    (destructure '{a b c} {:a 1}))
  (with [(pytest.raises SyntaxError)]
    (destructure '[:as a :as b] []))
  (with [(pytest.raises SyntaxError)]
    (destructure '[:& a :& b] []))
  (with [(pytest.raises SyntaxError)]
    (destructure '{:syms [] :strs []} {}))
  (with [(pytest.raises SyntaxError)]
    (destructure '(:& a :& b) {}))
  (with [(pytest.raises SyntaxError)]
    (destructure '[a b :& d e :as f] (range 10)))
  (with [(pytest.raises SyntaxError)]
    (destructure '[a b d :as e :& f] (range 10))))

(defn test-failure-to-match []
  ;; https://github.com/hylang/hyrule/issues/27
  ;; empty
  (setv+ [foo]
        [])
  (assert (is foo None))
  ;; empty, missing key
  (setv+ {foo :bar}
        {})
  (assert (is foo None))
  ;; not empty, missing key
  (setv+ {foo :bar}
        {"nothing" "here"})
  (assert (is foo None))
  ;; nested twice, missing key
  (setv+ {{foo :bar} :baz}
         {:baz {"nothing" "here"}})
  (assert (is foo None))

  ;; nested once, missing key
  (setv+ {{foo :bar} :baz}
         {"nothing" "here"})
  (assert (is foo None))
  ;; nested twice, wrong type
  (setv+ {{foo :bar} :baz}
         {:baz []})
  (assert (is foo None))

  ;; shouldn't ignore :or
  (setv+ {{foo :bar :or {foo "eggs"}} :baz}
         {:baz {"nothing" "here"}})
  (assert (= foo "eggs"))
  ;; shouldn't ignore :or, even if it's false-y
  (setv+ {{foo :bar :or {foo []}} :baz}
         {:baz {"nothing" "here"}})
  (assert (= foo []))
  ;; shouldn't ignore :or, even if wrong type
  (setv+ {{foo :bar :or {foo "eggs"}} :baz}
         {:baz []})
  (assert (= foo "eggs"))

  ;; wrong type, list only
  (setv+ [baz [bar [quux [foo]] wibblethwop]]
          Ellipsis)
  (assert (is None foo bar baz quux wibblethwop))
  ;; wrong type, mixed
  (setv+ {[foo] :bar}
         {:bar Ellipsis})
  (assert (is foo None))

  ;; nested, missing structure
  (setv+ {{{[[{foo :bar}]] :baz} :bacon} :spam}
         {"nothing" "here"})
  (assert (is foo None))

  ;; :as pattern after a miss
  (setv+ [_ :as the-list] 3)
  (setv+ {_ "key" :as the-dict} 3)
  (assert (is the-list the-dict None))

  ;; :& pattern after a miss
  (setv+ [_ :& foo] 3)
  (assert (is foo None))

  ;; :keys pattern after a miss
  (setv+ {:keys [foo]} 3)
  (assert (is foo None))
  ;; :keys pattern after a partial miss
  (setv+ {:keys [foo] :as the-dict} {"nothing" "here"})
  (assert (is foo None))
  (assert (= the-dict {"nothing" "here"}))

  ;; :strs pattern after a miss
  (setv+ {:strs [foo]} 3)
  (assert (is foo None))
  ;; :strs pattern after a partial miss
  (setv+ {:strs [foo] :as the-dict} {"nothing" "here"})
  (assert (is foo None))
  (assert (= the-dict {"nothing" "here"}))

  ;; tries all the patterns, misses everything
  (setv+ [foo
          [bar :as the-list]
          {baz "key"
           zot "zot"
           ;; user wants "text" if zot misses, don't ignore it
           :or {zot "text"}
           :keys [quux]
           :strs [frob]
           :as the-dict}
          :& remaining
          :as everything]
         None)
  (assert (is foo bar baz quux frob remaining the-dict the-list everything
              None))
  (assert (= zot "text")))

(defn test-defn+ []
  (defn+ foo [[a b] {:keys [c d]} :& {:strs [e f]}]
    "bar foo"
    [a b c d e f])
  (assert (= [1 2 3 4 5 6] (foo [1 2] {:c 3 :d 4} "e" 5 "f" 6)))
  (assert (= "bar foo" foo.__doc__))
  (defn+ bar [&optional &rest &kwonly]
    (+ &optional &rest &kwonly))
  (assert (= (bar 1 2 3) 6)))

(defn test-fn+ []
  (setv f (fn+ [[a b] {:keys [c d]} :& {:strs [e f]}]
            [a b c d e f]))
  (assert (= [1 2 3 4 5 6]
             (f [1 2] {:c 3 :d 4} "e" 5 "f" 6)))
  (setv g (fn+ [&optional &rest &kwonly]
            (+ &optional &rest &kwonly)))
  (assert (= 6 (g 1 2 3))))

(defn test-let+ []
  (let+ [a 1]
    (assert (= a 1)))
  (let+ [[a b] [1 2]
         [c d] [3 4]
         {[style colour] "format" :strs [options]}
         {"cells" [{"type" "x"  "count" 3}
                   {"type" "y"  "count" 6}]
          "format" ["pretty" "purple"]
          "options" "xyzq"}]
    (assert (= [a b c d style colour options]
               [1 2 3 4 "pretty" "purple" "xyzq"]))))
