"Create an SVG logo for Hyrule inspired by the Triforce and by the
suckers of a cuttlefish.

https://commons.wikimedia.org/wiki/File:SuckersOfACuttle-Fish,1834.PNG
https://commons.wikimedia.org/wiki/File:Natural_History_-_Mollusca_-_Sucker.png"


(import
  math [sqrt])


(defn logo []
  (setv z (/ (sqrt 3) 3))
  (setv w 500)
  (setv vb [-1 (* 2 (- z)) 2 (* 3 z)])

  (+
    "<?xml version='1.0' encoding='UTF-8'?>"
    (render-elem (E.svg
      :width w :height (* (sqrt 3) (/ w 2))
      :viewBox (.join " " (map str vb))
      :xmlns "http://www.w3.org/2000/svg"
      ; Draw an equilateral triangle.
      (E.symbol :id "triangle" (E.polygon
        :points f"0,{(* 2 (- z))} 1,{z} -1,{z}"))
      (E.use
        :href "#triangle"
        :fill "rgb(0%, 50%, 50%)"
        :mask "url(#m)")
      ; Mask out three circles from the triangle.
      (E.mask :id "m"
        (E.use
          :href "#triangle"
          :fill "white")
        (E.circle :id "c"
          :cx 0
          :cy (- (* (/ 3 4) z))
          :r (* (/ 2 5) z)
          :fill "white"
          :stroke "black"
          :stroke-width (/ z 3))
        (E.use :href "#c" :transform "rotate(120)")
        (E.use :href "#c" :transform "rotate(240)"))))))


; In combination with `render-elem`, `E` works roughly like
; `lxml.builder.ElementMaker`.
(setv E ((type "ElementMaker" #() (dict
  :__getattr__ (fn [self tag]
    (fn [#* kids #** attrs]
      #(tag attrs kids)))))))

(defn render-elem [x]
  (defn hesc [x]
    (hy.I.html.escape (str x)))
  (when (isinstance x str)
    (return (hesc x :quote False)))
  (setv [tag attrs kids] x)
  (.format "<{} {}>{}</{}>"
    (hesc tag)
    (.join " " (gfor
      [k v] (.items attrs)
      f"{(hesc (.replace k "_" "-"))}='{(hesc v)}'"))
    (.join "" (map render-elem kids))
    (hesc tag)))


(when (= __name__ "__main__")
  (print (logo)))
