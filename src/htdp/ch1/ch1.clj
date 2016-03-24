(ns htdp.ch1)

(comment
  "Skipping all of the exercises for which the answer is not a function. This
  is to enable testing. Maybe I'll go back and add maps of {ex# ans} later?")

(defn f->c
  "Ex 2.2.1"
  [t]
  (/ (- t 32) 1.8))

(defn dollar->euro
  "Ex 2.2.2"
  [d]
  (* d 0.89))

(defn triangle
  "Ex 2.2.3"
  [b h]
  (/ (* b h) 2))

(defn convert3
  "Ex 2.2.4"
  [ones tens hundreds]
  (+ (* 100 hundreds)
     (* 10 tens)
     ones))

(defn tax
  "Ex 2.3.1"
  [gross]
  (* gross 0.15))

(defn net
  "Ex 2.3.1"
  [hours]
  (let [gross (* hours 12)]
    (- gross (tax gross))))

(defn sum-coins
  "Ex 2.3.2"
  [counts]
  (reduce-kv (fn [total value n] (+ total (* value n)))
             0
             (zipmap [1 5 10 25] counts)))

(defn total-profit
  "Ex 2.3.3"
  [n]
  (- (* 4.5 n) 20))

;; The functions for the movie theater examples

(defn attendees
  "Ex 3.1.3-4"
  [ticket-price]
  (+ 120
     (* (/ 15 0.10) (- 5 ticket-price))))

(defn cost
  "Ex 3.1.3-4"
  [ticket-price]
  (+ 180 (* 0.04 (attendees ticket-price))))

(defn revenue
  "Ex 3.1.3-4"
  [ticket-price]
  (* (attendees ticket-price) ticket-price))

(defn profit
  "Ex 3.1.3-4"
  [ticket-price]
  (- (revenue ticket-price)
     (cost ticket-price)))


(defn in->cm
  "Ex 3.3.1"
  [in]
  (* 2.54 in))

(defn ft->in
  "Ex 3.3.1"
  [ft]
  (* 12 ft))

(defn yd->ft
  "Ex 3.3.1"
  [yd]
  (* yd 3))

(defn rd->yd
  "Ex 3.3.1"
  [rd]
  (* 5.5 rd))

(defn fl->rd
  "Ex 3.3.1"
  [fl]
  (* 40 fl))

(defn mi->fl
  "Ex 3.3.1"
  [mi]
  (* 8 mi))

(def ft->cm
  "Ex 3.3.1"
  (comp ft->in in->cm))

(def yd->cm
  "Ex 3.3.1"
  (comp yd->ft ft->cm))

(def rd->in
  "Ex 3.3.1"
  (comp rd->yd yd->ft ft->in))

(def mi->ft
  "Ex 3.3.1"
  (comp mi->fl fl->rd rd->yd yd->ft))



(def PI 3.1419)

(defn area-circle
  "Ex 3.3.2"
  [r]
  (* PI (* r r)))

(defn perimeter-circle
  "Ex 3.3.2"
  [r]
  (* 2 PI r))

(defn vol-cylinder
  "Ex 3.3.2"
  [r h]
  (* h (area-circle r)))

(defn area-ring
  "Ex 3.3.3-4"
  [inner outer]
  (- (area-circle outer)
     (area-circle inner)))

(defn area-pipe
  "Ex 3.3.3-4"
  [inner-r h thickness]
  (let [outer-r (+ inner-r thickness)]
    (+ (* 2 (area-ring inner-r outer-r))
       (* h (perimeter-circle inner-r))
       (* h (perimeter-circle outer-r)))))

(defn area-cylinder
  "Ex 3.3.3"
  [r h]
  (area-pipe 0 h r))

(defn rocket-height
  "Ex 3.3.5"
  [g t]
  (* 0.5 g t t))

(defn c->f
  "Ex 2.2.1"
  [t]
  (+ (* t 1.8) 32))

(def temp-I
  "Ex 2.2.1"
  (comp c->f f->c))


(defn interval [left-f left-val right-f right-val]
  (fn [x] (and (left-f left-val x) (right-f right-val x))))


;; Naming convention intvl5-6 is (5, 6), intvl-5-6 is [5,6], intvl-5-6- is [5,6], etc
;; Could go wicked crazy and make it a macro?

(def intvl3-7- "Ex 4.2.1.1" (interval < 3 >= 7))

(def intvl-3-7- "Ex 4.2.1.2" (interval <= 3 >= 7))

(def intvl-3-9 "Ex 4.2.1.3" (interval <= 3 > 9))

(def intvl1-3 "Ex 4.2.1.4" (interval < 1 > 3))

(def intvl9-11 "Ex 4.2.1.4" (interval < 9 > 11))

(defn intvl1-3&9-11 "Ex 4.2.1.4" [x] (or (intvl1-3 x) (intvl9-11 x)))

(defn intvl-not1-3 "Ex 4.2.1.5" [x] (not (intvl1-3 x)))

;; these are some math problems translated into clojure?
;; 4.2.3
(def problems-4-2-3
  [(fn [x] (= 62 (+ 2 (* 4 x))))
   (fn [x] (= 102 (* 2 x x)))
   (fn [x] (= 462 (+ (* 4 x x) (* 6 x) 2)))])


(defn interest
  "Ex 4.4.1"
  [amount]
  (let [rate (cond
               (<= amount 1000) 0.04
               (<= amount 5000) 0.045
               :else 0.05)]
    (* amount rate)))

(defn tax
  "Ex 4.4.2"
  [amount]
  (let [rate (cond
               (<= amount 240) 0
               (<= amount 480) 0.15
               :else 0.28)]
    (* amount rate)))

;; TANGENT AHEAD!

(defn graduated-tax
  "It's bugging me that you earn less making 481 than 480.... this function is
  ugly though, it should recurse through the brackets?"
  [amount]
  (cond
    (<= amount 240) 0
    (<= amount 480) (+ (* 0.15 (- amount 240)) 0)
    :else (+ (* 0.28 (- amount 480)) (graduated-tax 480))))

(defn amount-in-bracket
  "Returns the dollars in a a given bracket range
  i.e. if I have 110 dollars, 10 dollars are in the 100+ bracket"
  [amount [lower upper]]
  (max
    (if upper
      (min (- amount lower) (- upper lower))
      (- amount lower))
    0))

(defn sum-bracketed-percentages
  "Returns sum of bracketed percentages for a bracket set and a principal"
  [principal bracket-set]
  (reduce-kv (fn [total rate bracket]
               (+ total (* rate (amount-in-bracket principal bracket))))
             0
             bracket-set))

(defn grad-tax
  "A more generalized graduated-tax function."
  [amount]
  (let [brackets {0.0 [0 240] 0.15 [240 480] 0.28 [480]}]
    (sum-bracketed-percentages amount brackets)))

;; /TANGENT -- Oh.... that was the next question, kind of....

(defn pay-back
  "Ex 4.4.3"
  [spent]
  (let [brackets {0.0025 [0 500] 0.0050 [500 1500] 0.0075 [1500 2500] 0.01 [2500]}]
    (sum-bracketed-percentages spent brackets)))


(comment "NOTE for Section 5: Seems that DrScheme's symbols are a little different than
  Clojure's. I'm not going to mess around with the `symbol=?` stuff and I'm
  just going to be using maps instead of `cond` which seems a bit silly")


(defn check-guess
  "Ex 5.1.2"
  [guess target]
  (cond (< guess target) :TooSmall
        (= guess target) :Perfect
        (> guess target) :TooLarge))


;; Ex 6.1 - let's define some abstract shapes!
(defrecord Posn [x y])

(defprotocol Shape
  (area [s] "Calculates the area of a shape")
  (perimeter [s] "Calculates the perimeter of a shape"))

(defrecord Line [start end])

;; start is the top left position
(defrecord Rect [start width height]
  Shape
  (area [this] (* width height))
  (perimeter [this] (* 2 (+ width height))))


;; center is a Posn, radius a number
(defrecord Circle [center radius]
  Shape
  (area [this] (* PI (* radius radius)))
  (perimeter [this] (* 2 PI radius)))




;; The more I think about it the more Pixl is a display element that shouldn't
;; mix with the shapes that much
(defrecord Pixl [pos color])

(defn display-pixl [pixl]
  ((:color pixl) {
                   :black " "
                   :red   "R"
                   :blue  "B"
                   :green "G"}))

(defn display-row [row]
  (map display-pixl row))

(defn display-canvas [canv]
  (map display-row canv))

(defn print-canvas [canv]
  (map println (display-canvas canv)))


(defn start
  "Ex 6.2.1 - Creates a blank x/y sized grid of blank spots for pixels"
  [w h]
  (partition h (for [y (range h) x (range w)] (Pixl. (Posn. x y) :black))))


(defn distance-to-0
  "Ex 6.2.1"
  [pos]
  (Math/sqrt
    (+ (* (:x pos) (:x pos))
       (* (:y pos) (:y pos)))))


(defn draw-solid-line
  "Ex 6.2.1"
  [start end]
  nil)

;; This whole exercise section seems to really actually need a teachpack. I
;; think I'll go look on how to make those soon.
;; TODO: MAKE A GUI TIE-IN FOR THIS PART, DOING IT IN PIXEL ART TURNED OUT TO
;; BE STUPID


(comment "So the define-struct bit seems to pretty much be the defrecord for
  clojure, so it will be treated as such. Keep in mind that the instantiation
  in clojure is done either by `(Mystruct. arg1 arg2...)` there is no
  auto-generated `make-my-struct` or `my-struct-arg` functions.")


;; 6.3


(defrecord Entry [fullname zip phone])


;; 6.3.3

(defrecord Fighter [designation acceleration top-speed max-range])

(defn within-range [fighter dist]
  (>= (:max-range fighter) dist))

(defn reduce-range [fighter]
  (update fighter :max-range #(* 0.80 %)))

;; 6.4.1
;; Maybe go and use Schema for all of these?

(defrecord Movie [title producer])

(defrecord Boyfriend [fullname hair eyes phone])

(defrecord Cheerleader [fullname number])

(defrecord CD [artist title price])

(defrecord Sweater [material price producer])


;; 6.4.2

(defrecord Elapsed-Since-Midnight [hours minutes seconds])

;; 6.4.3

(defrecord TLWords [f s t])



;; 6.5

(defrecord Student [lastname firstname teacher])

(defn subst-teacher [student sub-teacher]
  (assoc student :teacher sub-teacher))

;; 6.5.2

(defn time->seconds [t]
  (+ (* 3600 (:hours t))
     (* 60 (:minutes t))
     (:seconds t)))










(comment "Below here is just random scratches")

(defn f-by-g [f g]
  (comment "
    Using Haskell notation: ([a] -> a) -> (b -> a) -> ([b] -> b) => a -> Key-able

    Example use:
      (def max-by-area (f-by-g max area))

    Maybe think of f as a selection function that operates on a collection
    generated by g? I need to write a better description")
  (fn [coll]
    (let [kv (zipmap (map g coll) coll)]
      (get kv (apply f (keys kv))))))





(reduce-kv
  (fn [m k v]
    (assoc m k (count (filter #(contains? % "b") v))))
  {}
  data)
