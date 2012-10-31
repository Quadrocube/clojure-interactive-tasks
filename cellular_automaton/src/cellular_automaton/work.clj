(ns cellular-automaton.work
  (:use cellular-automaton.core))
(use '[clojure.pprint :only (pp pprint)])
;;; Your task is to implement cellular automaton.
;;; The most famous example of cellular automaton is Conway's Game of Life.
;;; Unlike previous tasks now you have to implement visualization and bots. So you need to implement everything :)
;;; I suggest to use quil library for animation (it was used in all previous tasks): https://github.com/quil/quil
;;; But of course you can use whatever you want.
;;; Keep in mind that is should be simple to run your simulator with different automata (Game of Life is only 1 example).


;;; Implement and run Brian's Brain automaton in your simulator: http://en.wikipedia.org/wiki/Brian%27s_Brain


;;; Implement Wireworld automaton: http://en.wikipedia.org/wiki/Wireworld


;;; Add Wireworld implementation to Rosetta Code (it's not present here yet): http://rosettacode.org/wiki/Wireworld


;;; Implement Von Neumann cellular automaton: http://en.wikipedia.org/wiki/Von_Neumann_cellular_automata


;;; Implement Langton's ant: http://en.wikipedia.org/wiki/Langton%27s_ant


;;; Add ability to change cells' states by mouse click, to restart and pause simulation.

(defn neighbours [[x y]] (for [dx [0 1 -1] dy [0 1 -1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(defn clean [l]
  (into {} (map #(let [[[_ state] freq] %] [state freq]) l)))

; Takes a sequence of rules
; Each rule should take the type of a cell and a mapping of type->count-neighbours-of-that-type and 
; return the keyword corresponding to that type of cell if the cell of a current time should appear
; nil otherwise
; f : [type [[type_i freq_i]]] -> nil | type
; Assumes that if a none of the rules applies to cell, the cell is dead
; Retturns a generated step-function

(defn stepper [rules neighbours] (fn []
   (dosync
      (ensure cells)
      (ensure cells-previous)
      (ensure cells-to-redraw)
      (ref-set cells 
              (into {} 
                    (for [[pos l] (group-by #(get-in % [0 0]) ; group by cell coordinates, so we get map pos->[[[pos] type-i] freq-i]
                                            (frequencies (for [[pos state] @cells-previous
                                                               neighbour (neighbours pos)] 
                                                          [neighbour state])))
                          :let [old-state (@cells-previous pos)
                                neighbour-types (clean l) ; currently we have some messed-up structure, clean it
                                new-state (some identity ((apply juxt rules) old-state neighbour-types))]
                          :when new-state]
                         [pos new-state])))
      (ref-set cells-to-redraw (merge @cells  
                                      (zipmap (clojure.set/difference (set (keys @cells-previous)) 
                                                                      (set (keys @cells)))
                                              (repeat nil))))
      (ref-set cells-previous @cells))))

; Conway's Game of Life (http://en.wikipedia.org/wiki/Conway's_Game_of_Life)

(def GoL-neighbours neighbours)

(def GoL-rules [(fn [_ neighbour-types] (when (= 3 (neighbour-types :on)) :on))
                (fn [state neighbour-types] (when (and (= state :on) (= 2 (neighbour-types :on))) :on))])

(def GoL-step (stepper GoL-rules GoL-neighbours))

(def GoL-colours {nil back-colour :on [255 255 255]})

(def GoL-glider {[1 0] :on 
                 [2 1] :on
                 [0 2] :on
                 [1 2] :on
                 [2 2] :on})

#_(field GoL-step GoL-colours GoL-glider "Game of Life")

;;; Brian's Brain (http://en.wikipedia.org/wiki/Brian%27s_Brain)

(def BB-neighbours neighbours)

(def BB-rules [(fn [state _] (when (= state :on) :dying))
               (fn [state neighbour-types] (when (and (= 2 (neighbour-types :on)) (nil? state)) :on))])

(def BB-step (stepper BB-rules BB-neighbours))

(def BB-colours {nil back-colour :on [255 255 255] :dying [0 0 140]})

(def BB-oscilator {[3 2] :dying
                   [5 3] :dying
                   [2 4] :dying
                   [4 5] :dying
                   [3 3] :on
                   [4 3] :on
                   [3 4] :on
                   [4 4] :on})

(def BB-grow-pattern {[200 150] :on
                      [200 151] :on})

#_(field BB-step BB-colours BB-grow-pattern "Brian's Brain")

;;; Langton's ant (http://en.wikipedia.org/wiki/Langton%27s_ant)


;;; Wireworld (http://en.wikipedia.org/wiki/Wireworld)

(def Ww-neighbours neighbours)

(def Ww-rules [(fn [state _]                (when (= state :head) :tail))
               (fn [state _]                (when (= state :tail) :wire))
               (fn [state neighbours-types] (when (= state :wire) (if (#{2 1} (neighbours-types :head)) :head :wire)))])

(def Ww-step (stepper Ww-rules Ww-neighbours))

(def Ww-colours {nil back-colour :head [63 128 255] :tail [223 73 10] :wire [246 214 19]})

(def Ww-triod {[28 27] :wire, [30 29] :wire, [55 23] :wire, [27 27] :head, [29 29] :wire, [54 23] :wire,
		       [26 27] :tail, [28 29] :wire, [53 23] :wire, [55 25] :wire, [27 29] :wire, [52 23] :wire,
		       [54 25] :wire, [25 28] :wire, [26 29] :wire, [53 25] :wire, [55 27] :wire, [51 24] :wire,
		       [52 25] :wire, [54 27] :wire, [53 27] :wire, [50 25] :wire, [51 26] :wire, [52 27] :wire,
		       [49 25] :wire, [48 25] :wire, [47 25] :wire, [46 25] :wire, [45 25] :head, [44 25] :tail,
		       [43 25] :wire, [41 24] :wire, [42 25] :wire, [38 22] :wire, [39 23] :tail, [40 24] :head,
		       [41 25] :wire, [37 22] :wire, [39 24] :head, [41 26] :wire, [36 22] :wire, [38 24] :head,
		       [40 26] :wire, [35 22] :wire, [38 25] :wire, [39 26] :wire, [38 26] :wire, [39 27] :wire,
		       [33 21] :head, [34 22] :wire, [32 21] :tail, [38 28] :wire, [31 21] :wire, [33 23] :wire,
		       [37 28] :wire, [30 21] :wire, [32 23] :wire, [36 28] :wire, [29 21] :wire, [31 23] :wire,
		       [35 28] :wire, [28 21] :wire, [30 23] :wire, [27 21] :wire, [29 23] :tail, [33 27] :head,
		       [34 28] :wire, [26 21] :wire, [28 23] :head, [32 27] :tail, [27 23] :wire, [31 27] :wire,
		       [33 29] :wire, [25 22] :wire, [26 23] :wire, [30 27] :wire, [32 29] :wire, [29 27] :wire,
		       [31 29] :wire}) ; Thx, kabbi =)

(field Ww-step Ww-colours Ww-triod "Wireworld")

