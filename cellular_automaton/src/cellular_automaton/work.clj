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

; Conway's Game of Life (http://en.wikipedia.org/wiki/Conway's_Game_of_Life)

(def GoL-colours {nil back-colour :on 255})

(dosync
  (ref-set cells-previous {[1 0] :on 
                          [2 1] :on
                          [0 2] :on
                          [1 2] :on
                          [2 2] :on})
  (ref-set cells-to-redraw @cells-previous))

(defn GoL-neighbours [[x y]] (for [dx [0 1 -1] dy [0 1 -1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(defn GoL-step [] 
   (dosync
      (ensure cells)
      (ensure cells-previous)
      (ensure cells-to-redraw)
      (ref-set cells 
              (into {} (for [[pos n] (frequencies (mapcat GoL-neighbours (keys @cells-previous)))  
                             :when (or (= n 3) 
                                       (and (@cells-previous pos) 
                                            (= n 2)))]
                            [pos :on])))
      (ref-set cells-to-redraw (merge 
                                (zipmap (clojure.set/difference (set (keys @cells)) 
                                                                (set (keys @cells-previous)))
                                        (repeat :on))
                                (zipmap (clojure.set/difference (set (keys @cells-previous)) 
                                                                (set (keys @cells)))
                                        (repeat nil))))
      (ref-set cells-previous @cells) 
))

(field GoL-step GoL-colours "Game of Life")

#_(start-simulation
	game-of-life gol-cell-colors gol-switch-cell)

;;; Brian's Brain (http://en.wikipedia.org/wiki/Brian%27s_Brain)

#_(start-simulation
	brian-brain bb-cell-colors bb-switch-cell)

;;; Langton's ant (http://en.wikipedia.org/wiki/Langton%27s_ant)

#_(start-simulation
	langton-ant ant-cell-colors ant-switch-cell)

;;; Wireworld (http://en.wikipedia.org/wiki/Wireworld)

#_(start-simulation
	wire-world ww-cell-colors ww-switch-cell)
