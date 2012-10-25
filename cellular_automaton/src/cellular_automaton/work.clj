(ns cellular-automaton.work
  (:use cellular-automaton.core))

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

(def size-x (quot w @cell-size))
(def size-y (quot h @cell-size))

; Conway's Game of Life (http://en.wikipedia.org/wiki/Conway's_Game_of_Life)

(defn GoL-neighbours [[x y]] (for [dx [0 1 -1] dy [0 1 -1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))
(defn [[x y]] GoL-count-neighbours (reduce #(when (not (nil? %2)) (inc %)) 
                                           0 
                                           (GoL-neighbours [x y])))

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
