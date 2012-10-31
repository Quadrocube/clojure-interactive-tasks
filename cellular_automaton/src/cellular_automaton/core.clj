(ns cellular-automaton.core
  (:use quil.core))
(use '[clojure.pprint :only (pp pprint)]) ; Only for debug purposes

; field-size
(def cell-size 10)
(def w (* 80 cell-size))
(def h (* 60 cell-size))
(defn inside? [[x y]] (and (< x w)
                           (< y h)
                           (>= x 0)
                           (>= y 0)))
(defn to-board-coords [[x y]] (do [(* x cell-size) (* y cell-size)]))
(defn from-board-coords [[x y]] (do [(int (/ x cell-size)) (int (/ y cell-size))]))

; colors
(def back-colour 60)
(def line-colour 10)

; Automaton-specific details
(def cells-to-redraw (ref (sorted-map))) 
(def cells           (ref {}))
(def cells-previous  (ref {}))
(def Name            (atom nil))
(def updatable?      (ref false))

; Handlers
(defn key-handler [] (let [pressed-key (raw-key)]
  (cond
    (= pressed-key \space)
      (dosync
          (alter updatable? not))
    (= pressed-key \n) 
      (dosync
          (ensure  updatable?)
          (ref-set updatable? true)
          (redraw)
          (ref-set updatable? false))
    (= pressed-key \c)
      (dosync
        (let [prev-state (ensure updatable?)] 
          (ref-set updatable? false) 
          (ensure  cells-previous)
          (ref-set cells-previous (into {} (for [x (range 0 w cell-size) 
                                                 y (range 0 h cell-size)] [[x y] nil])))
          (redraw) 
          (ref-set updatable? prev-state))))))

(defn mouse-handler [colours] 
  (dosync
     (let [mouse-coord [(mouse-x) (mouse-y)]
           cell-coord (from-board-coords mouse-coord)
           list-states (keys colours)
           cell-state (@cells-previous cell-coord) 
           next-states (concat (drop-while #(not= cell-state %) list-states) list-states)
           prev-board-state (ensure updatable?)]
         (ref-set updatable? false)
         (alter   cells-to-redraw assoc cell-coord (second next-states))
         (alter   cells-previous assoc cell-coord (second next-states))
         (redraw)
         (ref-set updatable? prev-board-state))))

; Main quil funcs
(defn draw [colours] 
  (doseq [[pos state] @cells-to-redraw :let [[x y] (to-board-coords pos)] :when (inside? [x y])] ; TODO change for use with while (sorted-map)
    (stroke-weight 1)          
    (stroke line-colour)
    (fill (colours state))                 
    (rect x y cell-size cell-size)))

(defn setup [colours]
  (smooth)                                
  (frame-rate 10)                          
  (background back-colour)
  (doseq [i (range 0 (+ w 1) cell-size)]
         (line i 0 i h))
  (doseq [i (range 0 (+ h 1) cell-size)]
         (line 0 i w i)))

(defn field [update-fn colours window-name] 
  (sketch
    :title window-name
    :draw #(do (when @updatable? (update-fn)) 
               (draw colours)) 
    :setup #(setup colours)                
    :key-pressed key-handler
    :mouse-pressed #(mouse-handler colours)
    :size [w h]))
