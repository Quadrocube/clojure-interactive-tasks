; TODO Use refs instead of atoms
(ns cellular-automaton.core
  (:use quil.core))
(use '[clojure.pprint :only (pp pprint)])

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
(def cells-to-redraw (atom (sorted-map))) 
(def cells           (atom {}))
(def cells-previous  (atom {}))
(def Name            (atom nil))

; Frame-rate control funcs
(defn redraw-continue [] (frame-rate 10))
(defn redraw-stop [] (frame-rate 0))
(defn redraw-stopped? [] (when (= current-frame-rate 0)
                     (true)))
(defn redraw-toggle [] (if (redraw-stopped?)
                    (redraw-continue)
                    (redraw-stop)))

; Handlers
(defn key-handler [] (let [pressed-key (raw-key)]
  (cond
    (= pressed-key \space)
        (redraw-toggle)
    (and (redraw-stopped?) (= pressed-key \n)) ; Complete
        (redraw)
    (= pressed-key \c)
      (let [prev-rate (current-frame-rate)] 
        (do
            (redraw-stop) 
            (reset! cells-previous (into {} (for [x (range 0 w cell-size) 
                                                  y (range 0 h cell-size)] 
                                                 [[x y] nil])))
            (redraw) 
            (frame-rate prev-rate)))
  )))

(defn mouse-handler [colours] 
  (let [mouse-coord [(mouse-x) (mouse-y)]
        updatable? (state :updatable?)
        cell-coord (from-board-coords mouse-coord)
        list-states (keys colours)
        cell-state (@cells-previous cell-coord) 
        next-states (concat (drop-while #(not= cell-state %) list-states) list-states)]
      (redraw-stop) 
      (reset! updatable? false)
      (swap! cells-to-redraw assoc cell-coord (second next-states))
      (swap! cells-previous assoc cell-coord (second next-states))
      (redraw)
      (reset! updatable? true)
  ))

; Main quil funcs
(defn draw [colours] 
  (doseq [[pos state] @cells-to-redraw :let [[x y] (to-board-coords pos)] :when (inside? [x y])] ; TODO change for use with while (sorted-map)
    (stroke-weight 1)          
    (stroke line-colour)
    (fill (colours state))                 
    (rect x y cell-size cell-size)))

(defn setup [colours]
  (smooth)                                
  (frame-rate 0)                          
  (set-state! :updatable? (atom false))
  (background back-colour)
  (doseq [i (range 0 (+ w 1) cell-size)]
         (line i 0 i h))
  (doseq [i (range 0 (+ h 1) cell-size)]
         (line 0 i w i))
  (draw colours))                       

(defn field [update-fn colours window-name] 
  (sketch
    :title window-name
    :draw #(do 
               (when @(state :updatable?) (update-fn)) 
               (draw colours)) 
    :setup #(setup colours)                
    :key-pressed key-handler
    :mouse-pressed #(mouse-handler colours)
    :size [w h]))
