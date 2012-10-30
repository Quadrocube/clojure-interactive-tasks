(ns snake.work
  (:use [snake.core :only (run-not-grow run-grow run-many-apples run-with-walls)]))


;;; You're writing a bot for playing snake.
;;; So, you are a snake and your goal is to collect apples.
;;; Field sizes: 40 x 30
;;; Every turn you move to one of the neighbours cell.
;;; Your function must take 2 arguments: snake's position and apple's position and decide which direction to move.
;;; Directions are: :up, :down, :left, :right (they are keywords). Your function must return one of these directions.
;;; Position (snake's or apple's) is a vector of 2 elements: x and y.
;;; In this task snake is not growing after it ate an apple so there is no danger of snake hitting itself.
;;; Note: upper left corner cell is (0, 0).

;;; Uncomment and substitute your solution
#_( def SOLUTION 
)
; (run-not-grow SOLUTION)



;;; Snake grows now (each time snake eats an apple, it's length increases).
;;; You need to write similar function as in previous task.
;;; It takes 2 arguments.
;;; First argument is snake's body - collection of cells, each cell is a vector of x and y. First cell is snake's head.
;;; Second argument is apple's position - vector of x and y.
;;; It should return direction: :up, :down, :left or :right.
;;; Note that you cannot change direction to the opposite in 1 move: snake will hit it's tail if length is 2 or more.
;;; Wait, you can change direction but snake will die :\

;;; Uncomment and substitute your solution
(defn move-growing [snake apple]
    (let [apple      #{apple} ; change in 3-rd
          len        (count snake)
          head       (first snake)
          snake      (zipmap snake (range len 0 -1))                        ; snake {[pos turn-to-vanish]}
          abs        (fn [x] (max x (- x)))
          neighbours (fn [[x y]] (for [dx [1 0 -1] dy [1 0 -1] ;ADD WRAP!
                                     :when (= 1 (+ (abs dx) (abs dy)))]
                                    [(mod (+ dx x) 40) (mod (+ dy y) 30)]))

          direct     (fn [[x1 y1] [x2 y2]] (cond (= (mod (dec y1) 30) y2) :up
                                                 (= (mod (inc y1) 30) y2) :down
                                                 (= (mod (dec x1) 40) x2) :left
                                                 (= (mod (inc x1) 40) x2) :right))]

        (loop [restricted snake                                            ; restricted {[pos turn-to-vanish]}
               queue      (list [head 0 nil])]                             ; queue '([cur-pos cur-turn beg-move]])

            (let [current (first queue)
                  other (rest queue)
                  beg-move (last current)
                  next-move-num (inc (second current))
                  possible-moves (map #(do [% next-move-num (if (nil? beg-move)
                                                                        (direct (first current) %)
                                                                        beg-move)]) 
                                      (filter #(or (nil? (restricted %)) 
                                                   (> next-move-num (restricted %))) ; >= ?
                                              (neighbours (first current))))
                  apple? (some #(if (apple (first %)) % nil) possible-moves)]

                (if (nil? apple?)
                    (recur (into restricted (for [[pos turn _] possible-moves] [pos (+ turn len)])) (concat other possible-moves))
                    (last apple?)
                )
            )
        )
    )
)

;(run-grow move-growing)



;;; Now you have many apples (5) instead of one.
;;; Function the same as previous but it takes set of apples instead of the single apple.
;;; Each apple in the set is a vector of x and y.
;;; E.g. you can try to reach nearest apple to the snake.

;;; Uncomment and substitute your solution
(defn move-many-apples [snake apple]
    (let [apple      (set apple) 
          len        (count snake)
          head       (first snake)
          snake      (zipmap snake (range len 0 -1))                        ; snake {[pos turn-to-vanish]}
          abs        (fn [x] (max x (- x)))
          neighbours (fn [[x y]] (for [dx [1 0 -1] dy [1 0 -1] ;ADD WRAP!
                                     :when (= 1 (+ (abs dx) (abs dy)))]
                                    [(mod (+ dx x) 40) (mod (+ dy y) 30)]))

          direct     (fn [[x1 y1] [x2 y2]] (cond (= (mod (dec y1) 30) y2) :up
                                                 (= (mod (inc y1) 30) y2) :down
                                                 (= (mod (dec x1) 40) x2) :left
                                                 (= (mod (inc x1) 40) x2) :right))]

        (loop [restricted snake                                            ; restricted {[pos turn-to-vanish]}
               queue      (list [head 0 nil])]                             ; queue '([cur-pos cur-turn beg-move]])

            (let [current (first queue)
                  other (rest queue)
                  beg-move (last current)
                  next-move-num (inc (second current))
                  possible-moves (map #(do [% next-move-num (if (nil? beg-move)
                                                                        (direct (first current) %)
                                                                        beg-move)]) 
                                      (filter #(or (nil? (restricted %)) 
                                                   (> next-move-num (restricted %))) ; >= ?
                                              (neighbours (first current))))
                  apple? (some #(if (apple (first %)) % nil) possible-moves)]

                (if (nil? apple?)
                    (recur (into restricted (for [[pos turn _] possible-moves] [pos (+ turn len)])) (concat other possible-moves))
                    (last apple?)
                )
            )
        )
    )
)

; (run-many-apples move-many-apples)



;;; Walls are added. So snake can hit wall and die.
;;; Your function now takes third argument - set of walls.
;;; Each wall is a cell that snake is not allowed to  move to.
;;; Wall is a vector of x and y.

;;; Uncomment and substitute your solution
(defn move-with-walls [snake apple walls]
    (let [apple      (set apple) 
          len        (count snake)
          head       (first snake)
          snake      (zipmap snake (range len 0 -1))                        ; snake {[pos turn-to-vanish]}
          abs        (fn [x] (max x (- x)))
          neighbours (fn [[x y]] (for [dx [1 0 -1] dy [1 0 -1] ;ADD WRAP!
                                     :when (= 1 (+ (abs dx) (abs dy)))]
                                    [(mod (+ dx x) 40) (mod (+ dy y) 30)]))

          direct     (fn [[x1 y1] [x2 y2]] (cond (= (mod (dec y1) 30) y2) :up
                                                 (= (mod (inc y1) 30) y2) :down
                                                 (= (mod (dec x1) 40) x2) :left
                                                 (= (mod (inc x1) 40) x2) :right))]

        (loop [restricted (into snake (for [w walls] [w 999999999]))         ; restricted {[pos turn-to-vanish]}
               queue      (list [head 0 nil])]                               ; queue '([cur-pos cur-turn beg-move]])

            (let [current (first queue)
                  other (rest queue)
                  beg-move (last current)
                  next-move-num (inc (second current))
                  possible-moves (map #(do [% next-move-num (if (nil? beg-move)
                                                                        (direct (first current) %)
                                                                        beg-move)]) 
                                      (filter #(or (nil? (restricted %)) 
                                                   (> next-move-num (restricted %))) ; >= ?
                                              (neighbours (first current))))
                  apple? (some #(if (apple (first %)) % nil) possible-moves)]

                (if (nil? apple?)
                    (recur (into restricted (for [[pos turn _] possible-moves] [pos (+ turn len)])) (concat other possible-moves))
                    (last apple?)
                )
            )
        )
    )
)

(run-with-walls move-with-walls)

; Ideas: - remember the individual list for each branch (eats dozens of memory, blocked right now)
;        - Multi-appled: try to eat all five apples, if current branch fails - return back and try again in another order
