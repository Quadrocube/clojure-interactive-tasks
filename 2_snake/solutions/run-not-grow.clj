( fn [snake apple] (
   let [lr (fn [s a] 
              (if (> (first s) (first a))
                 :left
                 :right
              )
           )]
        (if (> (second snake) (second apple))
            :up
            (if (= (second snake) (second apple)) 
                (lr snake apple)
                :down
            )
        )
))  
