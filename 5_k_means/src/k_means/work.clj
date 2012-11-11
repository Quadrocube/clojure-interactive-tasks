(ns k-means.work
  (:use [k-means.core :only [run-empty run-2-circles run-3-circles run-random-circles]]))


;;; Your task is to implement clustering algorithm.
;;; You're a given a set of points on plane. And your goal is to divide them to k clusters.
;;; Implement k-means algorithm to solve this task: http://en.wikipedia.org/wiki/K-means_clustering
;;; Your function must take collection of points. Each point is a vector of x and y.
;;; It must return collection of clusters. Each cluster - collection of points.
;;; E.g. you have 4 points: [0 0] [1 1] [9 9] [10 10] and you need to partition them to 2 clusters.
;;; Input will be [[0 0] [9 9] [1 1] [10 10]] and output should be something like [[[0 0] [1 1]] [[9 9] [10 10]]].
;;; Note that you don't get k - number of clusters. You need to specify it somewhere in function.
;;; To test you solution use following tests:

;;; k-means

(def ^:dynamic k 1)

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn idx->clusters [M]
  (map #(map first %)
    (vals
      (group-by second M))))

(defn find-nearest [point other]
  (:point
    (first
      (sort-by :dist
         (for [x other] {:point x :dist (distance point x)})))))

(defn centers->clusters [centers points]
  (idx->clusters
    (for [p points] [p (find-nearest p centers)])))

(defn clusters->centers [clusters]
  (map 
    (fn [points] 
        [(/ (apply + (map first points)) (count points))
         (/ (apply + (map second points)) (count points))]) 
    clusters))

(defn next-clusters [clusters]
  (let [new-centers (clusters->centers clusters)]
    (centers->clusters new-centers (apply concat clusters))))

(defn k-means [points gen-initial-clusters]
  (loop [clusters (gen-initial-clusters points)]
    (let [n (next-clusters clusters)]
      (if (= n clusters)
          n
          (recur n))))) 
                
;;; initializing methods

(defn random-partition [points]
  (idx->clusters (for [p points] [p (rand-int k)])))

(defn Forgy [points]
  (let 
    [centers (take k
               (distinct
                 (repeatedly #(rand-nth points))))]
    (centers->clusters centers points)))

;;; k-determining methods

(defn static [_] 6)

;;; tests

(defn solution-generator [k-determ initialize]
  (fn [points]
    (binding [k (k-determ points)]
      (k-means points initialize))))

(def SOLUTION (solution-generator static random-partition))

; (run-empty SOLUTION)

; (run-2-circles SOLUTION)

; (run-3-circles SOLUTION)

;;; Manipulation: mouse click - add new point
;;;               space - reset simulation (remove all points or regenerate them, depenends on test)
;;; Note that may need use different solutions (with k = 2 for run-2-circles and  k = 3 for run-3-circles).



;;; Now try to improve your solution so it can determine k based on given points. So if there are visually 3 clusters it should partition points to 3 clusters, if 4 than to 4 clusters.
;;; Test your solution on this test:

(run-random-circles SOLUTION)



;;; Implement some other clustering algorithm.
