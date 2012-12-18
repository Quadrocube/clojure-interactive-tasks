(ns church-encoding.work
  (:use [church-encoding.core]))

;;; Task is to implement arithmetic on Church numerals.
;;; Check this page: http://en.wikipedia.org/wiki/Church_encoding
;;; You can use utility function to-church-num and to-normal-num to convert normal to church and church to normal:
;;; Note that to-church-num returns function that takes 1 argument (f)
;;; and returns function that takes 1 argument (x) that calculates (f (f ... (f x)...))
;;; All functions in this task must be 1 argument functions that return other functions.

;;; Example:

(def church-5 (to-church-num 5))    ; 5 in church numerals

(defn print-star [x] (print "*") x) ; Takes 1 argument, prints a star and retuns argument without modification.

((church-5 print-star) nil)         ; Prints ***** to console

(to-normal-num church-5)            ; returns 5

(def church-2 (to-church-num 2))    ; we'll use it in examples later

(def church-0 (to-church-num 0))


;;; Implement + (plus) for church numerals.

(def plus 
  (fn [a]
    (fn [b]
      (fn [f]
        (fn [x]
          ((a f) ((b f) x)))))))

(to-normal-num ((plus church-2) church-2)) ; must return 4

(test-plus plus) ; test your solution



;;; Implement * (multiplication) for church numerals

(def mult 
  (fn [a]
    (fn [b]
      (fn [f]
        (fn [x]
          ((a (b f)) x))))))

(to-normal-num ((mult church-2) church-5)) ; must return 10

(test-mult mult) ; test your solution


;;; Implement ^ (pow function) for church numerals.

(def pow 
  (fn [a]
    (fn [b]
      (b a))))

(to-normal-num ((pow church-2) church-5)) ; must return 32

(test-pow pow) ; test your solution


;;; Implement dec function for church numerals.

(def make-pair
  (fn [a]
    (fn [b]
      (fn [f]
        ((f a) b)))))

(def First
  (fn [a]
    (fn [b]
      a)))

(def Second
  (fn [a]
    (fn [b]
      b)))


(defn Dec [n]
  (fn [f]
    (fn [x]
      (((n (fn [p]
             ((make-pair (f (p First))) (p First))))
          ((make-pair x) x)) Second))))

(to-normal-num (Dec church-5)) ; must return 4

(test-dec Dec) ; test your solution


;;; Implement sum function. sum takes number n and returns sum of all numbers less or equals to n.
;;; You'll need to use recursion here. For recursion you'll need lazy values.
;;; You can use delay for that: http://clojuredocs.org/clojure_core/1.2.0/clojure.core/delay

(def True First)
(def False Second)

(defn Zero? [x]
    ((x (fn [_] False)) True))

(def sum-r
  (fn [f]
    (fn [n]
      (((Zero? n) 
          (delay
            n)) 
          (delay
            ((plus n) @((f f) (Dec n))))))))

(defn sum [n] 
  @((sum-r sum-r) n))

(to-normal-num (sum church-2)) ; must return 3

(test-sum sum)

;;; Implement set of function to create/manipulate lists.
;;; Your need to implement following functions:
;;; empty? - checks if list is empty, returns true or false. see church booleans http://en.wikipedia.org/wiki/Church_encoding#Church_booleans
;;; empty-list - used as "end" of the list.
;;; head - returns head of a list
;;; tail - returns tail of a list
;;; cons - takes 2 arguments h and t, and creates a list such that (head (cons a b)) = a, (tail (cons a b)) = b
;;;
;;; Help: http://en.wikipedia.org/wiki/Church_encoding#List_encodings


;;; node : [ (type | element) | next-node]]

(def minus
  (fn [a]
    (fn [b]
      ((b dec) a))))

(def eq
  (fn [a]
    (fn [b]
      (Zero? ((minus a) b)))))

(def nil-type (to-church-num 0))
(def node-type (to-church-num 1))

(defn empty? [x]
  ((eq ((x First) First)) nil-type))

(defn make-node [a]
  (make-pair 
     ((make-pair 
        node-type) 
        a)))

(def empty-list 
  ((make-pair 
     ((make-pair 
        nil-type) 
        nil))
     nil))

(defn head [l]
  ((l First) Second))

(defn tail [l]
  (((l Second) First) Second))

(def cons 
  (fn [a]
    (fn [b]
      ((make-node a)
         ((make-node b)
            empty-list)))))

(println "\n")
(println (((empty? empty-list) true) false)) ; must return true

(println (head ((cons "Hello") empty-list))) ; must return "Hello"

(let [list ((cons "Hello") empty-list)
      t (tail list)]
  (println (((empty? t) true) false))) ; must return true

(test-list {:empty? empty?
            :empty-list empty-list
            :head head
            :tail tail
            :cons cons}) ; test your solution


;;; Additional task.
;;; Implement map and reduce functions for lambda lists.
;;; map takes 2 arguments: function and list
;;; reduce takes 3 arguments: function, init value and list

;;; node : [ (type | element) | next-node]]

(def map-r
  (fn [f]
    (fn [l]
      (fn [rf]
        (((empty? l)
            (delay l))
            (delay ((cons 
                      (f (head l))) 
                      @(((rf f) (tail l)) rf))))))))

(def mmap
  (fn [f]
    (fn [l]
      @(((map-r f) l) map-r))))

(def reduce-r
  (fn [f]
    (fn [v]
      (fn [l]
        (fn [rf]
          (((empty? l)
              (delay v))
              (delay @((((rf f) ((f v) (head l))) (tail l)) rf))))))))

(def mreduce
  (fn [f]
    (fn [v]
      (fn [l]
        @((((reduce-r f) v) l) reduce-r)))))

#_((mmap print) empty-list)
#_((mmap print) ((mmap print) l))

(test-map-reduce {:empty? empty?
                  :empty-list empty-list
                  :head head
                  :tail tail
                  :cons cons
                  :map mmap
                  :reduce mreduce})
