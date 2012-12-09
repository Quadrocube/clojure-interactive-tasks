(ns derivative.core
  (:use [incanter.charts :only (add-function function-plot)]
        [incanter.core :only (view sin cos log)]))

(defn functions-plot [min-x max-x f & fns]
  (reduce #(add-function % %2 min-x max-x)
          (function-plot f min-x max-x)
          fns))

(def ^:dynamic x)

(defn calc-expr [expr x-value]
  (binding [x x-value]
    (eval expr)))

(defn expr-plot [min-x max-x & fns]
  (apply functions-plot min-x max-x (map #(partial calc-expr %) fns)))



;;; Task is to implement derivative function.
;;; It takes expression - valid clojure form like (+ x x) or (* 5 (+ x (* 7 x))) and returns derivative of this expression.
;;; All functions in expression are binary or unary, so (* x x x) is not allowed. (* (* x x) x) will be passed instead.
;;; derivative should support following functions: +, -, *, / - binary, sin, cos, log - unary.
;;; It must return valid clojure expression.
;;; Examples:
;;; (derivative '(+ x x)) => 2 or '(+ 1 1)
;;; (derivative '(sin (* 2 x))) => (* 2 (cos (* 2 x))) or it's equilavent.

(defmulti derivative 
  (fn [l] (if (sequential? l)
              (first l)
              l)))

(defmethod derivative 'x [_]
  1)

(defmethod derivative '+ [[_ u v]]
  (list '+
        (derivative u)
        (derivative v)))

(defmethod derivative '- [[_ u v]]
  (list '-
        (derivative u)
        (derivative v)))

(defmethod derivative '* [[_ u v]]
  (list '+
        (list '* u (derivative v))
        (list '* v (derivative u))))

(defmethod derivative '/ [[_ u v]]
  (list  '/
        (list '- 
              (list '* (derivative u) v)
              (list '* (derivative v) u))
        (list '* v v)))

(defmethod derivative 'log [[_ u]]
  (list '/
        (derivative u)
        u))

(defmethod derivative 'sin [[_ u]]
  (list '*
        (list 'cos u)
        (derivative u)))

(defmethod derivative 'cos [[_ u]]
  (list '*
        -1
        (list '*
              (list 'sin u)
              (derivative u))))

(defmethod derivative :default [_]
  0)

;;; Tests.
;;; Uncomment and try every function.

(defn test-simple []
  "f = x^2"
  (let [f '(* x x)]
    (view (expr-plot -10 10 f (derivative f)))))

;(test-simple)

(defn test-second-derivative []
  "f = x^2"
  (let [f '(* x x)]
    (view (expr-plot -10 10 f (derivative f) (derivative (derivative f))))))

;(test-second-derivative)

(defn test-complex []
  "f = -7x^2 + 2x + 5"
  (let [f '(+ (+ (* -7 (* x x))
                 (* 2 x))
              5)]
    (view (expr-plot -5 5 f (derivative f) (derivative (derivative f))))))

;(test-complex)

(defn test-division []
  "f = 1/(1 + x^2"
  (let [f '(/ 1 (+ (* x x)
                   1))]
    (view (expr-plot -4 4 f (derivative f)))))

;(test-division)

(defn test-trigonometry []
  "f = sin(2x)"
  (let [f '(sin (* 2 x))]
    (view (expr-plot (- Math/PI) Math/PI  f (derivative f) (derivative (derivative f))))))

;(test-trigonometry)

(defn test-logarithm []
  "f = log(1 + x^2)"
  (let [f '(log (+ 1 (* x x)))]
    (view (expr-plot -4 4 f (derivative f)))))

(test-logarithm)
