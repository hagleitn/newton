(ns newton.linear-algebra
  (:gen-class))

(require '[clojure.core :as core :exclude (+ - *)])

(def i [1 0 0])
(def j [0 1 0])
(def k [0 0 1])
(def id [i j k])
(def zero [0 0 0])

(defn- zip-accum [f acc args]
  (if (not (empty? (first args)))
    (let [x (map first args)
          r (map rest args)
          acc (conj acc (apply f x))]
      (recur f acc r)) acc))

(defn zip [f & args]
  "takes n lists and computes one new list by applying function f to
  the n-th argument of each list."
  (zip-accum f [] args))

(defn + [x y]
  "vector addition"
  (zip core/+ x y))

(defn - [x y]
  "vector subtraction"
  (zip core/- x y))

(defn s* [a x]
  "scalar multiplication of a (scalar) and x (vector)"
  (map #(core/* a %) x))

(defn d* [x y]
  "dot product of x and y"
  (reduce core/+ 0 (zip core/* x y)))

(defn || [x]
  "length of vector x"
  (Math/sqrt (d* x x)))

(defn * [A x]
  "matrix multiplication A and x are assumed to be of the right shapes"
  (reduce + zero (zip s* x A)))

(defn proj [x y]
  "Projects x onto y"
  (s* (/ (d* x y) (d* y y)) y))

(defn norm [x]
  "normalized (len == 1) x"
  (s* (/ 1 (|| x)) x))

(defn x [u v]
  "cross product of u and v"
  (let [[u1 u2 u3] u
        [v1 v2 v3] v]
    [(core/- (core/* u2 v3) (core/* v2 u3))
     (core/- (core/* v1 u3) (core/* u1 v3))
     (core/- (core/* u1 v2) (core/* v1 u2))]))
