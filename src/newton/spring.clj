(ns newton.spring
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]))

(defn spring-force [s x t dt k]
  "-k*x"
  (s* (- k) (:x x)))
