(ns newton.lorentz
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]))

(defn const-force [s x t dt c] c)

(defn wire-field [s x t dt direction position]
  "magnetic field is circular around direction (moved by position)"
  (let [dist (la/- (:x x) position)
        b (la/x dist direction)
        r (|| (la/- dist (proj dist direction)))]
    (if (== 0 (|| b)) zero (s* (/ 1 (+ 1 r)) (norm b)))))

(defn lorentz-force [s x t dt E B]
  "q(E + v x B)"
  (s* (:q x) (la/+ (E s x t dt) (la/x (:v x) (B s x t dt)))))
