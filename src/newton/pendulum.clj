(ns newton.pendulum
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]))

(defn pendulum-force [s x t dt pos]
  "Applies gravity (z-axis) and applies enough force to keep the
  object on a distance |x| from [0 0 0]."
  (let [g -9.8331
        x' (la/- (:x x) pos)
        f (s* (:m x) [0 0 g])
	pr (proj f x')
        f (la/- f pr)
	v' (proj (:v x) x')
	f'-perpendicular (s* (* -1 (/ (:m x) dt)) v')
	f-total (la/+ f'-perpendicular f)]
    f-total))
