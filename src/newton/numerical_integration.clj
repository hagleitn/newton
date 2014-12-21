(ns newton.numerical-integration
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]
	      [physical-object :as obj]
	      [collision :as coll]))
(import 'newton.physical_object.O)

(defn- step-integ [x v dt] (la/+ x (s* dt v)))
(defn- accel [f m] (s* (/ 1 m) f))

(defn next-state [state force update-collided t dt]
  "Computes the next state of the system by integrating force
  numerically by dt. After integration any collisions will be passed
  to the 'update-collided' function."
  (let [ms (map :m state)
        ds (map :d state)
        xs (map :x state)
        vs (map :v state)
	qs (map :q state)
        gms (map :gm state)
        as (pmap #(accel (force state % t dt) (:m %)) state)
        nvs (map #(step-integ %1 %2 dt) vs as)
        nxs (map #(step-integ %1 %2 dt) xs vs)
	nstate (map #(O. %1 %2 %3 %4 %5 %6 %7) ms ds gms qs nxs nvs as)]
    (handle-collision nstate update-collided)))
