(ns newton.physical-object
  (:gen-class))

(use '[newton.linear-algebra :as la :exclude (+ - *)])

(defrecord O 
    [m  ;; mass
     d  ;; density
     gm ;; GM (gravity*mass)
     q  ;; charge
     x  ;; location
     v  ;; speed
     a  ;; accell
     ])

(defn mass-to-radius [o]
  "computes radius of object (using mass and density)"
  (let [m (:m o)
        d (:d o)
        volume (/ m d)
        c (/ 3 (* 5 Math/PI))]
    (Math/pow (* c volume) (/ 1 3))))

;; --- Helper functions to define random states ----

(defn random-coos [] (- (* 3 (Math/random)) 1.5))

(defn random-speed [] (- (* 1.2 (Math/random)) 0.6))

(defn random-mass [] (+ (* (- 1e9 1e7) (Math/random)) 1e7))

(defn random-charge [] (+ (* (- 1e9 1e7) (Math/random)) 1e7))

(defn random-state [n] 
  (for [x (range n)] 
    (O. (random-mass) 5e11 0 (random-charge)
        [(random-coos) (random-coos) (random-coos)]
        [(random-speed) (random-speed) (random-speed)]
        zero)))

(defn random-state-no-speed [n] 
  (for [x (range n)] 
    (O. 1 1000 0 (random-charge)
        [(random-coos) (random-coos) (random-coos)]
        zero zero)))
