(ns newton.projection
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]
	      [physical-object :as obj]))

(defn- object-intensity [dist]
  (let [z (+ 1 dist)]
    (/ 1 (* z z))))

(defn- apply-iso [i iso]
  (let [[min max] iso
        exposure (float 
                  (+ 0.3 
                     (* 0.7 
                        (/ (- i min) 
                           (- max min)))))]
    (cond (< exposure 0) (float 0)
          (> exposure 1) (float 1)
          true exposure)))

(defn- apply-transform [pos transform o]
  (la/- (la/* transform (:x o)) pos))

(defn find-iso [pos os]
  "Computes range of intensities to map between 0-1 given the start
  state of the simulation."
  (let [d (map #(|| (la/- pos (:x %))) os)
        min-d (apply min d)
	max-d (apply max d)
	max-i (object-intensity min-d)
	min-i (object-intensity (* 2 max-d))]
    [min-i max-i]))

(defn project-object [pos transform distance iso o]
  "projects the object onto a plane. The observer is at located at
  'pos'. Transorm is a matrix applied before projecting (i.e.:
  direction of observer). Finally the plane will be at 'distance' away
  from the observer perpendicular to the z-axis."
  (let [x (apply-transform pos transform o)
        r (mass-to-radius o)
	fac (/ distance (last x))
	shade (apply-iso (object-intensity (|| x)) iso)]
    [[(* fac (first x)) (* fac (second x)) (last x)] (* fac r) [shade shade shade]]))
