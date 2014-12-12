(ns newton.collision
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]
	      [physical-object :as obj]))

(import 'newton.physical_object.O)

(defn- collided? [o1 o2]
  (let [dist (|| (la/- (:x o1) (:x o2)))
        rsum (reduce + 0 (map mass-to-radius [o1 o2]))]
    (< dist rsum)))

(defn- elastic-collision-compute-speed [o1 o2]
  (let [fac (/ (* 2 (:m o2)) (+ (:m o1) (:m o2)))
	v-diff (la/- (:v o1) (:v o2))
	x-diff (la/- (:x o1) (:x o2))]
    (la/- (:v o1) (s* fac (proj v-diff x-diff)))))

(defn- elastic-collision-two [o1 o2]
  (let [v1 (elastic-collision-compute-speed o1 o2)
        v2 (elastic-collision-compute-speed o2 o1)]
    [(assoc o1 :v v1) (assoc o2 :v v2)]))

(defn- elastic-collision-accum [acc o]
  (let [[o-prev os] acc
        [o-prev' o'] (elastic-collision-two o-prev o)]
    [o-prev' (conj os o')]))

(defn elastic-collision [o os]
  "returns an updated list of objects after an elastic collision
  between o and all os"
  (let [[o' os'] (reduce elastic-collision-accum [o []] os)]
    (conj os' o')))

(defn inelastic-collision [o os]
  "returns a single object (wrapped in a vector) of all objects merged
  in a completely inelastic collision"
  (let [os (conj os o)
        m (apply + (map :m os))
        gm (apply + (map :gm os))
	d (/ m (apply + (map #(/ (:m %) (:d %)) os)))
	mv (reduce la/+ zero (map #(s* (:m %) (:v %)) os))
	v (s* (/ 1 m) mv)
	mx (reduce la/+ zero (map #(s* (:m %) (:x %)) os))
	x (s* (/ 1 m) mx)
	ma (reduce la/+ zero (map #(s* (:m %) (:a %)) os ))
	a (s* (/ 1 m) ma)
	q (apply + (map :q os))]
    [(O. m d gm q x v a)]))

(defn- handle-collision-accum [s update-collided acc]
  (let [o (first s)
        os (rest s)
        gby (group-by #(collided? o %) os)
        h (get gby true)
	r (get gby false)
	acc (concat (update-collided o h) acc)]
    (if (empty? r) acc (recur r update-collided acc))))

(defn handle-collision [s update-collided]
  "Takes a list of objects and a function to handle collisions. All
  collisions between objects are computed and passed to the handler."
  (handle-collision-accum s update-collided ()))
