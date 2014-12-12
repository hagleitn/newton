(ns newton.gravity
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]))

(defn- gravity-single-object-si [m1 m2 x y]
  (let [r (la/- y x)
        rlen (|| r)
        factor (Math/pow rlen -3)
        G 6.673e-11]
    (if (== rlen 0)
      zero
      (s* (* G m1 m2 factor) r))))

(defn gravity-force-si [s x d dt]
  "m1*m2*G/r^2 applied to all pairs of objects in s"
  (let [xcoos (:x x)
        xm (:m x)
        force #(gravity-single-object-si xm (:m %) xcoos (:x %))]
    (reduce la/+ zero (map force s))))

(defn- gravity-single-object-solar [gm-y x y]
  (let [r (la/- y x)
        rlen (|| r)
        factor (Math/pow rlen -3)]
    (if (== rlen 0) zero (s* (* gm-y factor) r))))

(defn gravity-force-solar [s x t dt]
  "m1*GM/r^2 applied to all pairs of objects in s"
  (let [xcoos (:x x)
        force #(gravity-single-object-solar (:gm %) xcoos (:x %))]
    (reduce la/+ zero (map force s))))
