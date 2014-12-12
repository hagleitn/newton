(ns newton.examples
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]
	      [physical-object :as obj]
	      [collision :as coll]
	      [gravity :as gr]
	      [spring :as sp]
	      [lorentz :as lz]
	      [pendulum :as pd]
	      [simulation :as sim]))

(import 'newton.physical_object.O)
(import 'newton.simulation.Simulation)

;; 1 au=149597870.700 km, 1 day=86400.0 s
;; 1 au 1.495978707e+8 km
(defn- km3-to-au3 [x]
  (* x (Math/pow 1.495978707e+8 3)))

(defn- km3:s2-to-au3:d2 [x]
  (* x (/ (Math/pow (double 86400.0) 2)
          (km3-to-au3 1.0))))

(defn- scale-density [m d]
  (let [volume (/ m d)]
    (/ 600 (Math/log (+ 1 (* 1e14 volume))))))

;; the following scale density produces to scale radii
;; (defn- scale-density [m d]
;;   (let [volume (/ m d)]
;;     (/ 1 volume)))

(def ^:private solar-system-state
  [;; units: au, day, kg
   ;; all data from jpl horizon
   ;; mass is set to 1 to make the calculations work (force is
   ;; computed with GM not m)
   ;; a conversion is applied to density to make it visible in the
   ;; projection, i.e: planets are displayed much larger than the
   ;; really are (also scale is logarithmic)

   ;; sun
   (O. 1 ;;1.988544e+30
       (scale-density 1.988544e+30 (km3-to-au3 1.408e+12))
       (km3:s2-to-au3:d2 1.3271244004193938e+11)
       1
       [2.731376257592918E-03 -9.943259137166194E-04 -1.345654880554433E-04]
       [4.150207197134728E-06 5.073619496266548E-06 -9.948725108893578E-08]
       zero)
   ;; mercury
   (O. 1 ;;3.302e+23
       (scale-density 3.302e+23 (km3-to-au3 5.427e+12))
       (km3:s2-to-au3:d2 22032.09)
       1
       [-1.812479222360599E-01 -4.272467614524213E-01 -1.808319050763030E-02]
       [2.017582901645816E-02 -9.765331659489960E-03 -2.649114276029301E-03]
       zero)
   ;; venus
   (O. 1 ;;48.685e+23
       (scale-density 48.685e+23 (km3-to-au3 5.204e+12))
       (km3:s2-to-au3:d2 324858.63)
       1
       [8.385794145724999E-02 -7.236404868356967E-01 -1.472053377579182E-02]
       [1.996816474521256E-02  2.190543330818498E-03 -1.122252717791348E-03]
       zero)
   ;; earth
   (O. 1 ;;5.97219e+24
       (scale-density 5.97219e+24 (km3-to-au3 5.515e+12))
       (km3:s2-to-au3:d2 398600.44)
       1
       [2.982311615841006E-01  9.391826476961919E-01 -1.645413417545258E-04]
       [-1.668259804825059E-02  5.094427807190187E-03  3.086735519432697E-07]
       zero)
   ;; mars
   (O. 1 ;;6.4185e+23
       (scale-density 6.4185e+23 (km3-to-au3 3.933e+12))
       (km3:s2-to-au3:d2 42828.3)
       1
       [1.215199342225740E+00 -6.619427555871434E-01 -4.374266381877291E-02]
       [7.237192376911529E-03  1.348936825874524E-02  1.049036015482519E-04]
       zero)
   ;; jupiter
   (O. 1 ;;1.89813e+27
       (scale-density 1.89813e+27 (km3-to-au3 1.326e+12))
       (km3:s2-to-au3:d2 126686511)
       1
       [-3.576323864846854E+00  3.923700125497625E+00  6.365288963929681E-02]
       [-5.667682356877504E-03 -4.727009078141517E-03  1.464765007724239E-04]
       zero)
   ;; saturn
   (O. 1 ;;5.68319e+26
       (scale-density 5.68319e+26 (km3-to-au3 0.687e+12))
       (km3:s2-to-au3:d2 37931207.8)
       1
       [-5.522932249749894E+00 -8.266909297662833E+00  3.635449694735542E-01]
       [4.334845698080518E-03 -3.114558888445349E-03 -1.184443805064603E-04]
       zero)
   ;; uranus
   (O. 1 ;;86.8103e+24
       (scale-density 86.8103e+24 (km3-to-au3 1.318e+12))
       (km3:s2-to-au3:d2 5793966)
       1
       [1.933495367124339E+01  5.152915530786324E+00 -2.313522672081139E-01]
       [-1.041543405113593E-03  3.617101076905490E-03  2.682827424207247E-05]
       zero)
   ;; neptune
   (O. 1 ;;102.41e+24
       (scale-density 102.41e+24 (km3-to-au3 1.638e+12))
       (km3:s2-to-au3:d2 6835107)
       1
       [2.749664519870901E+01 -1.192207246787463E+01 -3.881752810268023E-01]
       [1.228333635751891E-03  2.898847749525875E-03 -8.820558660834556E-05]
       zero)
   ;; pluto
   (O. 1;;1.307e+22
       (scale-density 1.307e+22 (km3-to-au3 1.83e+12))
       (km3:s2-to-au3:d2 872.4)
       1
       [7.316338035124889E+00 -3.192166311213740E+01  1.299487963029723E+00]
       [3.112910157604230E-03  5.842693178363616E-05 -8.995790021635178E-04]
       zero)
   ])

(def ^:private pendulum-single-state [(O. 1 100 0 1 [1 0 0] zero zero)])
(def ^:private pendulum-dual-state [(O. 1 100 0 1 [1 0 0] zero zero)
                                    (O. 1 100 0 1 [-1 0 0] zero zero)])
(def ^:private exec-toy-state [(O. 1 100 0 1 [3 0 0] zero zero)
                               (O. 1 100 0 1 [0 0 -3] zero zero)])
(def ^:private lorentz-state [(O. 1 100 0 -5 [0 0 0] [0 2 0] zero)])
(def ^:private lorentz-wire [(O. 1 100 0 -5 [0 -1 0] [0 2 0] zero)])

(def ^:private random-gravity
  (Simulation. "random-gravity" [0 0 -4] id
               300 (random-state 50) gravity-force-si
               inelastic-collision 0.005 1 1000 1200 800 1))
(def ^:private single-spring
  (Simulation. "single-spring" [0 0 -3] id
               800 pendulum-single-state
               #(spring-force %1 %2 %3 %4 10)
               inelastic-collision 0.005 1 300 1200 800 1))
(def ^:private random-spring
  (Simulation. "random-spring" [0 0 -4] id
               600 (random-state 20)
               #(spring-force %1 %2 %3 %4 1e9)
               elastic-collision 0.01 1 1000 1200 800 1))
(def ^:private solar-system
  (Simulation. "solar-system" [0 0 -2000] id
               60000 solar-system-state
               gravity-force-solar
               inelastic-collision 0.01 1 1000 1200 800 5))
(def ^:private single-pendulum
  (Simulation. "single-pendulum" [0 1 -4] [i k (s* -1 j)]
               700 pendulum-single-state
               #(pendulum-force %1 %2 %3 %4 zero)
               inelastic-collision 0.001 1 1000 1200 800 1))
(def ^:private random-pendulum
  (Simulation. "random-pendulum" [0 1 -4] [i k (s* -1 j)]
               400 (random-state-no-speed 20)
               #(pendulum-force %1 %2 %3 %4 zero)
               elastic-collision 0.005 1 1000 1200 800 1))
(def ^:private colliding-pendulum
  (Simulation. "colliding-pendulum" [0 1 -4] [i k (s* -1 j)]
               700 pendulum-dual-state
               #(pendulum-force %1 %2 %3 %4 zero)
               elastic-collision 0.001 1 1000 1200 800 1))
(def ^:private exec-toy
  (Simulation. "exec-toy" [0 3 -4] [i k (s* -1 j)]
               300 exec-toy-state
               #(pendulum-force %1 %2 %3 %4 zero)
               elastic-collision 0.001 1 1000 1200 800 1))
(def ^:private single-lorentz
  (Simulation. "single-lorentz" [5 0 -4] id
               300 lorentz-state
               #(lorentz-force %1 %2 %3 %4
                               (fn [s x t dt] (const-force s x t dt [0 0.3 0]))
                               (fn [s x t dt] (const-force s x t dt [0 0 1])))
               inelastic-collision 0.001 1 100 1200 800 1))
(def ^:private lorentz-wire
  (Simulation. "lorentz-wire" [0 0 -4] id
               300 (random-state 20)
               #(lorentz-force %1 %2 %3 %4
                               (fn [s x t dt] (const-force s x t dt zero))
                               (fn [s x t dt] (wire-field s x t dt i zero)))
               inelastic-collision 0.005 1 1000 1200 800 1))

(def sims
  {"random-gravity" random-gravity
   "single-spring" single-spring
   "random-spring" random-spring
   "solar-system" solar-system
   "single-pendulum" single-pendulum
   "colliding-pendulum" colliding-pendulum
   "random-pendulum" random-pendulum
   "exec-toy" exec-toy
   "single-lorentz" single-lorentz
   "lorentz-wire" lorentz-wire})
