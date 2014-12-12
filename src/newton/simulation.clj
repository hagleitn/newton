(ns newton.simulation
  (:gen-class))

(use '(newton [linear-algebra :as la :exclude (+ - *)]
              [projection :as pr]
              [rendering :as rend]
	      [buffer :as buf]))

(defrecord Simulation 
    [name ;; name of the simulation
     position ;; viewpoint of observer
     transformation ;; base transformation for observer
     distance ;; distance of projection plane from observer
     initial-state ;; starting state of the simulation
     force ;; force function (F = ma)
     update-collided ;; collision handler
     dt ;; time step for integration
     simul-factor ;; speed of simulation
     buffer-size ;; size of the buffer of coordinates
     width ;; width of the window
     height ;; height of the window
     speed-up ;; factor by which the animation differs from real time.
     ])

(defn run-simulation [sim]
  "Kicks off a simulation."
  (let [queue (java.util.concurrent.LinkedBlockingQueue. (:buffer-size sim))
        pos (:position sim)
	transform (:transformation sim)
	distance (:distance sim)
	state (:initial-state sim)
	iso (find-iso (la/* transform pos) state)
	proj #(project-object pos transform distance iso %1)
	width (:width sim)
	height (:height sim)
	dt (:dt sim)
	force (:force sim)
	update-collided (:update-collided sim)
	simul-fact (:simul-factor sim)
        speed-up (:speed-up sim)
        drop-rate (int (* speed-up simul-fact))]
    (do (draw queue width height dt simul-fact)
        (buffer-states queue state force update-collided proj dt drop-rate))))
