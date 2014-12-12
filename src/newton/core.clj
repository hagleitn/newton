(ns newton.core
  (:gen-class :main :true))

(use '(newton [examples :as exam]
	      [simulation :as sim]))

(import 'newton.simulation.Simulation)

(defn -main [& args]
  (let [sim (get sims (first args))]
    (if (nil? sim) 
      (println "Available simulations: " (keys sims))
      (do (println "running " (first args)) (run-simulation sim)))))
