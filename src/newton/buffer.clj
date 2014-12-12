(ns newton.buffer
  (:gen-class))

(use '(newton [numerical-integration :as ni]))

(defn buffer-states [queue start force update-collided proj dt simul-fact]
  "buffer-states will run the simulation (take the start state run it
  through numerical integration using force, update-collided, dt and
  simul-fac) and store the results after projecting the states onto
  the plane (with proj) into the queue." 
  (let [state (atom start)
        cnt (atom 0)
	t (atom (double 0))]
    (while [true]
      (do (when (== (mod @cnt simul-fact) 0)
            (.put queue (sort-by #(- (last (first %))) (map proj @state))))
          (swap! state #(next-state % force update-collided t dt))
          (swap! cnt inc)
          (swap! t #(+ %1 dt))))))
