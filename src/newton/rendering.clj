(ns newton.rendering
  (:gen-class))

(defn- render-objects [g objs w h]
  (doseq [[[x y z] r [c1 c2 c3]] objs]
    (let [d (* r 2)
	  w' (/ w 2)
	  h' (/ h 2)]
      (doto g
        (.setColor (java.awt.Color. c1 c2 c3))
	(.fillOval (- (+ w' x) r) (- (+ h' y) r) d d)))))

(defn- render-buffering [g w h]
  (doto g
    (.setColor (java.awt.Color/white))
    (.drawString "Buffering ..." (/ w 2) (/ h 2))))

(defn draw [queue w h dt simul-fac]
  "Calling draw opens a window and starts a timer to periodically render a 
simulation state onto the window's canvas. The simulation state is taken
from the queue, the window will be of size w x h, and the timer will fire
every dt*simul-fact. The items in the queue are assumed to be projected
objects (i.e.: seq of [pos radius color])."
  (let [objs (atom [])
        wait (atom false)
        panel (doto 
                  (proxy [javax.swing.JPanel] []
                    (paintComponent [^java.awt.Graphics g]
                      (if (not (nil? @objs)) 
                        (render-objects g @objs w h) 
                        (render-buffering g w h))))
                (.setBackground (java.awt.Color/black))
                (.setPreferredSize (java.awt.Dimension. w h)))
        frame (doto (javax.swing.JFrame. "Newton")
                (.setContentPane panel)
                .pack
                (.setVisible true))]
    (doto (java.util.Timer.)
      (.scheduleAtFixedRate
       (doto (proxy [java.util.TimerTask] []
               (run [] 
                 (do (when (not @wait) (swap! objs (fn [x] (.poll queue))))
                     (when (nil? @objs) (swap! wait (fn [x] true)))
                     (when (and @wait (== 0 (.remainingCapacity queue))) 
                       (swap! wait (fn [x] false)))
                     (.repaint panel)))))
       0 (long (* simul-fac (* 1000 dt)))))))
