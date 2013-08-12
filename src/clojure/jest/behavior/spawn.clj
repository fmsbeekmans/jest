(ns jest.behavior.spawn
  (:require [jest.world :refer [coords cell]]
            [jest.world.building :refer [spawning-spawners spawn?]]
            [jest.scheduler :refer [schedule offset]]
            [jest.behavior.movement :refer [load-vehicle-on-spawn schedule-move]]
            [jest.score :refer [score-vehicle]]))

(defn spawn
  "Spawns a vehicle on the given cell."
  [c]
  {:pre [(spawn? c)]}
  (dosync
   (let [vehicle (load-vehicle-on-spawn c)]
     (schedule-move (:id vehicle))
     (score-vehicle :spawn vehicle)
     vehicle)))

(def active-spawners (atom #{}))

(defn- activate-spawner [s]
  (swap! active-spawners
         conj (coords s)))

(defn- deactivate-spawner [s]
  (swap! active-spawners
         disj (coords s)))

(defn active? [s]
  (@active-spawners (coords s)))


(defn start-spawning []
  (doseq [{:keys [coord spawn-rate spawn-offset] :as s} (spawning-spawners)]
    (println coord)
    (letfn [(spawn-and-reschedule [time]
              (let [s (cell coord)
                    time (+ time (:spawn-rate s))]
                (when (active? s)
                  (spawn s)
                  (schedule (partial spawn-and-reschedule time) time))))]
      (activate-spawner s)
      (let [time (offset (:spawn-offset s))]
        (schedule (partial spawn-and-reschedule time) time)))))

(defn stop-spawning []
  (reset! active-spawners #{}))
