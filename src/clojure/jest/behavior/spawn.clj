(ns jest.behavior.spawn
  (:require [jest.world :refer [coords cell]]
            [jest.world.building :refer [spawning-spawners spawn? vehicle-type]]
            [jest.world.vehicle :refer [vehicle-state-change load-vehicle
                                        map->Vehicle vehicle]]
            [jest.scheduler :refer [schedule offset game-time]]
            [jest.behavior.movement :refer [schedule-move]]
            [jest.behavior.routing :refer [select-exit]]
            [jest.score :refer [score-vehicle]]))

(defonce ^:private idc (atom 0))

(defn- next-idc
  "Generates a unique id using a counter."
  []
  (swap! idc inc))

(defn- load-vehicle-on-spawn
  "Loads a vehicle on a spawn point, setting all initial state."
  [c]
  {:pre [(spawn? c)]}
  (dosync
   (let [id (next-idc)]
     (load-vehicle c (select-exit (map->Vehicle
                                   {:id id
                                    :type (vehicle-type c)
                                    :coords (coords c)
                                    :entry-time @game-time
                                    :state :spawning})))

     (when-not (:exit-direction (vehicle id))
       (vehicle-state-change id :spawning-exploding))
     (vehicle id))))

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
