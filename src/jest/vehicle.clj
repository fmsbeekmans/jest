(ns jest.vehicle
  (:use [jest.world.cell :only [cell alter-cell coords all-cells]]
        [jest.world.building :only [vehicle-type spawn?]]
        [jest.world.path :only [in-paths out-paths from to path-type vehicle->path]]
        [jest.scheduler :only [game-time schedule]]))

(defrecord Vehicle [type coords entry-time cargo])

(defn vehicles
"Returns the vehicles on the given cell."
[c]
(:vehicles c))

(defn vehicle-cell
"Returns the cell this vehicle is on"
[v]
(cell (:coords v)))

(defn- load-vehicle [c v]
  "Loads a vehicle on the given cell"
  (let [v (assoc v :coords (coords c))]
    (alter-cell c
              update-in [:vehicles] conj v)
    v))


(defn- unload-vehicle
  "Unloads the given vehicle"
  [v]
  {:pre [(some (partial = v) (vehicles (vehicle-cell v)))]}
  (dosync
   (alter-cell (vehicle-cell v)
               update-in [:vehicles] (partial remove (partial = v))))
  (assoc v :coords nil))

(defn preferred-path
  "select the preferred path for this vehicle"
  [v]
  (let [paths (out-paths (vehicle-cell v))]
    (rand-nth (filter #(= (path-type %) (vehicle->path (:type v))) paths))))

(defn- move-vehicle
  [v path]
  {:pre [(= (vehicle-cell v)
            (from path))]}
  (dosync
   (unload-vehicle v)
   (when-not (spawn? (to path))
     (load-vehicle (to path) v))))

(defn schedule-move [v]
  (schedule (fn []
              (dosync
               (if-let [v (move-vehicle v (preferred-path v))]
                 (schedule-move v))))
            (jest.scheduler/delay 10 :seconds)))

(defn spawn
"Spawns a vehicle on the given cell."
([c]
 {:pre [(spawn? c)]}
 (dosync
  (let [vehicle (load-vehicle c (->Vehicle (vehicle-type c)
                                           (coords c)
                                           @game-time
                                           nil))]
    (schedule-move vehicle)
    vehicle))))


(defn all-vehicles []
  (remove nil? (flatten (map vehicles (all-cells)))))
