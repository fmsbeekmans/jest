(ns jest.vehicle
  (:use [jest.world.cell :only [cell alter-cell coords all-cells]]
        [jest.world.building :only [vehicle-type spawn?]]
        [jest.world.path :only [in-paths out-paths from to path-type vehicle->path path->duration]]
        [jest.scheduler :only [game-time schedule offset]]))

(defrecord Vehicle [id type coords entry-time entry-direction exit-time exit-direction cargo state])

(defn vehicles
"Returns the vehicles on the given cell."
[c]
(:vehicles c))

(defn vehicle-cell
"Returns the cell this vehicle is on"
[v]
(cell (:coords v)))

(defonce ^:private idc (atom 0))

(defn- next-idc []
  (swap! idc inc))

(defn vehicle
  ([id]
     (first (remove nil? (map #(vehicle % id)
                              (all-cells)))))
  ([c id]
     (contains-vehicle? c (:id id))
     (first (filter #(= id (:id %))
                    (:vehicles c)))))

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

(defn- select-exit [v]
  (assoc v
    :exit-time (+ (:entry-time v)
                  (path->duration (vehicle->path (:type v))))
    :exit-direction (:direction (preferred-path v))))

(defn- move-vehicle
  [v path]
  {:pre [(= (vehicle-cell v)
            (from path))]}
  (dosync
   (unload-vehicle v)
   (when-not (spawn? (to path))
     (load-vehicle (to path) v))))

(defn vehicle-state-change [c v state]
  (let [nv (assoc v :state state)]
    (update-in c [:vehicles]
               #(conj (disj % v)
                      nv))))

(defn- schedule-state-change [v state time]
  (schedule #(dosync (alter-cell (cell (:coords v))
                                 vehicle-state-change
                                 v
                                 state))
            time))

(defn- schedule-move [v]
  (schedule (fn []
              (dosync
               (if-let [v (move-vehicle v (preferred-path v))]
                 (schedule-move v))))
            (jest.scheduler/offset 10 :seconds)))

(defn spawn
  "Spawns a vehicle on the given cell."
  [c]
  {:pre [(spawn? c)]}
  (dosync
   (let [vehicle (load-vehicle c (select-exit (map->Vehicle
                                               {:id (next-idc)
                                                :type (vehicle-type c)
                                                :coords (coords c)
                                                :entry-time @game-time
                                                :state :spawning})))]
     (schedule-state-change vehicle :moving (offset 5))
     (schedule-move vehicle)
     vehicle)))


(defn all-vehicles []
  (remove nil? (flatten (map vehicles (all-cells)))))
