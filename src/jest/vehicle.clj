(ns jest.vehicle
  (:use [jest.world.cell :only [cell alter-cell coords all-cells]]
        [jest.world.building :only [vehicle-type spawn?]]
        [jest.world.path :only [in-paths out-paths from to path-type vehicle->path path->duration path opposite-dirs]]
        [jest.scheduler :only [game-time schedule offset]]
        [jest.color :only [hue average-hue hue-difference]]))

(defrecord Vehicle [id type coords entry-time entry-direction exit-time exit-direction cargo state])

(defn vehicle->duration [v]
  (path->duration (vehicle->path (:type v))))
  
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
     (first (filter #(= id (:id %))
                    (:vehicles c)))))

(defn update-vehicle
  [id f & args]
  (let [v (vehicle id)
        nv (apply f v args)]
    (alter-cell (vehicle-cell v)
                update-in [:vehicles] #(conj (disj % v)
                                             nv))))

(defn vehicle-state-change [id state]
  (update-vehicle id assoc :state state))

(defn- schedule-state-change [id state time]
  (schedule #(dosync (vehicle-state-change id state))
            time))

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
               update-in [:vehicles] disj v))
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

(defn- vehicle-enter [v]
  (assoc (select-exit v)
    :entry-time (:exit-time v)
    :entry-direction (opposite-dirs (:exit-direction v))))

(defn start-despawning [id]
  {:pre [(spawn? (vehicle-cell (vehicle id)))]}
  (vehicle-state-change id :despawning)
  (update-vehicle id
                  assoc
                  :exit-time nil
                  :exit-direction nil)
  (schedule #(unload-vehicle (vehicle id))
            (offset (/ (vehicle->duration (vehicle id))
                       2))))

(defn cargo? [vehicle]
  (not (not (:cargo vehicle))))

(defmulti vehicle-transition-state
  "Does the required work for a vehicle upon moving into a cell. dispatches on [state cargo? cell-type]"
  (fn vehicle-transition-state-dispatch
    [id]
    (let [vehicle (vehicle id)]
      [(:state vehicle)
       (not (not (:cargo vehicle)))
       (:building-type (vehicle-cell vehicle))])))

(defmethod vehicle-transition-state :default
  [id])

(defmethod vehicle-transition-state
  [:moving false :spawn]
  [id]
  (println "I should despawn!")
  (start-despawning id))

(defmethod vehicle-transition-state
  [:moving false :supply]
  [id]
  (println "I should pick up something!"))

(defmethod vehicle-transition-state
  [:moving true :mixer]
  [id]
  (println "I should drop off and mix something!"))

(defmethod vehicle-transition-state
  [:moving true :depot]
  [id]
  (println "I should drop off something!"))

(defn- move-vehicle
  [id direction]
  {:pre [(let [v (vehicle id)
               path (path (vehicle-cell v) direction)]
           (and (= (:inout path) :out)
                (= (path-type path)
                   (vehicle->path (:type (vehicle id))))))]}
  (let [v (vehicle id)
        path (path (vehicle-cell v)
                   direction)]
    (dosync
     (unload-vehicle v)
     (load-vehicle (to path) v)
     (update-vehicle id vehicle-enter)
     (vehicle-transition-state id))))

(defn- schedule-move [id]
  (schedule (fn []
              (dosync
               (move-vehicle id (:exit-direction (vehicle id)))
               (schedule-move id)))
            (offset (vehicle->duration (vehicle id)))))

(defn spawn
  "Spawns a vehicle on the given cell."
  [c]
  {:pre [(spawn? c)]}
  (let [vehicle (dosync (load-vehicle c (select-exit (map->Vehicle
                                                      {:id (next-idc)
                                                       :type (vehicle-type c)
                                                       :coords (coords c)
                                                       :entry-time @game-time
                                                       :state :spawning}))))]
    (schedule-state-change (:id vehicle) :moving (/ (vehicle->duration vehicle)
                                                    2))
    (schedule-move (:id vehicle))
    vehicle))


(defn all-vehicles []
  (remove nil? (flatten (map vehicles (all-cells)))))
