(ns jest.vehicle
  "Code for vehicle manipulation"
  (:use [jest.world :only [cell alter-cell coords all-cells]]
        [jest.world.building :only [vehicle-type spawn?]]
        [jest.world.path :only [in-paths out-paths from to path-type
                                vehicle->path path->duration path
                                opposite-dirs]]))

(defrecord Vehicle [id type coords entry-time entry-direction
                    exit-time exit-direction cargo state])

(def ^{:doc "Returns how much cargo each vehicle type can carry"}
  cargo-capacity {:truck 1
                  :train 5
                  :boat 10})

(defn cargo-color
  "Returns the color of the cargo a vehicle is carrying.
   This is a hue color code, as a Double."
  [v]
  (first (:cargo v)))
(defn cargo-count
  "Returns the amount of cargo a vehicle is carrying."
  [v]
  (or (second (:cargo v)) 0))

(defn vehicle->duration
  "Returns the amount of time the given vehicle should spend in a cell."
  [v]
  (path->duration (vehicle->path (:type v))))

(defn vehicles
  "Returns the vehicles on the given cell."
  [c]
  (:vehicles c))

(defn vehicle-cell
  "Returns the cell this vehicle is on"
  [v]
  (cell (:coords v)))

(def ^:private id->vehicle (ref {}))

(defn- add-to-vehicle-map [v]
  (alter id->vehicle assoc (:id v) v))

(defn- remove-from-vehicle-map [v]
  (alter id->vehicle dissoc (:id v)))

(defn vehicle
  "Returns the vehicle with the given id, or nil if there isn't any."
  ([id]
     (id->vehicle id))
  ([c id]
     (first (filter #(= id (:id %))
                    (:vehicles c)))))

(defn update-vehicle
  "Updates the vehicle with the given id.
   Function f is called with the vVhicle record and the given args as arguments.
   The result of this function should be the new Vehicle record."
  [id f & args]
  (let [v (vehicle id)
        nv (apply f v args)]
    (alter-cell (vehicle-cell v)
                update-in [:vehicles] #(conj (disj % v)
                                             nv))
    (add-to-vehicle-map nv)
    nv))

(defn vehicle-state-change
  "Updates the vehicle with the given id to the given state.
   State should be a keyword, one of :spawning, :despawning,
   :moving, :exploding"
  [id state]
  (update-vehicle id assoc :state state))

(defn load-vehicle
  "Loads a vehicle on the given cell"
  [c v]
  (let [v (assoc v :coords (coords c))]
    (alter-cell c
                update-in [:vehicles] conj v)
    (add-to-vehicle-map v)
    v))

;;TODO: inconsistent. load-vehicle does not have a dosync, unload-vehicle does.
(defn unload-vehicle
  "Unloads the given vehicle"
  [v]
  {:pre [(some (partial = v) (vehicles (vehicle-cell v)))]}
  (dosync
   (alter-cell (vehicle-cell v)
               update-in [:vehicles] disj v)
   (remove-from-vehicle-map v))
  (assoc v :coords nil))


(defn- =state
  "Compares the state of the vehicle with the given id to the given state."
  [v state]
  (= (:state v)
     state))

(defn spawning?
  "Returns true iff the vehicle with the given id is in the spawning state."
  [v]
  (or (=state v :spawning)
      (=state v :spawning-exploding)))

(defn moving?
  "Returns true iff the vehicle with the given id is in the spawning state."
  [v]
  (=state v :moving))

(defn despawning?
  "Returns true iff the vehicle with the given id is in the despawning state."
  [v]
  (=state v :despawning))

(defn exploding?
  "returns true iff the vehicle with the given id is in the exploding state."
  [v]
  (or (=state v :exploding)
      (=state v :spawning-exploding)))

(defn cargo?
  "Returns true iff the given vehicle has cargo"
  [vehicle]
  (not (not (:cargo vehicle))))

(defn set-cargo
  "Updates the vehicle with the given id to have resource-count cargo of color
   resource-color, which should be a hue."
  [id resource-color resource-count]
  (update-vehicle id
                  assoc :cargo [resource-color resource-count]))

(defn clear-cargo
  "Updates the vehicle with the given id to no longer have any cargo."
  [id]
  (update-vehicle id
                  assoc :cargo nil))

(defn all-vehicles
  "Returns all vehicles in the game."
  ([]
     (dosync
      (remove nil? (flatten (map (comp seq vehicles) (all-cells))))))
  ([p]
     (filter p (all-vehicles))))

(defn truck?
  "Is this a truck?"
  [t?]
  (if (map? t?)
    (= :truck (:type t?))
    false))

(defn boat?
  "Is this a boat?"
  [t?]
  (if (map? t?)
    (= :boat (:type t?))
    false))

(defn train?
  "Is this a train?"
  [t?]
  (if (map? t?)
    (= :train (:type t?))
    false))
