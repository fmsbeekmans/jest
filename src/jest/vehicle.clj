(ns jest.vehicle
  (:use [jest.world.cell :only [cell alter-cell coords all-cells]]
        [jest.world.building :only [vehicle-type spawn?]]
        [jest.world.path :only [in-paths out-paths from to path-type vehicle->path path->duration path opposite-dirs]]
        [jest.scheduler :only [game-time schedule offset]]
        [jest.color :only [hue average-hue hue-difference +delta+]]))

(defrecord Vehicle [id type coords entry-time entry-direction exit-time exit-direction cargo state])

(def cargo-capacity {:truck 1
                     :train 5
                     :boat 10})

(defn cargo-color [v] (first (:cargo v)))
(defn cargo-count [v] (or (second (:cargo v)) 0))

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

(defn load-vehicle [c v]
  "Loads a vehicle on the given cell"
  (let [v (assoc v :coords (coords c))]
    (alter-cell c
                update-in [:vehicles] conj v)
    v))

(defn unload-vehicle
  "Unloads the given vehicle"
  [v]
  {:pre [(some (partial = v) (vehicles (vehicle-cell v)))]}
  (dosync
   (alter-cell (vehicle-cell v)
               update-in [:vehicles] disj v))
  (assoc v :coords nil))

(defn despawning? [id]
  (= (:state (vehicle id)) :despawning))

(defn cargo? [vehicle]
  (not (not (:cargo vehicle))))

(defn set-cargo
  [vehicle resource-color resource-count]
  (assoc vehicle :cargo [resource-color resource-count]))

(defn clear-cargo
  [vehicle]
  (assoc vehicle :cargo nil))

(defn resource-color [cell]
  (first (:resource cell)))

(defn resource-count [cell]
  (or
   (second (:resource cell))
   0))

(defn mix-colors [cell color magnitude]
  (let [[existing-color existing-magnitude] (or (:resource cell)
                                                [nil 0])
        new-color (apply average-hue (concat (repeat magnitude color)
                                   (repeat existing-magnitude existing-color)))
        new-magnitude (+ magnitude existing-magnitude)]
    (assoc cell :resource [new-color new-magnitude])))

(defn reduce-resource [cell amount]
  {:post [(or (nil? (:resource cell))
              (>= (resource-count cell) 0))]}
  (assoc cell :resource (if (= amount (resource-count cell))
                          nil
                          [(resource-color cell)
                           (- (resource-count cell)
                              amount)])))


(defn all-vehicles []
  (remove nil? (flatten (map (comp seq vehicles) (all-cells)))))
