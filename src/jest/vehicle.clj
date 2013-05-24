(ns jest.vehicle
  (:use [jest.world.cell :only [cell alter-cell coords]]
        [jest.world.building :only [vehicle-type spawn?]]
        [jest.scheduler :only [game-time]]))

(defrecord Vehicle [type coords entry-time cargo])

(defn vehicles
  "Returns the vehicles on the given cell."
  [c]
  (:vehicles c))

(defn vehicle-cell
  "Returns the cell this vehicle is on"
  [v]
  (cell (:coords v)))

(defn spawn
  "Spawns a vehicle on the given cell."
  [c]
  {:pre [(spawn? c)]}
  (dosync
   (alter-cell c
               update-in [:vehicles] conj (->Vehicle (vehicle-type c)
                                                     (coords c)
                                                     @game-time
                                                     nil))))

(defn unspawn
  "Unspawns the given vehicle"
  [v]
  {:pre [(some (partial = v) (vehicles (vehicle-cell v)))]}
  (dosync
   (alter-cell (vehicle-cell v)
               update-in [:vehicles] (partial remove (partial = v)))))
