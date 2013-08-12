(ns jest.behavior.state
  (:require [jest.world.vehicle :refer [vehicle->duration vehicle-state-change update-vehicle vehicle-clear-exit vehicle-cell vehicle spawning? vehicles]]
            [jest.world.building :refer [spawn?]]
            [jest.world.path :refer [vehicle->path path]]
            [jest.scheduler :refer [game-time]]
            [jest.score :refer [score-vehicle]]
            [jest.behavior.routing :refer [update-preferred-path]]))

(defn vehicle-state-in-cell [v]
  (let [halfpoint (+ (:entry-time v)
                     (/ (vehicle->duration v) 2))]
    (if (< @game-time halfpoint)
      :incoming
      :outgoing)))

(defn incoming? [v]
  (= (vehicle-state-in-cell v) :incoming))

(defn outgoing? [v]
  (= (vehicle-state-in-cell v) :outgoing))

(defn- set-end-state [id state]
  (dosync
   (vehicle-state-change id state)
   (update-vehicle id vehicle-clear-exit)))

(defn start-despawning
  "Modifies the state of the vehicle with the given id to :despawning, and
   schedules removal from the map."
  [id]
  {:pre [(spawn? (vehicle-cell (vehicle id)))]}
  (dosync
   (score-vehicle :despawn (vehicle id))
   (set-end-state id :despawning)))

(defn start-exploding
  "Modifies the state of the vehicle with the given id to :exploding."
  [id]
  (if (spawning? (vehicle id))
    (set-end-state id :spawning-exploding)
    (set-end-state id :exploding)))

(defn- maybe-explode [id]
  (when-not (or (:exit-direction (vehicle id))
                (spawn? (vehicle-cell (vehicle id))))
    (start-exploding id)))

(defn update-vehicle-exit [id]
  (update-vehicle id update-preferred-path)
  (maybe-explode id))


(defn update-vehicles-for-cell-changes
  "Ensures all vehicles on a particular cell are in a consistent state with the
paths and routes on this cell. If no exit exists for this path anymore, explode.
This function should be called from within a transaction."
  [c]
  (let [vs (vehicles c)
        incoming (filter incoming? vs)
        outgoing (filter outgoing? vs)]
    (doseq [{:keys [id]} incoming]
      (update-vehicle-exit id))

    (doseq [{:keys [id type exit-direction] :as v} outgoing]
      (when-not (= (vehicle->path type) (:type (path (vehicle-cell v) exit-direction)))
        (start-exploding id)))))
