(ns jest.behavior.movement
  "Vehicle movement actions. This includes picking up and dropping off cargo."
  (:use [jest.world.vehicle :only [vehicle vehicle-cell cargo-color vehicles
                             vehicle-state-change update-vehicle unload-vehicle
                             vehicle->duration cargo? set-cargo cargo-capacity
                             cargo-count clear-cargo load-vehicle despawning?
                             exploding? moving? spawning? map->Vehicle]]
        [jest.color :only [<=delta? hue hue-matches?]]
        [jest.world :only [cell alter-cell coords]]
        [jest.world.path :only [out-paths path->duration vehicle->path
                                opposite-dirs path path-type to
                                out-path?]]
        [jest.world.building :only [spawn? vehicle-type resource-color
                                    resource-count reduce-resource mix-colors
                                    supply? mixer? depot? all-spawns
                                    spawning-spawners
                                    dropoff-resource all-depots-filled?]]
        [jest.scheduler :only [schedule offset game-time]]
        [jest.score :only [score-vehicle]])
  (:require [jest.util :as util]
            [jest.behavior.routing :refer [update-preferred-path select-exit]]
            [jest.behavior.callback :as callback]))

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

(defn- vehicle-enter
  "Returns a Vehicle record that is the vehicle record with an entry time and
   entry direction attached to it. Both are based on the exit values for the
   given vehicle."
  [v]
  (assoc v
    :entry-time (:exit-time v)
    :entry-direction (opposite-dirs (:exit-direction v))))

(defn- vehicle-clear-exit
  "Returns a Vehicle record with the exit information cleared."
  [v]
  (assoc v :exit-direction nil))

(defn set-end-state [id state]
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

(defn vehicle-transition-state-dispatch
  "Dispatch function for the vehicle-transition-state multimethod.
   Dispatch happens on [cargo? cell-type]."
  [id]
  (let [vehicle (vehicle id)]
    [(cargo? vehicle)
     (:building-type (vehicle-cell vehicle))]))


(defmulti vehicle-transition-state
  "Does the required work for a vehicle upon moving into a cell.
   Dispatch happens on [cargo? cell-type]."
  vehicle-transition-state-dispatch)

(defmethod vehicle-transition-state :default
  [id])

(defmethod vehicle-transition-state
  [false :spawn]
  [id]
  (start-despawning id))

(defmethod vehicle-transition-state
  [true :spawn]
  [id]
  (start-despawning id))

(defn resource-hue [cell]
  (hue (:resource-type cell)))

(defn half-duration [v]
  (/ (vehicle->duration v) 2))

(defn schedule-half-duration [v f]
  (schedule #(dosync (f))
            (+ (:entry-time v)
               (/ (vehicle->duration v) 2))))

(defn maybe-explode [id]
  (when-not (or (:exit-direction (vehicle id))
                (spawn? (vehicle-cell (vehicle id))))
    (start-exploding id)))

(defn update-vehicle-exit [id]
  (update-vehicle id update-preferred-path)
  (maybe-explode id))

(defmethod vehicle-transition-state
  [false :supply]
  [id]
  (schedule-half-duration (vehicle id)
                          (fn []
                            (set-cargo id
                                       (resource-hue (vehicle-cell (vehicle id)))
                                       (cargo-capacity (:type (vehicle id))))
                            (update-vehicle-exit id)
                            (score-vehicle :get-cargo (vehicle id)))))

(defmethod vehicle-transition-state
  [false :mixer]
  [id]
  (schedule-half-duration (vehicle id)
                          #(let [color (resource-color (vehicle-cell
                                                        (vehicle id)))
                                 pickup-count (min (resource-count
                                                    (vehicle-cell (vehicle id)))
                                                   (cargo-capacity
                                                    (:type (vehicle id))))]
                             (reduce-resource (vehicle-cell (vehicle id))
                                              pickup-count)
                             (set-cargo id color pickup-count)
                             (update-vehicle-exit id))))


(defmethod vehicle-transition-state
  [true :mixer]
  [id]
  (schedule-half-duration (vehicle id)
                          (fn []
                            (mix-colors (vehicle-cell (vehicle id))
                                        (cargo-color (vehicle id))
                                        (cargo-count (vehicle id)))
                            (println (resource-color (vehicle-cell (vehicle id))))
                            (clear-cargo id)
                            (update-vehicle-exit id))))

(defmethod vehicle-transition-state
  [true :depot]
  [id]
  (let [v (vehicle id)
        c (vehicle-cell v)]
    (schedule-half-duration
     v
     #(when (hue-matches? (cargo-color v)
                          (hue (:resource-type c)))
        (score-vehicle :drop-cargo v)
        (dropoff-resource c (cargo-count v))
        (clear-cargo id)
        (update-vehicle-exit id)
        (if (all-depots-filled?)
          (callback/on-done))))))

;;BIG FAT TODO update-preferred-path does double work now
;;reason to do preferred path last is cause the vehicle might have picked something up
;;which alters routing decisions. so do that last!
(defn move-vehicle
  "Moves the vehicle with the given id in the given direction. This will also
   perform any actions required for this vehicle on the given cell, such as
   picking up and dropping off cargo."
  [id direction]
  {:pre [(let [v (vehicle id)
               path (path (vehicle-cell v) direction)]
           (and (= (:inout path) :out)
                (= (path-type path)
                   (vehicle->path (:type (vehicle id))))))]}
  (let [v (vehicle id)
        path (path (vehicle-cell v)
                   direction)
        incoming (filter #(and (incoming? %)
                               (= (:type v)
                                  (:type %)))
                         (vehicles (to path)))]
    (dosync
     (unload-vehicle v)
     (load-vehicle (to path) v)
     (vehicle-state-change id :moving)
     (update-vehicle id (comp select-exit vehicle-enter))
     (vehicle-transition-state id)
     (update-vehicle-exit id)

     (when (seq incoming)
       (start-exploding id)
       (doseq [v incoming]
         (start-exploding (:id v)))))))

(defn schedule-move
  "Schedules the next move for the vehicle with the given id. If the vehicle has
   entered a spawn point, no move is scheduled."
  [id]
  (schedule (fn []
              (dosync
               (if (not (:exit-direction (vehicle id)))
                 (unload-vehicle (vehicle id))
                 (do
                   (move-vehicle id (:exit-direction (vehicle id)))
                   (schedule-move id)))))
            (:exit-time (vehicle id))))

(defn valid-out-direction? [v dir]
  (let [path (path (vehicle-cell v) dir)]
    (and path
         (out-path? path)
         (= (:type path) (vehicle->path (:type v))))))

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

(defn pickup-color [v]
  (let [c (vehicle-cell v)]
    (cond
     (supply? c) (hue (:resource-type c))
     (mixer? c) (if (cargo-color v)
                  nil
                  (resource-color c))
     (depot? c) nil
     :default (cargo-color v))))
