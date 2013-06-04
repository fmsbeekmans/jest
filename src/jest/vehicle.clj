(ns jest.vehicle
  (:use [jest.world.cell :only [cell alter-cell coords]]
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
(declare schedule-move)

(defn spawn
"Spawns a vehicle on the given cell."
([c]
 {:pre [(spawn? c)]}
 (dosync
  (let [vehicle (spawn c (->Vehicle (vehicle-type c)
                                    (coords c)
                                    @game-time
                                    nil))]
    (schedule-move vehicle)
    vehicle)))
([c v]
 (dosync
  (alter-cell c
              update-in [:vehicles] conj v)
  v)))


(defn despawn
"Despawns the given vehicle"
[v]
{:pre [(some (partial = v) (vehicles (vehicle-cell v)))]}
(dosync
(alter-cell (vehicle-cell v)
           update-in [:vehicles] (partial remove (partial = v)))))

(defn preferred-path
  "select the preferred path for this vehicle"
  [v]
  (let [paths (out-paths (vehicle-cell v))]
    (rand-nth (filter #(= (path-type %) (vehicle->path (:type v))) paths))))

(defn- move-vehicle
  [v path]
  {:pre [(= (vehicle-cell v)
            (from path))]}
  (println "move")
  (dosync
   (despawn v)
   (when-not (spawn? (to path))
     (spawn (to path) (assoc v :coords (coords (to path)))))))

(defn schedule-move [v]
  (schedule (fn []
              (dosync
               (if-let [v (move-vehicle v (preferred-path v))]
                 (schedule-move v))))
            (jest.scheduler/delay 10 :seconds)))
  ; snelheid
  ; naar
  ; naar = spawn? => despawn
  ; schedule!
  ; schedule schedule-move!)

