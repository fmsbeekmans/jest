(ns jest.movement
  "Vehicle movement actions. This includes picking up and dropping off cargo."
  (:use [jest.vehicle :only [vehicle vehicle-cell cargo-color
                             vehicle-state-change update-vehicle unload-vehicle
                             vehicle->duration cargo? set-cargo cargo-capacity
                             cargo-count clear-cargo load-vehicle despawning?
                             exploding? map->Vehicle]]
        [jest.color :only [hue-difference <=delta? hue]]
        [jest.world :only [alter-cell coords]]
        [jest.world.path :only [out-paths path->duration vehicle->path
                                opposite-dirs path path-type to]]
        [jest.world.building :only [spawn? vehicle-type resource-color
                                    resource-count reduce-resource mix-colors]]
        [jest.scheduler :only [schedule offset game-time]]))

(defonce ^:private idc (atom 0))

(defn- next-idc
  "Generates a unique id using a counter."
  []
  (swap! idc inc))

(defn- dir-num
  "Maps a direction to a number, going clockwise from :north = 0."
  [dir]
  (.indexOf [:north :east :south :west] dir))

(defn route-score
  "Returns the lowest hue difference with the given color among the routes for
   the given path. If there are no routes, route-score returns nil. A lower
   route score is better."
  [color path]
  (if color
    (let [hue-diffs  (map (partial hue-difference color)
                          (:routes path))]
      (if (seq hue-diffs)
        (apply min hue-diffs)))))

(defn- dir-sort-fn
  "Sorter function for paths. Returns true if p1 should go before p2, false
   otherwise. Sorting happens based on clockwise direction, with :north first."
  [p1 p2]
  (<= (dir-num (:direction p1))
      (dir-num (:direction p2))))

(defn best-route
  "Returns the path that best matches the given color. This is either a path
   with a route close to the given color, or the first path in clockwise
   order starting from :north."
  [color paths]
  (let [route-scores (remove (comp nil? second) (map #(vector %
                                                   (route-score color %))
                                          paths))
        sorted-routes (sort #(let [s1 (second %1)
                                   s2 (second %2)]
                               (if (= s1 s2)
                                 (dir-sort-fn %1 %2)
                                 (< s1 s2)))
                            route-scores)]
    (first sorted-routes)))

;; if there are routes, vehicle carries cargo, and best matching route is less
;; than <delta> away, select path with that route.
;; otherwise, select first clockwise
(defn preference
  "Sorter function for paths. Returns true if p1 is the best match, false
   otherwise. This is decided by either best-route, or if the best route score
   is too high, by dir-sort-fn."
  [color p1 p2]
  (let [[best-path best-score] (best-route color [p1 p2])]
    (if (and best-path
             (<=delta? best-score))
      (= best-path p1)
      (dir-sort-fn p1 p2))))

(defn preferred-path
  "select the preferred path for this vehicle, as decided by the preference
   function."
  [v]
  (let [paths (out-paths (vehicle-cell v))]
    (first (sort (partial preference (cargo-color v))
                 paths))))

(defn update-preferred-path
  [v]
  (assoc v
    :exit-direction (:direction (preferred-path v))))

(defn- select-exit
  "Returns a Vehicle record that is the given vehicle record with an exit time
   and exit direction attached to it. Exit time is based on the vehicle type,
   exit-direction on preferred-path."
  [v]
  (assoc v
    :exit-time (+ (:entry-time v)
                  (vehicle->duration v))
    :exit-direction (:direction (preferred-path v))))

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
  (assoc v :exit-time nil :exit-direction nil))

(defn start-despawning
  "Modifies the state of the vehicle with the given id to :despawning, and
   schedules removal from the map."
  [id]
  {:pre [(spawn? (vehicle-cell (vehicle id)))]}
  (dosync 
   (vehicle-state-change id :despawning)
   (update-vehicle id vehicle-clear-exit)
   (schedule #(unload-vehicle (vehicle id))
             (offset (/ (vehicle->duration (vehicle id))
                        2)))))

(defn- schedule-state-change
  "Schedules a state change for the vehicle with the given id to the given state
   at the given time."
  [id state time]
  (schedule #(dosync (vehicle-state-change id state))
            time))

(defn half-duration [vehicle-id]
  (/ (vehicle->duration (vehicle vehicle-id))
     2))

(defn start-exploding
  "Modifies the state of the vehicle with the given id to :exploding."
  [id]
  (vehicle-state-change id :exploding))

(defn schedule-explode [id]
  (schedule #(dosync (start-exploding id))
            (offset (half-duration id))))

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
  (schedule #(start-despawning id)
            (offset (half-duration id))))

(defmethod vehicle-transition-state
  [true :spawn]
  [id]
  ;;TODO add penalty for despawning with cargo
  (schedule #(start-despawning id)
            (offset (half-duration id))))

(defn resource-hue [cell]
  (hue (:resource-type cell)))

(defmethod vehicle-transition-state
  [false :supply]
  [id]
  (set-cargo id
             (resource-hue (vehicle-cell (vehicle id)))
             (cargo-capacity (:type (vehicle id)))))

(defmethod vehicle-transition-state
  [false :mixer]
  [id]
  (dosync
   (let [color (resource-color (vehicle-cell (vehicle id)))
         pickup-count (min (resource-count (vehicle-cell (vehicle id)))
                           (cargo-capacity (:type (vehicle id))))]
     (reduce-resource (vehicle-cell (vehicle id)) pickup-count)
     (set-cargo id color pickup-count))))


(defmethod vehicle-transition-state
  [true :mixer]
  [id]
  (dosync
   (mix-colors (vehicle-cell (vehicle id))
               (cargo-color (vehicle id))
               (cargo-count (vehicle id)))
   (clear-cargo id)))

(defmethod vehicle-transition-state
  [true :depot]
  [id]
  (when (< (hue-difference (cargo-color (vehicle id))
                          (resource-hue (vehicle-cell (vehicle id))))
           (/ Math/PI 8))
    ;;TODO this should also update some score
    (clear-cargo id)))

(defn maybe-explode [id]
  (when-not (or (:exit-direction (vehicle id))
                (spawn? (vehicle-cell (vehicle id))))
    (schedule-explode id)))

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
                   direction)]
    (dosync
     (unload-vehicle v)
     (load-vehicle (to path) v)
     (update-vehicle id (comp select-exit vehicle-enter))
     (vehicle-transition-state id)
     (update-vehicle id update-preferred-path)
     (maybe-explode id))))

(defn- schedule-move
  "Schedules the next move for the vehicle with the given id. If the vehicle has
   entered a spawn point, no move is scheduled."
  [id]
  (schedule (fn []
              (dosync
               (if (exploding? id)
                 (unload-vehicle (vehicle id))
                 (do
                   (move-vehicle id (:exit-direction (vehicle id)))
                   (when-not (spawn? (vehicle-cell (vehicle id)))
                     (schedule-move id))))))
            (offset (vehicle->duration (vehicle id)))))

(defn- load-vehicle-on-spawn
  "Loads a vehicle on a spawn point, setting all initial state."
  [c]
  {:pre [(spawn? c)]}
  (dosync
   (load-vehicle c (select-exit (map->Vehicle
                                 {:id (next-idc)
                                  :type (vehicle-type c)
                                  :coords (coords c)
                                  :entry-time @game-time
                                  :state :spawning})))))

(defn spawn
  "Spawns a vehicle on the given cell."
  [c]
  {:pre [(spawn? c)]}
  (dosync
   (let [vehicle (load-vehicle-on-spawn c)]
     (if (:exit-direction vehicle)
       (schedule-state-change (:id vehicle) :moving (/ (vehicle->duration vehicle)
                                                       2))
       (schedule-explode (:id vehicle)))
     (schedule-move (:id vehicle))
     vehicle)))
