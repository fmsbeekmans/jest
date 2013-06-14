(ns jest.movement
  (:use [jest.vehicle :only [vehicle vehicle-cell cargo-color vehicle-state-change update-vehicle unload-vehicle vehicle->duration cargo? set-cargo cargo-capacity cargo-count clear-cargo load-vehicle despawning? map->Vehicle]]
        [jest.color :only [hue-difference <=delta?]]
        [jest.world :only [alter-cell coords]]
        [jest.world.path :only [out-paths path->duration vehicle->path opposite-dirs path path-type to]]
        [jest.world.building :only [spawn? vehicle-type resource-color resource-count reduce-resource mix-colors]]
        [jest.scheduler :only [schedule offset game-time]]))

(defonce ^:private idc (atom 0))

(defn- next-idc []
  (swap! idc inc))

(defn- dir-num [dir]
  (.indexOf [:north :east :south :west] dir))

(defn route-score [color path]
  (if color
    (let [hue-diffs  (map (partial hue-difference color)
                          (:routes path))]
      (if (seq hue-diffs)
        (apply min hue-diffs)))))

(defn dir-sort-fn [p1 p2]
  (<= (dir-num (:direction p1)) (dir-num (:direction p2))))

(defn best-route [color paths]
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

;; if there are routes, vehicle carries cargo, and best matching route is less than <delta> away, select path with that route.
;; otherwise, select first clockwise
(defn preference [color p1 p2]
  (let [[best-path best-score] (best-route color [p1 p2])]
    (if (and best-path
             (<=delta? best-score))
      (= best-path p1)
       (dir-sort-fn p1 p2))))

(defn preferred-path
  "select the preferred path for this vehicle"
  [v]
  (let [paths (out-paths (vehicle-cell v))]
    (first (sort (partial preference (cargo-color v))
                 paths))))

(defn- select-exit [v]
  (assoc v
    :exit-time (+ (:entry-time v)
                  (path->duration (vehicle->path (:type v))))
    :exit-direction (:direction (preferred-path v))))

(defn- vehicle-enter [v]
  (assoc (select-exit v)
    :entry-time (:exit-time v)
    :entry-direction (opposite-dirs (:exit-direction v))))

(defn- vehicle-clear-exit [v]
  (assoc v :exit-time nil :exit-direction nil))

(defn start-despawning [id]
  {:pre [(spawn? (vehicle-cell (vehicle id)))]}
  (vehicle-state-change id :despawning)
  (update-vehicle id vehicle-clear-exit)
  (schedule #(unload-vehicle (vehicle id))
            (offset (/ (vehicle->duration (vehicle id))
                       2))))

(defn vehicle-transition-state-dispatch
  [id]
  (let [vehicle (vehicle id)]
    [(cargo? vehicle)
     (:building-type (vehicle-cell vehicle))]))


(defmulti vehicle-transition-state
  "Does the required work for a vehicle upon moving into a cell. dispatches on [state cargo? cell-type]"
  vehicle-transition-state-dispatch)

(defmethod vehicle-transition-state :default
  [_])

(defmethod vehicle-transition-state
  [false :spawn]
  [id]
  (start-despawning id))

(defmethod vehicle-transition-state
  [true :spawn]
  [id]
  ;;TODO add penalty for despawning with cargo
  (start-despawning id))

(defmethod vehicle-transition-state
  [false :supply]
  [id]
  (set-cargo id
             (:resource-type (vehicle-cell (vehicle id)))
             (cargo-capacity (:type (vehicle id)))))

(defmethod vehicle-transition-state
  [false :mixer]
  [id]
  (dosync
   (let [color (resource-color (vehicle-cell (vehicle id)))
         pickup-count (min (resource-count (vehicle-cell (vehicle id)))
                           (cargo-capacity (:type (vehicle id))))]
     (alter-cell (vehicle-cell (vehicle id))
                 reduce-resource pickup-count)
     (set-cargo id color pickup-count))))


(defmethod vehicle-transition-state
  [true :mixer]
  [id]
  (dosync
   (alter-cell (vehicle-cell (vehicle id)) mix-colors (cargo-color (vehicle id)) (cargo-count (vehicle id)))
   (clear-cargo id)))

(defmethod vehicle-transition-state
  [true :depot]
  [id]
  (when (< (hue-difference (cargo-color (vehicle id))
                          (:resource-type (vehicle-cell (vehicle id))))
           (/ Math/PI 8))
    ;;TODO this should also update some score
    (clear-cargo id)))

(defn move-vehicle
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
               (when-not (despawning? id)
                 (schedule-move id))))
            (offset (vehicle->duration (vehicle id)))))

(defn- schedule-state-change [id state time]
  (schedule #(dosync (vehicle-state-change id state))
            time))

(defn- create-vehicle-on-spawn [c]
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
  (let [vehicle (create-vehicle-on-spawn c)]
    (schedule-state-change (:id vehicle) :moving (/ (vehicle->duration vehicle)
                                                    2))
    (schedule-move (:id vehicle))
    vehicle))


