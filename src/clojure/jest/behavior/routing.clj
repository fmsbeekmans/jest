(ns jest.behavior.routing
  (:require [jest.world.path :refer [out-paths]]
            [jest.world.vehicle :refer [vehicle-cell cargo-color moving? vehicle->duration]]
            [jest.color :refer [<=delta?]]
            [jest.util :refer [angle-difference]]))

(defn- dir-num
  "Maps a direction to a number, going clockwise from :north = 0."
  [dir]
  (.indexOf [:north :east :south :west] dir))

(defn- route-score
  "Returns the lowest hue difference with the given color among the routes for
   the given path. If there are no routes, route-score returns nil. A lower
   route score is better."
  [color path]
  (cond color
        (let [hue-diffs  (map (partial angle-difference color)
                              (remove nil? (:routes path)))]
          (if (seq hue-diffs)
            (apply min hue-diffs)))

        (contains? (:routes path) nil)
        0

        :default
        nil))

(defn- dir-sort-fn
  "Sorter function for paths. Returns true if p1 should go before p2, false
   otherwise. Sorting happens based on clockwise direction, with :north first."
  [p1 p2]
  (<= (dir-num (:direction p1))
      (dir-num (:direction p2))))

(defn- best-route
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
(defn- preference
  "Sorter function for paths. Returns true if p1 is the best match, false
   otherwise. This is decided by either best-route, or if the best route score
   is too high, by dir-sort-fn."
  [color p1 p2]
  (let [[best-path best-score] (best-route color [p1 p2])]
    (if (and best-path
             (<=delta? best-score))
      (= best-path p1)
      (dir-sort-fn p1 p2))))

(defn- preferred-path
  "select the preferred path for this vehicle, as decided by the preference
   function."
  [v]
  (let [paths (out-paths (vehicle-cell v))]
    (first (sort (partial preference (cargo-color v))
                 paths))))

(defn update-preferred-path
  [v]
  (assoc v
    :exit-direction (if (moving? v)
                      (:direction (preferred-path v)))))

(defn select-exit
  "Returns a Vehicle record that is the given vehicle record with an exit time
   and exit direction attached to it. Exit time is based on the vehicle type,
   exit-direction on preferred-path."
  [v]
  (assoc v
    :exit-time (+ (:entry-time v)
                  (vehicle->duration v))
    :exit-direction (:direction (preferred-path v))))
