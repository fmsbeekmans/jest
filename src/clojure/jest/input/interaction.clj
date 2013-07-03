(ns jest.input.interaction
  (:require [jest.input.core :refer [set-input-handler!]]
            [jest.world :refer [directions direction cell direction-exists?]]
            [jest.world.building :refer [spawn?]]
            [jest.world.path :refer [path in-path? build-path unbuild-path in-paths path-type opposite-dirs vehicle->path]]
            [jest.world.route :refer [paths-with-route build-route unbuild-route]]
            [jest.vehicle :refer [vehicles cargo? cargo-color update-vehicle]]
            [jest.movement :refer [spawn preferred-path update-vehicles-for-cell-changes]]
            [jest.scheduler :refer [paused? resume! pause!]]))

(def ^:private inv-directions (clojure.set/map-invert directions))

(defn on-down [id pos]
  (if (and (direction-exists? (cell pos) :south)
           (spawn? (direction (cell pos) :south)))
    (spawn (direction (cell pos) :south))
    (case pos
      [0 0]
      (if (paused?)
        (resume!)
        (pause!))
      nil)))

(defn- maybe-build-route [c dir]
  (dosync
   (if-let [v (first (vehicles c))]
     (let [vehicle-type (:type v)]
       (when (and (cargo? v)
                  (= (path-type (path c dir))
                     (vehicle->path (:type v))))
         (doseq [p (filter #(= (vehicle->path (:type v)) (path-type %))
                           (paths-with-route c (cargo-color v)))]
           (unbuild-route c (:direction p) (cargo-color v)))
         (build-route c dir (cargo-color v))
         (update-vehicles-for-cell-changes c))))))

(defn on-move [id pos1 pos2]
  (let [c1 (cell pos1)
        c2 (cell pos2)
        direction (inv-directions (map - pos2 pos1))]
    (if (path c1 direction)
      (if (in-path? (path c1 direction))
        (unbuild-path c1 direction)
        (maybe-build-route c1 direction))

      (if (or (seq (in-paths c1))
              (spawn? c1))
        (build-path c1 direction :road)))
    ))

(def pointer-track ( atom {}))

(defn track-pointer [id type tile direction on-same on-empty]
  (let [[path-type count] (@pointer-track id)]
    (swap! pointer-track assoc id
           (if path-type
             [(cond (= :invalid path-type)
                    :invalid

                    (= type path-type)
                    (on-same)
                    
                    (nil? type)
                    (on-empty)
                    
                    :default
                    :invalid)
              (inc count) tile direction]
             [(if type
                (on-same)
                (on-empty)) 1 tile direction]))))

(defn untrack-pointer [id]
  (swap! pointer-track dissoc id))

(defn pointer-track-type [id]
  (first (@pointer-track id)))

(defn pointer-track-tile [id]
  ((@pointer-track id) 2))

(defn pointer-track-direction [id]
  ((@pointer-track id) 3))

(let [paths [:road :rails :canal]]
  (defn path-type-sort [p1 p2]
    (< (.indexOf paths p1)
       (.indexOf paths p2))))

(defn on-move [id pos1 pos2]
  (dosync 
   (let [c1 (cell pos1)
         c2 (cell pos2)
         direction (inv-directions (map - pos2 pos1))
         path (path c1 direction)
         type (path-type path)]
     (track-pointer id type pos1 direction
                    (fn on-same []
                      (when (in-path? path)
                        (unbuild-path c1 direction)
                        (update-vehicles-for-cell-changes c2))
                      type)
                    (fn on-empty []
                      (if-let [in-paths (seq (in-paths c1))]
                        (let [type (or (pointer-track-type id)
                                       (first (sort path-type-sort
                                                    (map path-type
                                                         in-paths))))]
                          (build-path c1 direction
                                      type)
                          (update-vehicles-for-cell-changes c1)
                          type)
                        :invalid))))))

(defn on-up [id _]
  (let [[type count tile direction] (@pointer-track id)]
    (if (and count
             (= count 1)
             (not= type :invalid))
      (maybe-build-route (cell tile) direction)))
  (untrack-pointer id))

(defn interaction-setup []
  (set-input-handler! :on-move on-move)
  (set-input-handler! :on-down on-down)
  (set-input-handler! :on-up on-up))
