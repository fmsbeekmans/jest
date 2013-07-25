(ns jest.input.interaction
  (:require [jest.input.core :refer [set-input-handler!]]
            [jest.world :refer [directions direction cell direction-exists? coords]]
            [jest.world.building :refer [spawn? vehicle-type restricted?]]
            [jest.world.path :refer [path in-path? build-path unbuild-path in-paths path-type opposite-dirs vehicle->path from to]]
            [jest.world.route :refer [paths-with-route build-route unbuild-route]]
            [jest.vehicle :refer [vehicles cargo? cargo-color update-vehicle vehicle-cell moving?]]
            [jest.movement :refer [spawn preferred-path update-vehicles-for-cell-changes incoming? outgoing? pickup-color]]
            [jest.scheduler :refer [paused? resume! pause!]]))

(def ^:private inv-directions (clojure.set/map-invert directions))

(defn on-down [id pos]
  (case pos
    [0 0]
    (if (paused?)
      (resume!)
      (pause!))
    nil))

(defn in-connected-cells [c]
  "All cells with a path to c."
  (map to (in-paths c)))

(defn routable-vehicles [c]
  (concat 
   (filter incoming? (vehicles c))
   (filter #(and (outgoing? %)
                 (moving? %)
                 (= (coords (to (path (vehicle-cell %) (:exit-direction %))))
                    (coords c)))
           (mapcat vehicles (in-connected-cells c)))))

(defn- maybe-build-route [c dir]
  (dosync
   (if-let [v (first (routable-vehicles c))]
     (let [vehicle-type (:type v)
           color (pickup-color v)]
       (when (= (path-type (path c dir))
                (vehicle->path vehicle-type))
         (doseq [p (filter #(= (vehicle->path vehicle-type) (path-type %))
                           (paths-with-route c color))]
           (unbuild-route c (:direction p) color))
         (build-route c dir color)
         (update-vehicles-for-cell-changes c))))))

(defn set-route
  [c dir color path]
  (if (and color
           path-type)
    (dosync
     (println (path-type (path (cell [1 1]) :south)))
     (when (= (path-type (path c dir)) path)

       (doseq [p (filter #(= path-type (path-type %))
                         (paths-with-route c color))]
         (unbuild-route c (:direction p) color))
       (build-route c dir color)
       (update-vehicles-for-cell-changes c)))))

(def pointer-track ( atom {}))

(defn track-pointer [id type tile direction on-same on-empty]
  "Track the pointenters."
  (let [{:keys [path-type count]} (@pointer-track id)]
    (swap! pointer-track assoc id
           (if path-type
             ;; update existing pointer
             {:path-type (cond (= :invalid path-type)
                               :invalid
                               
                               (= type path-type)
                               (on-same)
                               
                               (nil? type)
                               (on-empty)
                               
                               :default
                               :invalid)
              :count (inc count)
              :tile tile
              :direction direction}
             ;; initialize new pointer
             {:path-type (if type
                           (on-same)
                           (on-empty))
              :count 1
              :route (:cargo (first (routable-vehicles tile)))
              :tile tile
              :direction direction}))))

(defn untrack-pointer [id]
  (swap! pointer-track dissoc id))

(defn pointer-track-path-type [id]
  (:path-type (@pointer-track id)))

(defn pointer-track-tile [id]
  (:tile (@pointer-track id)))

(defn pointer-track-direction [id]
  (:direction (@pointer-track id)))

(defn pointer-track-route [id]
  (println (@pointer-track id))
  (:route (@pointer-track id)))

(let [paths [:road :rails :canal]]
  ;; Waar is deze let voor?
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
                      type
                      (set-route c1 direction
                                 (pointer-track-route id)
                                 (pointer-track-path-type id)))
                    (fn on-empty []
                      (if (restricted? c2)
                        :invalid
                        (if-let [in-paths (seq (in-paths c1))]
                          (let [type (or (pointer-track-path-type id)
                                         (first (sort path-type-sort
                                                      (map path-type
                                                           in-paths))))]
                            (build-path c1 direction
                                        type)
                            (update-vehicles-for-cell-changes c1)
                            type)
                          (if (spawn? c1)
                            (let [type (vehicle->path (vehicle-type c1))]
                              (build-path c1 direction type)
                              type)
                            :invalid)))
                      (set-route c1 direction
                                 (pointer-track-route id)
                                 (pointer-track-path-type id)))))))

(defn on-up [id _]
  (untrack-pointer id))

(defn interaction-setup []
  (set-input-handler! :on-move on-move)
  (set-input-handler! :on-down on-down)
  (set-input-handler! :on-up on-up))
