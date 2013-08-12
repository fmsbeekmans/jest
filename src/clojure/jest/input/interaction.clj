(ns jest.input.interaction
  (:require [jest.input.core :refer [set-input-handler!]]
            [jest.world :refer [directions direction cell direction-exists? coords]]
            [clojure.core.match :refer [match]]
            [jest.world.building :refer [spawn? vehicle-type restricted?]]
            [jest.world.path :refer [path in-path? build-path unbuild-path in-paths path-type opposite-dirs vehicle->path from to paths]]
            [jest.world.route :refer [paths-with-route build-route unbuild-route]]
            [jest.color :refer [hue-matches?]]
            [jest.world.vehicle :refer [vehicles cargo? update-vehicle vehicle-cell moving? pickup-color]]
            [jest.behavior.state :refer [update-vehicles-for-cell-changes incoming? outgoing?]]
            [jest.scheduler :refer [paused? resume! pause!]]))

(def inv-directions (clojure.set/map-invert directions))
(def pointers ( atom {}))

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

(defrecord Pointer [path-type coord])
(defrecord Movement [from to dir inv-dir])

(defn extract-path-type [cell]
  (let [{:keys [building-type vehicle-type]} cell]
    {:path-type
     (if (= building-type :spawn)
       (vehicle->path vehicle-type)
       (let [types (set (map :type (paths cell)))]
         ;; TODO only roads for now
         (:road types)))}))

(defn extract-route-info [cell]
  (if-let [v (first (routable-vehicles cell))]
    {:route (pickup-color v)}))

(defn update-pointer [id pointer]
  (swap! pointers assoc id pointer))

(defn track-pointer [id cell]
  (let [{:keys [coord]} cell]
    (update-pointer id
                    (map->Pointer
                     (merge {:coord coord}
                            (extract-path-type cell)
                            (extract-route-info cell))))))

(defn untrack-pointer [id]
  (swap! pointers dissoc id))

(def pointer-track ( atom {}))
;;;
(defn set-route
  ([c path color]
     (doall
      (map #(unbuild-route c (:direction %) color) (paths-with-route c color)))
     (build-route c (:direction path) color))
  ([c dir color path-kind]
     (set-route c (dir (:paths c)) color)))

(defn on-down
  [id pos]
  (when-not (apply restricted? pos)
    (dosync
     ;; (if (= pos [0 0])
     ;;   (if (paused?)
     ;;     (resume!)
     ;;     (pause!)))
     (track-pointer id (cell pos)))))

;;;(defrecord Movement [from to dir inv-dir])
(defn- from-path [m]
  (let [{:keys [dir from]} m]
    (dir (:paths from))))

(defn- to-path [m]
  (let [{:keys [inv-dir to]} m]
    (inv-dir (:paths to))))

(defn- unbuild-movement [m]
  (let [{:keys [dir from]} m]
    (unbuild-path from dir)))

(defn- build-movement [m type]
  (let [{:keys [dir from]} m]
    (build-path from dir type)))

(defn route-movement [m color path-type]
  (let [{:keys [dir from]} m]
    (set-route from dir color path-type)))

(defn u-cell [m]
  (let [{:keys [to from]} m
        to (cell (:coord to))
        from (cell (:coord from))]
    (-> m
        (assoc :to to)
        (assoc :from from))))

(defn on-move [id from-pos to-pos]
  (dosync
   (let [dir (inv-directions (map - to-pos from-pos))
         movement (map->Movement {:from (cell from-pos)
                                  :to (cell to-pos)
                                  :dir dir
                                  :inv-dir (opposite-dirs dir)})
         pointer (@pointers id)
         ptype (:path-type pointer)
         pointer (if (apply restricted? to-pos)
                   (assoc pointer :path-type nil)
                   pointer)
         ]
     (when (:path-type pointer)
       (let [fpath (from-path movement)
             tpath (to-path movement)
             matcher (vec (map :inout [fpath tpath]))
             up (fn [] (unbuild-movement movement))
             bp (fn [] (build-movement movement (:path-type pointer)))
             br (fn []
                  (when (contains? pointer :route)
                    (route-movement (u-cell movement)
                                    (:route pointer)
                                    (:path-type pointer))))]
         (match matcher
                [nil nil]  (do (bp) (br))
                [:in :out] (do (up) ;(bp) (br)
                               )
                [:out :in] (do (br))
                [_ _] (println "Default handler, should never come here..."))))
     (update-pointer id (assoc pointer :coord (:to movement)))
     (update-vehicles-for-cell-changes (cell from-pos))
     (update-vehicles-for-cell-changes (cell to-pos)))))

(defn on-up [id _]
  (dosync
   (untrack-pointer id)))

(defn interaction-setup []
  (set-input-handler! :on-move on-move)
  (set-input-handler! :on-down on-down)
  (set-input-handler! :on-up on-up))
