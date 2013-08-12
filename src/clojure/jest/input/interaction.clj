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

;; ;;;;;; OLD CODE FROM HERE
;; (defn track-pointer [id type tile direction on-same on-empty]
;;   "Track the pointenters."
;;   (let [{:keys [path-type count route]} (@pointer-track id)]
;;     (swap! pointer-track assoc id
;;            (if path-type
;;              ;; update existing pointer
;;              {:path-type (cond (= :invalid path-type)
;;                                :invalid

;;                                (= type path-type)
;;                                (on-same)

;;                                (nil? type)
;;                                (on-empty)

;;                                :default
;;                                :invalid)
;;               :route route
;;               :count (inc count)
;;               :tile tile
;;               :direction direction}
;;              ;; initialize new pointer
;;              (do
;;                {:path-type (if type
;;                              (on-same)
;;                              (on-empty))
;;                 :count 1
;;                 :route (:cargo (first (routable-vehicles tile)))
;;                 :tile tile
;;                 :direction direction})))))

;; (defn untrack-pointer [id]
;;   (swap! pointer-track dissoc id))

;; (defn pointer-track-path-type [id]
;;   (:path-type (@pointer-track id)))

;; (defn pointer-track-tile [id]
;;   (:tile (@pointer-track id)))

;; (defn pointer-track-direction [id]
;;   (:direction (@pointer-track id)))

;; (defn pointer-track-route [id]
;;   (println (@pointer-track id))
;;   (:route (@pointer-track id)))




;; (defn on-down [id pos]
;;   (case pos
;;     [0 0]
;;     (if (paused?)
;;       (resume!)
;;       (pause!))
;;     nil))



;; (defn- maybe-build-route [c dir]
;;   (dosync
;;    (if-let [v (first (routable-vehicles c))]
;;      (let [vehicle-type (:type v)
;;            color (pickup-color v)]
;;        (when (= (path-type (path c dir))
;;                 (vehicle->path vehicle-type))
;;          (doseq [p (filter #(= (vehicle->path vehicle-type) (path-type %))
;;                            (paths-with-route c color))]
;;            (unbuild-route c (:direction p) color))
;;          (build-route c dir color)
;;          (update-vehicles-for-cell-changes c))))))

;; (defn set-route
;;   [c dir color path-kind]
;;   (when path-type
;;     (println (:coords c))
;;     (dosync
;;      (when (= (path-type (path c dir)) path-kind)
;;        (doseq [p (filter #(= path-type (path-type %))
;;                          (paths-with-route c color))]
;;          (unbuild-route c (:direction p) color))
;;        (build-route c dir color)
;;        (update-vehicles-for-cell-changes c)))))


;; (defn track-pointer [id type tile direction on-same on-empty]
;;   "Track the pointenters."
;;   (let [{:keys [path-type count route]} (@pointer-track id)]
;;     (swap! pointer-track assoc id
;;            (if path-type
;;              ;; update existing pointer
;;              {:path-type (cond (= :invalid path-type)
;;                                :invalid

;;                                (= type path-type)
;;                                (on-same)

;;                                (nil? type)
;;                                (on-empty)

;;                                :default
;;                                :invalid)
;;               :route route
;;               :count (inc count)
;;               :tile tile
;;               :direction direction}
;;              ;; initialize new pointer
;;              (do
;;                {:path-type (if type
;;                              (on-same)
;;                              (on-empty))
;;                 :count 1
;;                 :route (:cargo (first (routable-vehicles tile)))
;;                 :tile tile
;;                 :direction direction})))))

;; (defn untrack-pointer [id]
;;   (swap! pointer-track dissoc id))

;; (defn pointer-track-path-type [id]
;;   (:path-type (@pointer-track id)))

;; (defn pointer-track-tile [id]
;;   (:tile (@pointer-track id)))

;; (defn pointer-track-direction [id]
;;   (:direction (@pointer-track id)))

;; (defn pointer-track-route [id]
;;   (println (@pointer-track id))
;;   (:route (@pointer-track id)))

;; (let [paths [:road :rails :canal]]
;;   ;; Waar is deze let voor?
;;   (defn path-type-sort [p1 p2]
;;     (< (.indexOf paths p1)
;;        (.indexOf paths p2))))

;; (defn on-move [id pos1 pos2]
;;   (dosync
;;    (let [c1 (cell pos1)
;;          c2 (cell pos2)
;;          direction (inv-directions (map - pos2 pos1))
;;          path (path c1 direction)
;;          type (path-type path)]
;;      (track-pointer id type pos1 direction
;;                     (fn on-same []
;;                       (if (in-path? path)
;;                         (do
;;                           (unbuild-path c1 direction)
;;                           (update-vehicles-for-cell-changes c2))
;;                         (set-route c1 direction
;;                                    (pointer-track-route id)
;;                                    (pointer-track-path-type id)))
;;                       type)
;;                     (fn on-empty []
;;                       (let [type' (if (restricted? c2)
;;                                     :invalid
;;                                     (if-let [in-paths (seq (in-paths c1))]
;;                                       (let [type (or (pointer-track-path-type id)
;;                                                      (first (sort path-type-sort
;;                                                                   (map path-type
;;                                                                        in-paths))))]
;;                                         (build-path c1 direction
;;                                                     type)
;;                                         (update-vehicles-for-cell-changes c1)
;;                                         type)
;;                                       (if (spawn? c1)
;;                                         (let [type (vehicle->path (vehicle-type c1))]
;;                                           (build-path c1 direction type)
;;                                           type)
;;                                         :invalid)))]
;;                         (set-route c1 direction
;;                                  (pointer-track-route id)
;;                                  (pointer-track-path-type id))
;;                         type'))))))

;; (defn on-up [id _]
;;   (untrack-pointer id))

(defn interaction-setup []
  (set-input-handler! :on-move on-move)
  (set-input-handler! :on-down on-down)
  (set-input-handler! :on-up on-up))
