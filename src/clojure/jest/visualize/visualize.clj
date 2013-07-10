(ns jest.visualize.visualize
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)
  (:use jest.visualize.arrow)
  (:use jest.visualize.junction)
  (:use [clojure.core.match :only [match]])
  (:require [brick.image :as image]
            [brick.drawable :as drawable]
            [quil.core :as quil])
  (:require [jest.world :as world]
            [jest.movement :as movement]
            [jest.world.building :as building]
            [jest.vehicle :as vehicle
             :refer [vehicle-cell]]
            [jest.world.path :as path
             :refer [opposite-dirs]]
            [jest.world.cell :as cell])
  (:require [jest.visualize.points :as points]
            [jest.visualize.util :as util
             :refer [cell-points absolute->relative]]
            [jest.visualize.resource :as resource]
            [jest.color :as color])
  (:require [jest.visualize.input :as input]))

(declare cell-bg)
(declare cell-building)
(declare cell-road)

(def min-borders [0.1 0.1])

(declare sketch-size)

(let [cardinal-arrow (memoize (partial arrow 0.5 0.5))
      arrow-stack (memoize (fn [dirs routes]
                             (drawable/->Stack
                              (vec (map cardinal-arrow dirs routes)))))]
  (defn paths-to-arrows [c]
    (let [*pi (partial * Math/PI)
          dir-to-radian  {:north (*pi 3/2)
                          :east (*pi 0)
                          :south (*pi 1/2)
                          :west (*pi 1)}
          out-p (path/out-paths c)]
      (arrow-stack (map dir-to-radian (map :direction out-p))
                   (map :routes out-p)))))


(defonce world-bricklet (atom nil))
(defonce world-sketch (atom nil))

(defn world-state->Grid
  "Builds a layer from the world state.
cell-draw-fn is a function that returns a Drawable."
  [cell-draw-fn]
;  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (apply drawable/->SquareTiledGrid
   (world/world-width)
   (world/world-height)
   (into {}
         (doall
          (for [c (world/all-cells)]
            [(world/coords c) (cell-draw-fn c)])))
   min-borders))

(defn moving-vehicle->location
  [v]
  (let [stroke (util/vehicle->stroke v [(quil/width)
                                        (quil/height)])
        p (util/vehicle->progress v)
        [x y] (points/point
               stroke p)]
    {:position [(/ x (quil/width))
          (/ y (quil/height))]
     :rotation (points/tangent stroke p [0 1])}))

(defn vehicle-animation
  ([location-fn]
     (vehicle-animation location-fn (fn [_ x] x)))
  ([location-fn image-modifier-fn]
     (fn [v image]
       (let [{:keys [position rotation]} (location-fn v)]
         (drawable/->Stack [(drawable/->Floating (image-modifier-fn v image)
                                                 position
                                                 (util/vehicle-scale)
                                                 rotation)
                            (drawable/->Floating (resource/drawable-from-resource-rate
                                                  (resource/vehicle-resource-rate v))
                                                 position
                                                 (util/vehicle-scale)
                                                 0)])))))

(def moving-vehicle (vehicle-animation moving-vehicle->location))

(defn vehicle-center [v]
  (absolute->relative (:center (cell-points (vehicle-cell v)))))

(def lols
  {:north (* 1.5 Math/PI)
   :south (* 0.5 Math/PI)
   :west Math/PI
   :east 0.0})

(defn spawning-vehicle->location
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (vehicle-center v)
       :rotation (+ (lols (:exit-direction v))
                    (* 4 Math/PI progress))}
      {:position (absolute->relative
                  (points/point stroke (* 2 (- progress 0.5))))
       :rotation (lols (:exit-direction v))})))

(defn spawning-scaler [vehicle drawable]
  (drawable/->Floating drawable
                       [0.5 0.5]
                       (min 1 (* 2 (util/vehicle->progress vehicle)))
                       0.0))

(def spawning-vehicle (vehicle-animation spawning-vehicle->location spawning-scaler))

(defn despawning-vehicle->location
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (absolute->relative
                  (points/point stroke (* 2 progress)))
       :rotation (lols (opposite-dirs (:entry-direction v)))}
      {:position (vehicle-center v)
       :rotation (+ (lols (opposite-dirs (:entry-direction v)))
                    (* 12 progress Math/PI))})))

(defn despawning-scaler [v d]
  (drawable/->Floating d
                       [0.5 0.5]
                       (min (- 2 (* 2 (util/vehicle->progress v)))
                            1)
                       0))

(def despawning-vehicle (vehicle-animation despawning-vehicle->location despawning-scaler))

(defn vehicles->Stack
  [vehicle-type image]
  (apply drawable/->Border
         (drawable/->Stack
          (vec
           (map (fn [v]
                  (cond
                   (vehicle/moving? v) (moving-vehicle v image)
                   (vehicle/spawning? v) (spawning-vehicle v image)
                   (vehicle/despawning? v) (despawning-vehicle v image)
                   (vehicle/exploding? v) (drawable/->Nothing)))
                (vehicle/all-vehicles vehicle/truck?))))
             (drawable/square-borders-size
              (sketch-size)
              (world/world-size)
              min-borders)))

(defn world->drawable
  [tile-fn path-fn]
  (drawable/->Stack
   [
    (world-state->Grid (comp tile-fn (constantly :grass)))
    (world-state->Grid path-fn)
    (vehicles->Stack :truck (tile-fn :truck))
    (world-state->Grid paths-to-arrows)
    (world-state->Grid (partial cell-building tile-fn))
    ]))

(defn cell-bg [c]
  "What is the background tile-key for this cell?"
  (:background c))

(defn cell-building
  "Which building tile-key fits this cell?"
  [tile-fn c]
  (if-let [type (building/building-type c)]
    (let [resource-vis (comp resource/drawable-from-resource-rate
                             resource/building-resource-rate)]
      (case type
        :spawn (tile-fn
                (hyphenate-keywords :spawn (building/vehicle-type c)))
        :mixer (drawable/->Stack [(tile-fn :mixer)
                                  (resource-vis c)])
        :supply (drawable/->Stack [(tile-fn :supply-red)
                                   (resource-vis c)])
        :depot (drawable/->Stack [(tile-fn :dirt)
                                  (resource-vis c)])))
    (tile-fn nil)))

(defn cell-road
  "Return the appropriate tile-key for roads in this cell."
  [c]
  (let [{in :in
         out :out} (group-seq
                    (path/paths c :road)
                    {:in path/in-path?
                     :out path/out-path?})]
    ;; actual
    (apply (partial hyphenate-keywords :road)
           (map :direction out))
    ;; temp
    (if-not (empty? (first out))
      (hyphenate-keywords :road (:direction (first out))))))

(defn setup [tile-fn]
  ;init een bricklet met tile-set
  (let [path-fn (nice-lookup)]
    (reset! world-bricklet
            (drawable/->Bricklet
             (atom
              (reify drawable/Drawable
                (draw [this [w h]]
                  (drawable/.draw
                   (world->drawable
                    tile-fn
                    path-fn)
                   [w h]))))
             (atom [])
             :decor false
             :size [800 600]
             :renderer :java2d
             :init (fn [_] (quil/frame-rate 60))
             :mouse-pressed input/on-down-handler
             :mouse-released input/on-up-handler
             :mouse-dragged input/on-move-handler))))

(defn sketch! []
  (reset! world-sketch (drawable/drawable->sketch! @world-bricklet)))

(defn sketch-width []
  (.getWidth @world-sketch))

(defn sketch-height []
  (.getHeight @world-sketch))

(defn sketch-size []
  [(sketch-width) (sketch-height)])
