(ns jest.visualize.visualize
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)
  (:use jest.visualize.arrow)
  (:use jest.visualize.junction)
  (:use jest.visualize.building)
  (:require [jest.score :as score :refer [set-visualize-score-fn!]]
            [jest.visualize.score-screen :refer [score-screen]])
  (:require [brick.drawable :as drawable :refer [square-borders-size]]
            [quil.core :as quil])
  (:require [jest.world :as world :refer [world-size]]
            [jest.vehicle :as vehicle
             :refer [vehicle-cell]]
            [jest.world.path :as path
             :refer [opposite-dirs]])
  (:require [jest.visualize.points :as points]
            [jest.visualize.util :as util
             :refer [cell-points absolute->relative]]
            [jest.visualize.resource :as resource])
  (:require [jest.visualize.input :as input]))

(def cell-bg :background)

(def min-borders [0.1 0.1])
(def visible (atom false))

(declare sketch-size)
(declare world-bricklet)

(defn score->drawable [score]
  (reify drawable/Drawable
    (draw [this [w h]]
      (quil/push-style)
      (quil/fill 255)
      (quil/text (str "Score: " score) 10 10)
      (quil/pop-style))))

(defn schedule-on-fps [f]
  (swap! (:command-queue @world-bricklet)
         conj (fn [_] (f))))

(defn score-animation [score-delta location type duration]
  (let [[x y] location]
    (fn []
      (when (pos? duration)
        (quil/push-style)
        (quil/fill 0 (min (* 3 duration) 255))
        (quil/text (str (if (pos? score-delta) "+")
                        score-delta)
                   x y
                   duration)
        (schedule-on-fps
         (score-animation score-delta [x (dec y)] type (dec duration)))
        (quil/pop-style)))))

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
  (let [stroke (util/vehicle->stroke v (sketch-size))
        p (util/vehicle->progress v)
        [x y] (points/.point
               stroke p)]
    {:position [(/ x (quil/width))
          (/ y (quil/height))]
     :rotation (points/.tangent stroke p)}))

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
       :rotation (lols (:exit-direction v))}
      {:position (absolute->relative
                  (points/.point stroke (* 2 (- progress 0.5))))
       :rotation (lols (:exit-direction v))})))

(defn spawning-animation [vehicle drawable]
  (let [progress (util/vehicle->progress vehicle)]
    (->
     (drawable/->Floating drawable
                          [0.5 0.5]
                          (min 1 (* 2 (util/vehicle->progress vehicle)))
                          0)
     (drawable/->Tint
       ,,, [255 (min 255 (* progress 510))]))))

(def spawning-vehicle (vehicle-animation spawning-vehicle->location spawning-animation))

(defn despawning-vehicle->location
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (absolute->relative
                  (points/.point stroke (* 2 progress)))
       :rotation (lols (opposite-dirs (:entry-direction v)))}
      {:position (vehicle-center v)
       :rotation (lols (opposite-dirs (:entry-direction v)))})))

(defn despawning-animation [v d]
  (let [progress (util/vehicle->progress v)]
    (->
     (drawable/->Floating
      d
      [0.5 0.5]
      (min (- 2 (* 2 progress))
           1)
      0)
     (drawable/->Tint
      ,,, [255 (* 255 (max 0 (- 1 (* 2 (- progress 0.5)))))]))))

(def despawning-vehicle (vehicle-animation despawning-vehicle->location despawning-animation))

(defn exploding-vehicle->location
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (absolute->relative
                  (points/point stroke (* 2 progress)))
       :rotation (lols (opposite-dirs (:entry-direction v)))}
      {:position (vehicle-center v)
       :rotation (lols (opposite-dirs (:entry-direction v)))})))

(defn exploding-animation [v d]
  (let [progress (util/vehicle->progress v)
        col-gradient (points/->Linear [255 255 255]
                                      [32 0 0])]
    (->
     (drawable/->Floating d
                          [0.5 0.5]
                          (min (- 2 (* 2 progress))
                               1)
                          (- (* Math/PI 4 progress)))
     (drawable/->Tint
      ,,, (points/point col-gradient progress)))))

(def exploding-vehicle (vehicle-animation exploding-vehicle->location exploding-animation))

(defn vehicles->Stack
  [vehicle-type image]
  (apply drawable/->Border
         (drawable/->Stack
          (vec
           (map (fn [v]
                  (cond
                   (and (vehicle/spawning? v)
                        (vehicle/exploding? v))
                   (brick.drawable/->Nothing)
                   
                   (vehicle/moving? v) (moving-vehicle v image)
                   (vehicle/spawning? v) (spawning-vehicle v image)
                   (vehicle/despawning? v) (despawning-vehicle v image)
                   (vehicle/exploding? v) (exploding-vehicle v image)))
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
    (score->drawable @score/current-score)
    ]))

(declare animate-score-on-tile)

(defn setup [tile-fn]
  ;init een bricklet met tile-set
  (let [path-fn (nice-lookup)]
    (reset! world-bricklet
            (drawable/->Bricklet
             (atom
              (reify drawable/Drawable
                (draw [this [w h]]
                  (quil/background 200)
                  (drawable/.draw
                   (drawable/->Stack
                    [(world->drawable
                      tile-fn
                      path-fn)
                     (drawable/->Toggle
                      (score-screen)
                      visible)])
                   [w h]))))
             (atom [])
             :decor false
             :size [800 600]
             :renderer :java2d
             :init (fn [_] (quil/frame-rate 60))
             :mouse-pressed input/on-down-handler
             :mouse-released input/on-up-handler
             :mouse-dragged input/on-move-handler)))
  (set-visualize-score-fn! #(animate-score-on-tile %1 %2 %3 60)))

(defn sketch! []
  (reset! world-sketch (drawable/drawable->sketch! @world-bricklet)))

(defn sketch-width []
  (try
    (.getWidth @world-sketch)
    (catch NullPointerException e
      0)))

(defn sketch-height []
  (try
    (.getHeight @world-sketch)
    (catch NullPointerException e
      0)))

(defn sketch-size []
  [(sketch-width) (sketch-height)])

(defn tl
  []
  (try
    (square-borders-size
     (sketch-size)
     (world-size)
     min-borders)
    (catch Exception e
      (println (pr-str e))
      [0 0])))

(defn br
  []
  (map
   (partial - 1)
   (tl)))

(defn abs-tl []
  (map * (tl) (sketch-size)))

(defn abs-br []
  (map * (br) (sketch-size)))

(defn map-size []
  (map - (abs-br) (abs-tl)))

(defn cell-size []
  (map /
       (map-size)
       (world-size)))

(defn pixel->tile
  [x y]
  (let [tl (map * (tl) (sketch-size))
        br (map * (br) (sketch-size))
        map-size (map - br tl)
        cell-size (map / map-size (world-size))
        ppos (map - [x y] tl)
        tpos (map (comp int /) ppos cell-size)
        tpos (map max [0 0]
                  (map min tpos (map dec (world-size))))]
    tpos))

(defn tile->pixel
  ([[tx ty] [dx dy]]
     (let [tl (map * (tl) (sketch-size))
           br (map * (br) (sketch-size))
           map-size (map - br tl)
           cell-size (map / map-size (world-size))]
       (map +
            tl
            (map #(/ % 2) cell-size)
            (map * cell-size [tx ty])
            (map *
                 cell-size
                 [dx dy]))))
  ([[tx ty]]
     (tile->pixel [tx ty] [0 0])))

(defmacro with-tile [[t c] & body]
  `(let [~t (apply pixel->tile ~c)]
     ~@body))

(defn animate-score-on-tile [score-delta [tx ty] type duration]
  (schedule-on-fps #((score-animation score-delta
                                      (tile->pixel [tx ty] [-0.4 -0.1])
                                      type
                                      duration))))
