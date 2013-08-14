(ns jest.visualize.visualize
  "Functions to facilitate the visualisation of the world state."
  (:require [jest.visualize.arrow :refer [arrow]]
            [jest.visualize.junction :refer [nice-lookup]]
            [jest.visualize.building :refer [cell-spawn
                                             cell-building]])
  (:require [jest.score :as score :refer [set-visualize-score-fn!]]
            [jest.visualize.score-screen :refer [score-screen]])
  (:require [brick.drawable :as drawable :refer [square-borders-size]]
            [quil.core :as quil])
  (:require [jest.world :as world :refer [world-size]]
            [jest.world.vehicle :as vehicle
             :refer [vehicle-cell]]
            [jest.world.path :as path
             :refer [opposite-dirs]])
  (:require [jest.visualize.points :as points]
            [jest.visualize.util :as util
             :refer [cell-points absolute->relative]]
            [jest.visualize.resource :as resource])
  (:require [jest.visualize.input :as input]))

(def cell-bg
  "fn to get the bg-sprite from the sprite-dictionary"
  :background)

(def min-borders
  "The minimum size of the borders around the game."
  [0.1 0.1])

(def win-screen-visible "win-" (atom false))


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

(defn img-overlay [img position]
  (fn []
    (quil/push-style)
    (quil/tint 255 128)
    (apply quil/image img position)
    (quil/pop-style)
    (schedule-on-fps (img-overlay img position))))

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

(defn frame-count []
  (fn []
    (quil/text (str "Framerate: " (quil/current-frame-rate))
               0 100)
    (schedule-on-fps (frame-count))))

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

(def direction-to-angle
  {:north (* 1.5 Math/PI)
   :south (* 0.5 Math/PI)
   :west Math/PI
   :east 0.0})

(defn spawning-vehicle->location
  "Spawning vehicle placement"
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (vehicle-center v)
       :rotation (direction-to-angle (:exit-direction v))}
      {:position (absolute->relative
                  (points/.point stroke (* 2 (- progress 0.5))))
       :rotation (direction-to-angle (:exit-direction v))})))

(defn spawning-animation
  "Spawning animation modifier"
  [vehicle drawable]
  (let [progress (util/vehicle->progress vehicle)]
    (drawable/->Tint
     (drawable/->Floating drawable
                          [0.5 0.5]
                          (min 1 (* 2 (util/vehicle->progress vehicle)))
                          0)
     [255 (min 255 (* progress 510))])))

(def spawning-vehicle
  "Defining the animation for a spawning vehicle."
  (vehicle-animation spawning-vehicle->location spawning-animation))

(defn despawning-vehicle->location
  "Placement of a despawning vehicle"
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (absolute->relative
                  (points/.point stroke (* 2 progress)))
       :rotation (direction-to-angle (opposite-dirs (:entry-direction v)))}
      {:position (vehicle-center v)
       :rotation (direction-to-angle (opposite-dirs (:entry-direction v)))})))

(defn despawning-animation
  "Animation modifier for a despawning vehicle."
  [v d]
  (let [progress (util/vehicle->progress v)]
    (drawable/->Tint
     (drawable/->Floating
      d
      [0.5 0.5]
      (min (- 2 (* 2 progress))
           1)
      0)
     [255 (* 255 (max 0 (- 1 (* 2 (- progress 0.5)))))])))

(def despawning-vehicle
  "Define the animation for a despawning vehicle."
  (vehicle-animation despawning-vehicle->location despawning-animation))

(defn exploding-vehicle->location
  "placement of an exploding vehicle."
  [v]
  (let [progress (util/vehicle->progress v)
        stroke (util/vehicle->stroke v [(quil/width) (quil/height)])]
    (if (< progress 0.5)
      {:position (absolute->relative
                  (points/point stroke (* 2 progress)))
       :rotation (direction-to-angle (opposite-dirs (:entry-direction v)))}
      {:position (vehicle-center v)
       :rotation (direction-to-angle (opposite-dirs (:entry-direction v)))})))

(defn exploding-animation
  "Modify d to be exploding"
  [v d]
  (let [progress (util/vehicle->progress v)
        col-gradient (points/->Linear [255 255 255]
                                      [32 0 0])]
    (drawable/->Tint
     (drawable/->Floating d
                          [0.5 0.5]
                          (min (- 2 (* 2 progress))
                               1)
                          (- (* Math/PI 4 progress)))
     (points/point col-gradient progress))))

(def exploding-vehicle
  "Animation for an exploding vehicle"
  (vehicle-animation exploding-vehicle->location exploding-animation))

(defn vehicles->Stack
  "Create a stack of all vehicles."
  [image]
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
  "Create the mapping from the world state to a drawable
with a tile-set and a tile-set for paths."
  [tile-fn path-fn]
  (drawable/->Stack
   [
    (world-state->Grid (comp tile-fn (constantly :grass)))
    (world-state->Grid path-fn)
    (world-state->Grid (partial cell-spawn
                                (comp (fn [keyword]
                                        (drawable/->Tint keyword
                                                         [255 128])) tile-fn)))
    (vehicles->Stack (tile-fn :truck))
    (world-state->Grid paths-to-arrows)
    (world-state->Grid (partial cell-building tile-fn))
    (score->drawable @score/current-score)
    ]))

(declare animate-score-on-tile)

(defn setup
  "Setup the visualization of the game."
  [tile-fn]
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
                      win-screen-visible)])
                   [w h]))))
             (atom [])
             :title "Traffic"
             :decor false
             :size [800 600]
             :renderer :java2d
             :init (fn [_] (quil/frame-rate 60))
             :mouse-pressed input/on-down-handler
             :mouse-released input/on-up-handler
             :mouse-dragged input/on-move-handler)))
  (set-visualize-score-fn! #(animate-score-on-tile %1 %2 %3 60)))

(defn sketch!
  "Show the visualization as a sketch."
  []
  (reset! world-sketch (drawable/drawable->sketch! @world-bricklet)))

(defn sketch-width
  "Return the width of the sketch. If none exists return 0"
  []
  (try
    (.getWidth @world-sketch)
    (catch NullPointerException e
      0)))

(defn sketch-height
  "Return the height of the sketch. If none exists return 0"
  []
  (try
    (.getHeight @world-sketch)
    (catch NullPointerException e
      0)))

(defn sketch-size
  "Return the size of the sketch. [0 0] if there's no sketch."
  []
  [(sketch-width) (sketch-height)])

(defn tl
  "Define where the top-left of the play-field is (relative)."
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
  "Define where the bottom-right of the play-field is (relative)."
  []
  (map
   (partial - 1)
   (tl)))

(defn abs-tl []
  "Define where the top-left of the play-field is (absolute)."
  (map * (tl) (sketch-size)))

(defn abs-br []
  "Define where the bottom-right of the play-field is (absolute)."
  (map * (br) (sketch-size)))

(defn map-size []
  "Return the size of the map in pixels."
  (map - (abs-br) (abs-tl)))

(defn cell-size []
  "Return the average size of a cell."
  (map /
       (map-size)
       (world-size)))

(defn pixel->tile
  "In what tile is cell [x y]? "
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
  "Find a pixel in a tile."
  ([[tx ty] [dx dy]]
     (let [tl (map * (tl) (sketch-size))
           br (map * (br) (sketch-size))
           map-size (map - br tl)
           cell-size (map nn/ map-size (world-size))]
       (map +
            tl
            (map #(/ % 2) cell-size)
            (map * cell-size [tx ty])
            (map *
                 cell-size
                 [dx dy]))))
  ([[tx ty]]
     (tile->pixel [tx ty] [0 0])))

(defmacro with-tile
  "Do the actions of body with the cell on c bound to the symbol ~t."
  [[t c] & body]
  `(let [~t (apply pixel->tile ~c)]
     ~@body))

(defn animate-score-on-tile
  "Start a score animation on tile [tx ty]."
  [score-delta [tx ty] type duration]
  (schedule-on-fps #((score-animation score-delta
                                      (tile->pixel [tx ty] [-0.4 -0.1])
                                      type
                                      duration))))
