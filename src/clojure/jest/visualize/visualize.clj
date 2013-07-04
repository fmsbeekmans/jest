(ns jest.visualize.visualize
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)

  (:require [brick.image :as image]
            [brick.drawable :as drawable]
            [quil.core :as quil])
  (:require [jest.world :as world]
            [jest.movement :as movement]
            [jest.world.building :as building]
            [jest.vehicle :as vehicle]
            [jest.world.path :as path]
            [jest.world.cell :as cell])
  (:require [jest.visualize.points :as points]
            [jest.visualize.util :as util]
            [jest.color :as color])
  (:require [jest.visualize.input :as input]))

(declare cell-bg)
(declare cell-building)
(declare cell-road)

(defn temp-lookup []
  (let [loader (comp
                drawable/->Image
                image/path->PImage
                clojure.java.io/resource
                (partial str "demo/"))]
    (let [junctions
          {[:east :out] (loader "00-east-out.png")
           [:south :out](loader "01-south-out.png")
           [:west :out] (loader "02-west-out.png")
           [:north :out](loader "03-north-out.png")
           [:east :in] (loader "10-east-in.png")
           [:south :in](loader "11-south-in.png")
           [:west :in] (loader "12-west-in.png")
           [:north :in](loader "13-north-in.png")
           }
          cached-stack drawable/->Stack]
      (fn [c]
        (let [roads (path/paths c :road)]
          (cached-stack
           (vec
            (doall
             (for [r roads]
                 (junctions [(:direction r)
                             (:inout r)]))))))))))


(defonce world-bricklet (atom nil))
(defonce world-sketch (atom nil))



(defn world-state->Grid
  "Builds a layer from the world state.
cell-draw-fn is a function that returns a Drawable."
  [cell-draw-fn]
;  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (drawable/->Grid
   (world/world-width)
   (world/world-height)
   (into {}
         (doall
          (for [c (world/all-cells)]
            [(world/coords c) (cell-draw-fn c)])))))

(defrecord Dot
  [v]
  drawable/Drawable
  (draw [this [w h]]
    (let [raw-color (vehicle/cargo-color (:v this))
          color (if raw-color
                  [(int (* (/ raw-color
                              (* 2 Math/PI))
                           256)) 255 255]
                  [255 0 255])]
      (quil/color-mode :hsb)
      (apply quil/fill color)
      (quil/ellipse
       (* 0.5 h)
       (* 0.5 w)
       (* 0.3 h)
       (* 0.3 w))
      (quil/color-mode :rgb))))

(defrecord Rect
  [color]
  drawable/Drawable
  (draw [this [w h]]
    (quil/color-mode :hsb)
    (apply quil/fill (:color this))
    (quil/rect
     (* 0 w)
     (* 0.2 h)
     (* 0.4 w)
     (* 0.4 h))
    (quil/color-mode :rgb)))

(defn vehicle->location
  [v]
  (let [stroke (util/vehicle->stroke v)
        p (util/vehicle->progress v)
        [x y] (points/point
               stroke p)]
    {:p' [(/ x (quil/width))
          (/ y (quil/height))]
     :rotation (points/tangent stroke p [0 1])}))

(defrecord Dot
  [v]
  drawable/Drawable
  (draw [this [w h]]
    (let [raw-color (vehicle/cargo-color (:v this))
          color (if raw-color
                  [(int (* (/ raw-color
                              (* 2 Math/PI))
                           256)) 255 255]
                  [255 0 255])]
      (quil/color-mode :hsb)
      (apply quil/fill color)
      (quil/ellipse
       (* 0.5 h)
       (* 0.5 w)
       (* 0.2 h)
       (* 0.2 w))
      (quil/color-mode :rgb))))

(defn moving-vehicle
  [v image]
  (let [{p' :p'
         rotation :rotation} (vehicle->location v)]
    (drawable/->Floating (drawable/->Stack [image
                                            (->Dot v)])
                         p'
                         (util/vehicle-scale)
                         rotation)))

(defn vehicles->Stack
  [vehicle-type image]
  (drawable/->Stack
   (vec
    (map (fn [v]
           (cond
            (vehicle/moving? v) (moving-vehicle v image)
            (vehicle/spawning? v) (drawable/->Nothing)
            (vehicle/despawning? v) (drawable/->Nothing)
            (vehicle/exploding? v) (drawable/->Nothing)))
     (vehicle/all-vehicles vehicle/truck?)))))

(defn world->drawable
  [tile-fn path-fn]
  (drawable/->Stack
   [
    (world-state->Grid (comp tile-fn cell-bg))
    (world-state->Grid path-fn)
    (vehicles->Stack :truck (tile-fn :truck))
    (world-state->Grid (partial cell-building tile-fn))
    ]))

(defn cell-bg [c]
  "What is the background tile-key for this cell?"
  (:background c))

(defn cell-building
  "Which building tile-key fits this cell?"
  [tile-fn c]
  (if-let [type (building/building-type c)]
    (case type
      :spawn (tile-fn
              (hyphenate-keywords :spawn (building/vehicle-type c)))
      :mixer (drawable/->Stack [(->Rect
                                  (color/hue->hsb
                                   (building/resource-color c)))
                                 (tile-fn :mixer)])
      :supply (drawable/->Stack [(->Rect
                                  (color/hue->hsb
                                   (building/resource-type c)))
                                 (tile-fn :snow)])
      :depot (drawable/->Stack [(->Rect
                                 (color/hue->hsb
                                  (building/resource-type c)))
                                (tile-fn :dirt)]))
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
  (let [path-fn (temp-lookup)]
    (reset! world-bricklet
            (drawable/->Bricklet
             (atom (drawable/->Border (reify drawable/Drawable
                                        (draw [this [w h]]
                                          (drawable/.draw
                                           (world->drawable
                                            tile-fn
                                            path-fn)
                                           [w h]))) 0.3 0.123))
             (atom [])
             :renderer :java2d
             :size [800 600]
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
