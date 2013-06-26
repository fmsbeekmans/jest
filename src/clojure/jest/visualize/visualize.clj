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
            [jest.visualize.util :as util])
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
          cached-stack (memoize drawable/->Stack)]
      (fn [c]
        (let [roads (path/paths c :road)]
          (cached-stack
           (vec
            (doall
             (for [r roads]
                 (junctions [(:direction r)
                             (:inout r)]))))))))))


(def world-bricklet (atom nil))
(def world-sketch (atom nil))

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

(defn vehicle->location
  [v]
  (let [[x y] (points/point
            (util/vehicle->stroke v)
            (util/vehicle->progress v))]
    (println [(/ x (quil/width))
              (/ y (quil/height))])
    [(/ x (quil/width))
     (/ y (quil/height))]))

(defn vehicles->Stack
  [vehicle-type image]
  (drawable/->Stack
   (vec
    (map (fn [v]
           (drawable/->Floating image
                                (vehicle->location v)
                                (vehicle-scale) Math/PI))
     (vehicle/all-vehicles vehicle/truck?)))))

(defn world->drawable
  [bg-fn building-fn path-fn vehicle-fn]
  (drawable/->Stack
   [
;    (world-state->Grid cell-bg tile-f)
    ;(world-state->Grid cell-road tile-f)
    (world-state->Grid path-fn)
    (world-state->Grid building-fn)
    (vehicles->Stack :truck (vehicle-fn :rails-east))
    ]))

(defn cell-bg [c]
  "What is the background tile-key for this cell?"
  (:background c))

(defn cell-building
  "Which building tile-key fits this cell?"
  [c]
  (if-let [type (building/building-type c)]
    (if (= type :mixer)
      :mixer
      (hyphenate-keywords
       type
       (({:spawn building/vehicle-type
          ;:mixer building/resource-color
          :supply building/resource-type
          :depot building/resource-type} type) c)))))

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

(defn setup [tile-f]
  ;init een bricklet met tile-set
  (let [path-fn (temp-lookup)]
    (reset! world-bricklet
            (drawable/->Bricklet
             (atom (reify drawable/Drawable
                     (drawable/draw [this [w h]]
                       (drawable/.draw
                        (world->drawable
                         (comp tile-f cell-bg)
                         (comp tile-f cell-building)
                         path-fn
                         tile-f)
                        [w h]))))
             (atom [])
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
