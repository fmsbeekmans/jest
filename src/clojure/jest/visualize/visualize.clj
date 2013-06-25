(ns jest.visualize.visualize
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)

  (:require [brick.image :as image])
  (:require [brick.drawable :as drawable])
  (:require [jest.world :as world])
  (:require [jest.movement :as movement])
  (:require [jest.world.building :as building])
  (:require [jest.visualize.points :as points])
  (:require [jest.vehicle :as vehicle])
  (:require [jest.world.path :as path])
  (:require [jest.world.cell :as cell])
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
            [(world/coords c)  (cell-draw-fn c)])))))

(defn vehicle-scale
  "What scale should a vehicle-tile be scaled by?
Returns an x-scale y-scale vector."
  []
  (/ 1 (world/world-width)))

(defn cell-borders
  [c]
  (let [h-range ((drawable/ranges
                  (world/world-width)
                  (quil.core/width)) ((world/coords c) 0))
        v-range ((drawable/ranges
                  (world/world-height)
                   (quil.core/height)) ((world/coords c) 1))]
    {:north (first v-range)
     :west (first h-range)
     :south (apply + v-range)
     :east (apply + h-range)}))

(defn cell-center
  "Returns the center of a cell."
  [c]
  (let [h-range ((drawable/ranges
                  (world/world-width)
                  (quil.core/width)) ((world/coords c) 0))
        v-range ((drawable/ranges
                  (world/world-height)
                  (quil.core/height)) ((world/coords c) 1))]
    [(/ (apply + h-range) 2)
     (/ (apply + v-range) 2)]))

(defn v-point
  [center borders dir]
  [(first center) (borders dir)])

(defn h-point
  [center borders dir]
  [(borders dir) (second center)])

(defn point-from-center
  [center borders dir]
  ((if (or
         (= dir :east)
         (= dir :west))
      h-point
      v-point) center borders dir))

;; from en to in coords zijn de cell-coordinaten, dit moeten de game
;; coordinaten zijn
(defn vehicle->stroke
  "Geef dirs op"
  [v coords from to]
  (cond
   (vehicle/spawning? (:id v))
   (do
     (points/stroke (coords :center)
                    (coords :to)))
    (vehicle/moving? (:id v))
    (do
      (let [l1 (points/stroke (point-from-center (coords :center)
                                                 (coords :borders)
                                                 :north) ;;; from
                              (coords :center))
            l2 (points/stroke (coords :center) ; goede coord v/h
                              (point-from-center (coords :center)
                                                 (coords :borders)
                                                 :east))] ;;; to
        (points/stroke-comp [l1 l2])))
   (vehicle/despawning? (:id v))
   (do
     (points/stroke (coords :center)
                    (coords :to)))))

(defn vehicle->location
  [v]
  (let [coords (:coords v)
        borders (cell-borders (world/cell coords))
        center (cell-center (world/cell coords))
        stroke (vehicle->stroke
                v
                {:center center
                 :to coords
                 :borders borders}
                (:entry-direction v)
                (:exit-direction v))
        t 0
        ;;(/ (- (:exit-)))
        ;; duur
        ;; game-time - entry-time
           ]
    (let [[x y] (points/point stroke t)]
      [(/ x (quil.core/width))
       (/ y (quil.core/height))])))

(defn vehicles->Stack
  [vehicle-type image]
  (drawable/->Stack
   (vec
    (map (fn [v]
           (drawable/->Floating image
                                (vehicle->location v)
                                (vehicle-scale) Math/PI))
     (vehicle/all-vehicles vehicle/truck?)))))

(defn vehicle-picker [cell]
  (if-let [v (first (vehicle/vehicles cell))]
    (or (vehicle/cargo-color v)
        :no-cargo)))

(defn draw-vehicle [color w h]
  (when color
    (quil.core/color-mode :hsb)
    (if (= color :no-cargo)
      (quil.core/fill 0 0 0)
      (quil.core/fill (* color
                         (/ (* 2 Math/PI))
                         255)
                      255
                      255))
    (quil.core/ellipse 32 32 20 20)
    (quil.core/color-mode :rgb)))


(defn vehicle-f [color]
  (reify brick.drawable.Drawable
    (draw [this [w h]]
      (draw-vehicle color w h))))

(defn world->drawable
  [bg-fn building-fn path-fn vehicle-fn]
  (drawable/->Stack
   [
;    (world-state->Grid cell-bg tile-f)
    ;(world-state->Grid cell-road tile-f)
    (world-state->Grid path-fn)
    (world-state->Grid vehicle-fn)
    (world-state->Grid building-fn)
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

(defn cell-canal
  ""
  [c]
    (if (seq (path/canals c))
      (drawable/->Image (image/path->PImage (clojure.java.io/resource "canal.png")))
      (drawable/->Nothing)))

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

(defn cell-rails [c]
  (if (seq (filter (fn [[_ v]]
                (= :rails (:type v))) (:paths c)))
    (drawable/->Image (image/path->PImage (clojure.java.io/resource "rails.png")))
    (drawable/->Nothing)))

(defn world->sketch []
  (reify drawable/Drawable
    (draw [this [w h]]
      ;maak een drawable van de wereld
      ;teken deze
      (.draw (world->drawable) [w h]))))

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
                         (comp vehicle-f vehicle-picker))
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
