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
  (:require [jest.world.cell :as cell]))

(declare cell-bg)
(declare cell-building)
(declare cell-road)

(def world-sketch (atom nil))

(defn world-state->Grid
  "Builds a layer from the world state.
cell-draw-fn is a function that returns a Drawable."
  [cell-f tiles-f]
;  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (drawable/->Grid
   (world/world-width)
   (world/world-height)
   (into {}
         (doall
          (for [c (world/all-cells)]
            [(world/coords c) (tiles-f (cell-f c))])))))

(defn vehicle-scale
  "What scale should a vehicle-tile be scaled by?
Returns an x-scale y-scale vector."
  []
;  (/ 1 (world/world-width))
  1)

(defn vehicles->Stack
  [vehicle-type image]
  (drawable/->Stack
   (vec
    (map (fn [v]
           (drawable/->Floating image [0.5 0.5] (vehicle-scale) 0))
     (vehicle/all-vehicles vehicle/truck?)))))

(defn world->drawable
  [tile-f]
  (drawable/->Stack
   [
    (world-state->Grid cell-bg tile-f)
    (world-state->Grid cell-building tile-f)
    (world-state->Grid cell-road tile-f)
;    (vehicles->Stack :truck (tile-f :rails-east))
    ]))

(defn cell-bg [c]
  "What is the background tile-key for this cell?"
  (:background c))

(defn cell-building
  "Which building tile-key fits this cell?"
  [c]
  (if-let [type (building/building-type c)]
    (hyphenate-keywords
     type
     (({:spawn building/vehicle-type
        :mixer building/resource-color
        :supply building/resource-type
        :depot building/resource-type} type) c))))

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
  (reset! world-sketch
          (drawable/->Bricklet
           (atom (reify drawable/Drawable
                   (drawable/draw [this [w h]]
                     (drawable/.draw
                      (world->drawable
                       tile-f)
                      [w h]))))
           (atom []))))
