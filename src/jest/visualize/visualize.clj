(ns jest.visualize.visualize
  (:require [brick.image :as image])
  (:require [brick.drawable :as drawable])
  (:require [jest.world :as world])
  (:require [jest.visualize.points :as points])
  (:require [jest.vehicle :as vehicle])
  (:require [jest.world.path :as path])
  (:require [jest.world.cell :as cell]))

(declare cell-bg)
(declare cell-building)
(declare cell-road)

; TODO
(defn tile-lookup [k]
  {:bg ()}
  )

(defn world-state->Grid
  "Builds a layer from the world state. cell-draw-fn is a function that returns a Drawable."
  [cell-draw-fn]
  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (drawable/->Grid (world/world-width) (world/world-height)
                   (into {} (for [c (world/all-cells)]
                              [(world/coords c) (cell-draw-fn c)]))))

(defn vehicles->Stack
  [vehicles-fn
   vehicle-draw-fn]
  (comment
    (vec Stack (map (fn [vehicle])))))

(defn world->drawable
  []
  (drawable/->Stack
   [(world-state->Grid cell-bg)
    (world-state->Grid cell-building)
    (world-state->Grid cell-road)
    (vehicles->Stack (vec
                      (filter (vehicle/vehicles vehicle/truck?)))
)]))

(defn cell-bg [c]
  (image/path->PImage (clojure.java.io/resource "grass.png")))

(defn cell-canal [c]
  (if (seq (path/canals c))
    (drawable/->Image (image/path->PImage (clojure.java.io/resource "canal.png")))
    (drawable/->Nothing)))


(defn cell-boat [c]
  (image/path->PImage (clojure.java.io/resource "boat.png")))

(defn cell-road [c]
  (if (seq (filter (fn [[_ v]]

                (= :road (:type v))) (:paths c)))
    (drawable/->Image (image/path->PImage (clojure.java.io/resource "road.png")))
    (drawable/->Nothing)))

(defn cell-truck [c]
  (image/path->PImage (clojure.java.io/resource "truck.png")))

(defn cell-rails [c]
  (if (seq (filter (fn [[_ v]]
                (= :rails (:type v))) (:paths c)))
    (drawable/->Image (image/path->PImage (clojure.java.io/resource "rails.png")))
    (drawable/->Nothing)))


(defn cell-train [c]
  (image/path->PImage (clojure.java.io/resource "train.png")))

(defn cell-buildings [c]
  (image/path->PImage (clojure.java.io/resource "buildings.png")))

(defn cell-explosions [c]
  (image/path->PImage (clojure.java.io/resource "explosions.png")))

(defn world->sketch []
  (reify drawable/Drawable
    (draw [this [w h]]
      ;maak een drawable van de wereld
      ;teken deze
      (.draw (world->drawable) [w h]))))

(defn setup [tile-f])
