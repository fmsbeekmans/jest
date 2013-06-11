(ns jest.visualize.visualize
  (:require [brick.image :as image])
  (:require [brick.drawable :as drawable])
  
  (:require [jest.visualize.points :as points])
  (:require [jest.world.path :as path])
  (:require [jest.world.cell :as cell]))


; TODO
(defn tile-lookup [k]
  {:bg ()}
  )

(defn world-state->Grid
  "Builds a layer from the world state. cell-draw-fn is a function that returns a Drawable."
  [cell-draw-fn]
  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (drawable/->Grid (cell/world-width) (cell/world-height)
                   (into {} (for [c (cell/all-cells)]
                              [(cell/coords c) (cell-draw-fn c)]))))

(defn visualize-world-state
  [world-state]
  (comment
    (->Stack (vec (juxt ...) worldstate)))
  )

(defn vehicles->Stack
  [world-statke]
  (comment
    (-> Stack (vec (map
                    (fn [vehicle]
                      ))))))

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
