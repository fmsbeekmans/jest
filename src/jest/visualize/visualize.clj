(ns jest.visualize.visualize
  (:require [jest.world.path :as path])
  (:require [jest.world.cell :as cell])
  (:require [brick.image :as image])
  (:require [brick.drawable :as drawable]))

; TODO
(defn tile-lookup [k]
  {:bg ()}
  )


(defn layer-from-world-state
  "Builds a layer from the world state. cell-draw-fn is a function that returns a Drawable."
  [cell-draw-fn]
  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (drawable/->Grid (cell/world-width) (cell/world-height)
                   (into {} (for [c (cell/all-cells)]
                              [(cell/coords c) (cell-draw-fn c)]))))
    ))

(defn vehicle->location-fn
  #^{:doc (str "Return a location-fn of a vehicle.\n"
               "The location-fn is a 1-arity fn where it's input is the progres, "
               "0" is the start of the animation and 1 the end.)}
  [v]
  (let [start [1 1]
        end [1 -1]
        mid [0 0]]
    (fn [progress]
      {:pre [(>= progress 0)
             (<= progress 1)]}
      (let [[sub-line sub-progress]
            (if (< progress 1/2)
              [(line start mid) (* 2 progress)]
              [(line mid end)   (dec (* 2 progress))])]
        (sub-line sub-progress)))))

(comment
  {:coord [0 0],
   :paths {},
   :background :none,
   :building-type nil,
   :vehicle-type nil,
   :resource-type nil})

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
