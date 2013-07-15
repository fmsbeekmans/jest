(ns jest.visualize.resource
  "Functions for adding, removing and searching for buildings."
  (:require
            [brick.drawable :as drawable]
            [quil.core :as quil]
            [jest.color :as color])

  (:use [jest.world :only [alter-cell all-cells cell]]
        [jest.vehicle :only [cargo-capacity cargo-color cargo-count]]
        [jest.world.building :only [resource-type resource-color resource-count]]))

(def soft-cap 10)

(defn- building-resource-rate-dispatch
  [c]
  (:building-type  c))

(def color-helper
  (comp color/hue resource-type))

(defmulti building-resource-rate
  building-resource-rate-dispatch)

(defmethod building-resource-rate :supply [c]
  [1.0 (color-helper c)])

(defmethod building-resource-rate :depot [c]
  (let [amount (:amount c 0)
        cap (:quotum c)]
    [(/ amount cap) (color-helper c)]))

(defmethod building-resource-rate :mixer [c]
  (let [color (resource-color c)
        count (resource-count c)]
    [(min 1.0 (/ count soft-cap))
     color]))

(defmethod building-resource-rate :default [c]
  [0 nil])

(defn vehicle-resource-rate
  [v]
  (let [color (cargo-color v)
        count (cargo-count v)
        cap (cargo-capacity (:type v))
        rate (/ count cap)]
    [rate color]))

(defn drawable-from-resource-rate [[rate color]]
  (let [rate (min rate 1.0)]
    (reify drawable/Drawable
      (draw [this [w h]]
        (let [w' (/ w 8)
              h' (/ h 6)
              r (* rate h')
              dy (- h' r)
              hsb (color/hue->hsb color)]
          (quil/color-mode :hsb)
          (quil/push-style)
          (apply quil/stroke hsb)
          (quil/no-fill)
          (quil/rect 0 0 w' h')
          (apply quil/fill hsb)
          (quil/rect 0 dy w' r)
          (quil/pop-style)
          (quil/color-mode :rgb)
          )))))
