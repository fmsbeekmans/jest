(ns jest.visualize.util
  (:require [jest.world :as world]
            [jest.vehicle :as vehicle]
            [jest.movement :as movement]
            [jest.world.cell :as cell])
  (:require [quil.core :as quil]
            [brick.drawable :as drawable]))

(defn cell-points [c]
  (let [coord (:coord c)
        h-range ((drawable/ranges
                  (world/world-width)
                  (quil/width))
                 (first coord))
        v-range ((drawable/ranges
                  (world/world-height)
                  (quil/height))
                 (second coord))
        center [(+ (first h-range) (* 0.5 (second h-range)))
                (+ (first v-range) (* 0.5 (second v-range)))]]
    {:center center
     :north [(first center)
             (first v-range)]
     :east [(apply + h-range)
            (second center)]
     :south [(first center)
            (apply + v-range)]
     :west [(first h-range)
             (second center)]}))

(defn vehicle->stroke [v])

(defn vehicle->progress [v])
