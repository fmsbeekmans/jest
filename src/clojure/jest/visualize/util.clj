(ns jest.visualize.util
  (:require [jest.world :as world]
            [jest.vehicle :as vehicle]
            [jest.movement :as movement]
            [jest.world.cell :as cell]
            [jest.scheduler :as scheduler])
  (:require [jest.visualize.points :as points])
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

(defn vehicle->stroke-to-mid
  [v]
  (let [points (cell-points (world/cell (:coords v)))]
    (points/stroke (points (:entry-direction v))
                   (points :center))))

(defn vehicle->stroke-from-mid
  [v]
  (let [points (cell-points (world/cell (:coords v)))]
    (points/stroke (points :center)
                   (points (:exit-direction v)))))

(defn vehicle->stroke
  [v]
  (cond
   (vehicle/spawning? v) (vehicle->stroke-to-mid v)
   (vehicle/despawning? v) (vehicle->stroke-from-mid v)
   (vehicle/exploding? v) (vehicle->stroke-from-mid v)
   (vehicle/moving?) (points/stroke-comp [(vehicle->stroke-to-mid v)
                                          (vehicle->stroke-from-mid v)])))

(defn vehicle-scale
  "What scale should a vehicle-tile be scaled by?
Returns an x-scale y-scale vector."
  []
  (/ 1 (world/world-width)))

(defn vehicle->progress [v]
  (let [duration (- (:exit-time v)
                    (:entry-time v))
        elapsed (- @scheduler/game-time
                   (:entry-time v))]
    (cond (>= elapsed duration) 1
          (<= elapsed 0) 0
          :default (/ elapsed
                      duration))))
