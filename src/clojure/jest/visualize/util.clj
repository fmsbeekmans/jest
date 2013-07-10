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

(defn absolute->relative
  ([point w h]
     (map /
          point
          [w h]))
  ([point]
     (absolute->relative point (quil/width) (quil/height))))

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

(def vehicle->stroke
  (memoize
   (fn [v s]
     (cond
      (vehicle/spawning? v) (vehicle->stroke-from-mid v)
      (vehicle/despawning? v) (vehicle->stroke-to-mid v)
      (vehicle/exploding? v) (vehicle->stroke-to-mid v)
      (vehicle/moving? v) (points/stroke-comp
                           [(vehicle->stroke-to-mid v)
                            (vehicle->stroke-from-mid v)])))))

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

(let [get-frame-atom (comp :target-obj meta)
      sketch-decorator
      (fn [sketch decorate]
        (let [frame-atom (get-frame-atom sketch)]
          (println frame-atom)
          (swap! frame-atom
                 #(doto %
                    (.dispose)
                    (.setUndecorated (not decorate))
                    (.setVisible true)))))]
  (defn decorate-sketch
    [sketch]
    (sketch-decorator sketch true))
  (defn undecorate-sketch
    [sketch]
    (sketch-decorator sketch false))
  (defn get-frame-offset
    [sketch]
    (let [frame @(get-frame-atom sketch)]
      [(.getX frame)
       (.getY frame)]))
  (defn get-pane-offset
    [sketch]
    (let [frame @(get-frame-atom sketch)
          root-pane (.getRootPane frame)]
      [(.getX root-pane)
       (.getY root-pane)])))
