(ns jest.visualize.util
  (:require [jest.util :refer [hyphenate-keywords]])
  (:require [jest.world :as world]
            [jest.vehicle :as vehicle]
            [jest.movement :as movement]
            [jest.world.cell :as cell]
            [jest.scheduler :as scheduler])
  (:require [jest.visualize.points :as points
                                   :refer [->Linear ->ComposedStroke ->Arc]])
  (:require [quil.core :as quil]
            [brick.drawable :as drawable]))

(def corner-rel 0.5)

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

(defn cell-points-p
  [c rel]
  (let [points (cell-points c)]
    (into points (vec (map (fn [center [dir border]]
                             (println border
                                      center)
                             [(hyphenate-keywords dir :p)
                              (points/.point
                               (->Linear center
                                         border) rel)])
                           (repeat (:center points))
                           (filter (fn [[dir _]]
                                     (not= dir :center)) points))))))

(defn absolute->relative
  ([point w h]
     (map /
          point
          [w h]))
  ([point]
     (absolute->relative point (quil/width) (quil/height))))

(defn vehicle->stroke-to-mid
  [v]
  (let [points (cell-points-p (world/cell (:coords v))
                              corner-rel)]
    (->Linear (points (hyphenate-keywords (:entry-direction v) :p))
              (points :center))))

(defn vehicle->stroke-from-mid
  [v]
  (let [points (cell-points-p (world/cell (:coords v))
                              corner-rel)]
    (comment (points (hyphenate-keywords (:exit-direction v) :p))
             (points :center))
    (->Linear (points :center)
              (points (hyphenate-keywords (:exit-direction v) :p)))))



(def vehicle->stroke
  (memoize
   (fn [v s]
     (cond
      (vehicle/spawning? v) (vehicle->stroke-from-mid v)
      (vehicle/despawning? v) (vehicle->stroke-to-mid v)
      (vehicle/exploding? v) (vehicle->stroke-to-mid v)
      (vehicle/moving? v) (->ComposedStroke
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
