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
            [brick.drawable :as drawable])
  (:require [clojure.algo.generic.math-functions :as m]))

(def corner-rel 0.5)

(def dirs
  {:east 0
   :north 1
   :west 2
   :south 3})

(defn rel-direction
  [from to]
  (case (mod (- (dirs from)
                (dirs to)) 4)
    0 :reverse
    1 :clock-wise
    2 :straight
    3 :counter-clock-wise))

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

(defn points->stroke
  ([points from to]
   (->Linear (points from)
             (points to))))

(defn vehicle->stroke-to-mid
  [v]
  (let [points (cell-points (world/cell (:coords v)))]
    (points->stroke points
                    (:entry-direction v)
                    :center)))

(defn vehicle->stroke-from-mid
  [v]
  (let [points (cell-points (world/cell (:coords v)))]
    (points->stroke points
                    :center
                    (:exit-direction v))))


(defn arc-center
  [points from to]
  (let [get-fn (if (or (= from :south)
                       (= from :south-p)
                       (= from :north)
                       (= from :north-p))
                    [second first]
                    [first second])]
    (vec (map
          (fn [get-fn point]
            (get-fn point))
          get-fn
          [(points from)
           (points to)]))))

(defn vehicle->rel-direction
  [v]
  (apply rel-direction ((juxt :entry-direction :exit-direction) v)))

(defn vehicle->arc
  [v]
  (let [points (cell-points-p (world/cell (:coords v)) corner-rel)
         from-key (hyphenate-keywords (:entry-direction v) :p)
         to-key (hyphenate-keywords (:exit-direction v) :p)
         rel-dir (rel-direction (:entry-direction v)
                                (:exit-direction v))]
    (->ComposedStroke
     [(points->stroke points
                      (:entry-direction v)
                      (hyphenate-keywords (:entry-direction v) :p))
      (->Arc
       (arc-center points
                   from-key
                   to-key)
       (- (first (points :east-p))
          (first (points :center)))
       (* (dirs (:entry-direction v)) 0.5 Math/PI)
       (* (dirs (:exit-direction v)) 0.5 Math/PI)
       rel-dir)
      (points->stroke points
                      (hyphenate-keywords (:exit-direction v) :p)
                      (:exit-direction v))])))

(defn vehicle->straight
  [v]
  (let [points (cell-points (world/cell (:coords v)))]
    (points->stroke points
                    (:entry-direction v)
                    (:exit-direction v))))

(def vehicle->stroke
  (memoize
   (fn [v _]
     (cond
      (vehicle/spawning? v) (vehicle->stroke-from-mid v)
      (vehicle/despawning? v) (vehicle->stroke-to-mid v)
      (vehicle/exploding? v) (vehicle->stroke-to-mid v)
      (vehicle/moving? v) (case (vehicle->rel-direction v)
                            :straight (vehicle->straight v)
                            :clock-wise (vehicle->arc v)
                            :counter-clock-wise (vehicle->arc v)
                            :reverse (vehicle->straight v))))))

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
