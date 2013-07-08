(ns jest.visualize.visualize
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)
  (:use [clojure.core.match :only (match)])


  (:require [brick.image :as image]
            [brick.drawable :as drawable]
            [quil.core :as quil])
  (:require [jest.world :as world]
            [jest.movement :as movement]
            [jest.world.building :as building]
            [jest.vehicle :as vehicle]
            [jest.world.path :as path]
            [jest.world.cell :as cell])
  (:require [jest.visualize.points :as points]
            [jest.visualize.util :as util]
            [jest.visualize.resource :as resource]
            [jest.color :as color])
  (:require [jest.visualize.input :as input]))

(declare cell-bg)
(declare cell-building)
(declare cell-road)

(defn replace-fn [p v]
  (fn [x]
    (if (p x) v x)))

(defn arrow [cx cy angle colors]
  (let [amount (count colors)]
    (drawable/->Floating
     (reify drawable/Drawable
       (draw [this [w h]]
         (quil/with-translation [(* w 5/8)
                                 (/ h 2)]
           (quil/push-style)
           (quil/stroke 255)
           ;;(quil/smooth)
           (let [len (/ w 4)
                 fat (/ h 12)]
             (quil/stroke-weight (/ len 12))
             (quil/line 0 0 len 0)
             (quil/line len 0 (- len fat) (quil/ceil (- fat)))
             (quil/line len 0 (- len fat) (quil/ceil fat))
             (quil/color-mode :hsb)
             (loop [ls (drawable/ranges (max amount fat) len)
                    cs colors]
               (if (and (seq ls) (seq cs))
                 (let [[offset l] (first ls)
                       endp (+ offset l)
                       c (first cs)]
                   (apply quil/stroke (color/hue->hsb c))
                   (quil/line offset 0 (- endp fat) (quil/ceil (- fat)))
                   (quil/line offset 0 (- endp fat) (quil/ceil fat))
                   (recur (rest ls)
                          (rest cs)))))
             (quil/color-mode :rgb))

           (quil/pop-style))))
     [cx cy] 1 angle)))

(def direction-order {:north 1 ;;first
                        :west 2
                        :south 3
                        :east 4})

(defn match-roads [roads]
  (let [sorted-roads (sort-by (comp direction-order :direction) roads)
        road-tuples (map (juxt :direction :inout) sorted-roads)]
    (match (vec road-tuples)
           [[:north :out]] :road-n
           [[:west :out]] :road-w
           [[:south :out]] :road-s
           [[:east :out]] :road-e
           [[:north :in]] :road-s
           [[:west :in]] :road-e
           [[:south :in]] :road-n
           [[:east :in]] :road-w

           [[_ :in] [_ :in]] :road-blocked
           ;; double
           [[:north :in] [:west :out]] :turn-nw
           [[:north :in] [:south :out]] :road-s
           [[:north :in] [:east :out]] :turn-ne

           [[:north :out] [:west :in]] :turn-nw
           [[:north :out] [:south :in]] :road-n
           [[:north :out] [:east :in]] :turn-ne

           [[:west :in] [:south :out]] :turn-sw
           [[:west :in] [:east :out]] :road-e

           [[:west :out] [:south :in]] :turn-sw
           [[:west :out] [:east :in]] :road-w

           [[:south :in] [:east :out]] :turn-se

           [[:south :out] [:east :in]] :turn-se

           ;; triple
           [[:north _] [:west _] [:south _]] :cross-t-e
           [[:north _] [:south _] [:east _]] :cross-t-w
           [[:north _] [:west _] [:east _]] :cross-t-s
           [[:west _] [:south _] [:east _]] :cross-t-n

           ;; quatro
           [[:north _] [:west _] [:south _] [:east _]] :cross
           :else nil)))

(let [cardinal-arrow (memoize (partial arrow 0.5 0.5))]
  (defn paths-to-arrows [c]
    (let [*pi (partial * Math/PI)
          dir-to-radian  {:north (*pi 3/2)
                          :east (*pi 0)
                          :south (*pi 1/2)
                          :west (*pi 1)}
          out-p (path/out-paths c)]
      (drawable/->Stack
       (vec (map
             cardinal-arrow
             (map dir-to-radian
                  (map :direction out-p))
             (map :routes out-p)))))))

(defn nice-lookup []
  (let [loader (comp
                drawable/->Image
                image/path->PImage
                clojure.java.io/resource
                (partial str "junction/road/"))]
    (let [rn (loader "road-n.png")
          rw (loader "road-w.png")
          rs (loader "road-s.png")
          re (loader "road-e.png")
          rb (drawable/->Nothing)

          tnw (loader "turn-nw.png")
          tne (loader "turn-ne.png")
          tse (loader "turn-se.png")
          tsw (loader "turn-sw.png")

                ctn (loader "cross-t-n.png")
          ctw (loader "cross-t-w.png")
          cts (loader "cross-t-s.png")
          cte (loader "cross-t-e.png")

          cross (loader "cross.png")
          junctions
          {:road-n rn
           :road-w rw
           :road-s rs
           :road-e re
           :road-blocked rb
           :turn-nw tnw
           :turn-ne tne
           :turn-se tse
           :turn-sw tsw
           :cross-t-n ctn
           :cross-t-w ctw
           :cross-t-s cts
           :cross-t-e cte
           :cross cross}]
      (fn [c]
        (let [roads (path/paths c :road)
              n (drawable/->Nothing)]
           (get junctions (match-roads roads) n))))))






(defonce world-bricklet (atom nil))
(defonce world-sketch (atom nil))



(defn world-state->Grid
  "Builds a layer from the world state.
cell-draw-fn is a function that returns a Drawable."
  [cell-draw-fn]
;  {:post [(every? drawable/drawable? (vals (:grid %)))]}
  (drawable/->Grid
   (world/world-width)
   (world/world-height)
   (into {}
         (doall
          (for [c (world/all-cells)]
            [(world/coords c) (cell-draw-fn c)])))))


(defrecord Rect
  [color]
  drawable/Drawable
  (draw [this [w h]]
    (quil/color-mode :hsb)
    (apply quil/fill (:color this))
    (quil/rect
     (* 0 w)
     (* 0.2 h)
     (* 0.4 w)
     (* 0.4 h))
    (quil/color-mode :rgb)))

(defn vehicle->location
  [v]
  (let [stroke (util/vehicle->stroke v [(quil/width)
                                        (quil/height)])
        p (util/vehicle->progress v)
        [x y] (points/point
               stroke p)]
    {:p' [(/ x (quil/width))
          (/ y (quil/height))]
     :rotation (points/tangent stroke p [0 1])}))

(defn moving-vehicle
  [v image]
  (let [{p' :p'
         rotation :rotation} (vehicle->location v)]
    (drawable/->Stack [(drawable/->Floating image
                                            p'
                                            (util/vehicle-scale)
                                            rotation)
                       (drawable/->Floating (resource/drawable-from-resource-rate
                                             (resource/vehicle-resource-rate v))
                                            p'
                                            (util/vehicle-scale)
                                            0)])))

(defn vehicles->Stack
  [vehicle-type image]
  (drawable/->Stack
   (vec
    (map (fn [v]
           (cond
            (vehicle/moving? v) (moving-vehicle v image)
            ;(vehicle/moving? v) (moving-vehicle-resource v)
            (vehicle/spawning? v) (drawable/->Nothing)
            (vehicle/despawning? v) (drawable/->Nothing)
            (vehicle/exploding? v) (drawable/->Nothing)))
     (vehicle/all-vehicles vehicle/truck?)))))

(defn world->drawable
  [tile-fn path-fn]
  (drawable/->Stack
   [
    (world-state->Grid (comp tile-fn (constantly :grass)))
    (world-state->Grid path-fn)
    (vehicles->Stack :truck (tile-fn :truck))
    (world-state->Grid paths-to-arrows)
    (world-state->Grid (partial cell-building tile-fn))
    ]))

(defn cell-bg [c]
  "What is the background tile-key for this cell?"
  (:background c))

(defn cell-building
  "Which building tile-key fits this cell?"
  [tile-fn c]
  (if-let [type (building/building-type c)]
    (case type
      :spawn (tile-fn
              (hyphenate-keywords :spawn (building/vehicle-type c)))
      :mixer (drawable/->Stack [(->Rect
                                  (color/hue->hsb
                                   (building/resource-color c)))
                                 (tile-fn :mixer)])
      :supply (drawable/->Stack [(->Rect
                                  (color/hue->hsb
                                   (building/resource-type c)))
                                 (tile-fn :supply-red)])
      :depot (drawable/->Stack [(->Rect
                                 (color/hue->hsb
                                  (building/resource-type c)))
                                (tile-fn :dirt)]))
    (tile-fn nil)))

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

(defn setup [tile-fn]
  ;init een bricklet met tile-set
  (let [path-fn (nice-lookup)]
    (reset! world-bricklet
            (drawable/->Bricklet
             (atom (drawable/->Border (reify drawable/Drawable
                                        (draw [this [w h]]
                                          (drawable/.draw
                                           (world->drawable
                                            tile-fn
                                            path-fn)
                                           [w h]))) 0 0))
             (atom [])
             :renderer :java2d
             :size [800 600]
             :mouse-pressed input/on-down-handler
             :mouse-released input/on-up-handler
             :mouse-dragged input/on-move-handler))))

(defn sketch! []
  (reset! world-sketch (drawable/drawable->sketch! @world-bricklet)))

(defn sketch-width []
  (.getWidth @world-sketch))

(defn sketch-height []
  (.getHeight @world-sketch))

(defn sketch-size []
  [(sketch-width) (sketch-height)])
