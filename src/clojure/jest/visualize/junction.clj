(ns jest.visualize.junction
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)
  (:use [clojure.core.match :only [match]])
  (:require [brick.image :as image]
            [brick.drawable :as drawable])
  (:require [jest.world.path :as path]))

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
           [[:north :in]] :road-end-s
           [[:west :in]] :road-end-e
           [[:south :in]] :road-end-n
           [[:east :in]] :road-end-w

;           [[:north :in] [:south :in]] :road-blocked
;           [[:west :in] [:east :in]] :road-blocked
           ;; double
           [[:north _] [:west _]] :turn-nw
           [[:north _] [:south _]] :road-s
           [[:north _] [:east _]] :turn-ne

           [[:north _] [:west _]] :turn-nw
           [[:north _] [:south _]] :road-n
           [[:north _] [:east _]] :turn-ne

           [[:west _] [:south _]] :turn-sw
           [[:west _] [:east _]] :road-e

           [[:west _] [:south _]] :turn-sw
           [[:west _] [:east _]] :road-w

           [[:south _] [:east _]] :turn-se

           [[:south _] [:east _]] :turn-se

           ;; triple
           [[:north _] [:west _] [:south _]] :cross-t-e
           [[:north _] [:south _] [:east _]] :cross-t-w
           [[:north _] [:west _] [:east _]] :cross-t-s
           [[:west _] [:south _] [:east _]] :cross-t-n

           ;; quatro
           [[:north _] [:west _] [:south _] [:east _]] :cross
           :else nil)))

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

          ren (loader "road-end-n.png")
          rew (loader "road-end-w.png")
          res (loader "road-end-s.png")
          ree (loader "road-end-e.png")

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
           :road-end-n ren
           :road-end-w rew
           :road-end-s res
           :road-end-e ree
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
