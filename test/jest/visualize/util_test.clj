(ns jest.visualize.util
  (:use [midje.sweet]
        [jest.testutils :only [world-fact vec-roughly]])
  (:require [jest.world :as world]
            [jest.world.vehicle :as vehicle]
            [jest.scheduler :as scheduler])
  (:require [jest.visualize.util :as v-util]
            [jest.visualize.points :as points])
  (:require [quil.core :as quil]))

(with-redefs [quil/width (fn []
                           300)
              quil/height (fn []
                            400)]
  (world-fact [5 7]
    "cell-points returns the correct points."
    (cell-points (world/cell [1 3]))
      => {:center [90.0 200.5]
          :north [90.0 172]
          :east [120 200.5]
          :south [90.0 229]
          :west [60 200.5]})

  (world-fact [5 9] "vehicle->stroke-to-mid"
    (let [v-loc [2 4]
          v (vehicle/->Vehicle nil nil v-loc nil :east
                               nil :south nil nil)
          stroke (vehicle->stroke-to-mid v)
          cell-points (v-util/cell-points (world/cell v-loc))]
      (points/start-point stroke)
        => (vec-roughly (cell-points :east) 0.3)
      (points/end-point stroke)
        => (vec-roughly (cell-points :center) 0.3)))

  (world-fact [5 9] "vehicle->stroke-from-mid"
    (let [v-loc [2 4]
          v (vehicle/->Vehicle nil nil v-loc nil :north
                               nil :west nil nil)
          stroke (vehicle->stroke-from-mid v)
          cell-points (v-util/cell-points (world/cell v-loc))]
      (points/start-point stroke)
        => (vec-roughly (cell-points :center) 0.3)
      (points/end-point stroke)
      => (vec-roughly (cell-points :west) 0.3)))

  (facts "vehicle->stroke"
    (fact "A spawning vehicle goes from mid"
      (let [v-loc [2 4]
            v (vehicle/->Vehicle nil nil v-loc nil nil
                                 nil :west nil :spawning)
            cell-points (v-util/cell-points (world/cell v-loc))
            stroke (v-util/vehicle->stroke v)]
        (points/start-point stroke)
          => (vec-roughly (cell-points :center) 0.3)
        (points/end-point stroke)
          => (vec-roughly (cell-points :west) 0.3)))
    
    (fact "A despawning vehicle goes to mid"
      (let [v-loc [2 4]
            v (vehicle/->Vehicle nil nil v-loc nil :south
                                 nil nil nil :despawning)
            cell-points (v-util/cell-points (world/cell v-loc))
            stroke (v-util/vehicle->stroke v)]
        (points/start-point stroke)
          => (vec-roughly (cell-points :south) 0.3)
        (points/end-point stroke)
          => (vec-roughly (cell-points :center) 0.3)))
    
    (fact "An exploding vehicle goes to mid"
      (let [v-loc [2 4]
            v (vehicle/->Vehicle nil nil v-loc nil :south
                                 nil nil nil :exploding)
            cell-points (v-util/cell-points (world/cell v-loc))
            stroke (v-util/vehicle->stroke v)]
        (points/start-point stroke)
          => (vec-roughly (cell-points :south) 0.3)
        (points/end-point stroke)
          => (vec-roughly (cell-points :center) 0.3)))
    
    (fact "A moving vehicle goes from entry to exit"
      (let [v-loc [2 4]
            v (vehicle/->Vehicle nil nil v-loc nil :south
                                 nil :north nil :moving)
            cell-points (v-util/cell-points (world/cell v-loc))
            stroke (v-util/vehicle->stroke v)]
        (points/start-point stroke)
          => (vec-roughly (cell-points :south) 0.3)
        (points/start-point stroke)
          => (vec-roughly (cell-points :south) 0.3)))))

(with-redefs [scheduler/game-time (atom 20)]
  (facts "vehicle->progress"
    (fact "Can't be more than 1"
      (v-util/vehicle->progress
       (vehicle/->Vehicle nil nil nil 18 :south
                          19 :north nil :moving))
      => 1)
    
    (fact "Can't be less than 0"
      (v-util/vehicle->progress
       (vehicle/->Vehicle nil nil nil 21 :south
                          23 :north nil :moving))
      => 0)
    
    (fact "Normal behaviour"
      (v-util/vehicle->progress
       (vehicle/->Vehicle nil nil nil 11 :south
                          23 :north nil :moving))
      => (roughly 3/4 0.01))))
