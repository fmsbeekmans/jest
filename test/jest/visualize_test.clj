(ns jest.visualize-test
  (:require [jest.monocle :as monocle]
            [brick.drawable :as drawable]
            [jest.world.building :as building])
  (:use jest.testutils))

(world-fact [10 10]
            "layer from world state builds a valid grid drawable"
            (build-spawn-circle)
            (let [grid (monocle/layer-from-world-state (fn [c]
                                                         (if (building/spawn? c)
                                                           (drawable/->Stack [])
                                                           (drawable/->Nothing))))]
              (get-in grid [:grid [5 5]]) => (drawable/->Stack [])
              (get-in grid [:grid [5 6]]) => (drawable/->Nothing)))
