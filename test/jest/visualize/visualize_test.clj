(ns jest.visualize.visualize-test
  (:use jest.testutils
        midje.sweet)
  (:require [jest.visualize.visualize :as visualize]
            [jest.world :as world]
            [brick.drawable :as drawable]
            [jest.world.building :as building]))

(world-fact [3 3]
  "Test the borders of a cell."
  (with-redefs [quil.core/width (fn []
                                  300)
                quil.core/height (fn []
                                   200)]
    (visualize/cell-borders (world/cell [1 1])) => {:north 67
                                              :west 100
                                              :south 134
                                              :east 200}))

(world-fact [10 10]
  "Layer from world state builds a valid grid drawable"
  (build-spawn-circle)
  (let [dnil (fn [c] (drawable/->Nothing))
        grid (visualize/world-state->Grid (fn [c]
                                            (if (building/spawn? c)
                                              (drawable/->Stack [])
                                              (drawable/->Nothing))))]
    (get-in grid [:grid [5 5]]) => (drawable/->Stack [])
    (get-in grid [:grid [5 6]]) => (drawable/->Nothing)))
