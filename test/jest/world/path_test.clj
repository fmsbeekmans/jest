(ns jest.world.path-test
  (:use midje.sweet
        jest.testutils
        [jest.world.cell :only [with-initialized-temp-world cell all-cells]])
  (:require [jest.world.path :as path]))


(world-fact [10 10]
            "No paths should exist after world initialization."
            (doall (map (fn [c]
                          (and (empty? (path/in-paths c))
                               (empty? (path/out-paths c))))
                        (all-cells))) => (n-of true 100))

(world-fact [10 10]
            "After making a path, it exists in the right direction, in two adjacent cells."
            (path/build-path (cell [2 3]) :east :road)
            (path/path (cell [2 3]) :east) => TRUTHY
            (path/path (cell [3 3]) :west) => TRUTHY)

(world-fact [10 10]
            "After unbuilding a path, it no longer exists."
            (path/build-path (cell [2 3]) :east :road)
            (path/unbuild-path (cell [2 3]) :east)
            (path/path (cell [2 3]) :east) => FALSEY
            (path/path (cell [3 3]) :west) => FALSEY)

(world-fact [10 10]
            "Paths can be unbuilt from the adjacent cell as well."
            (path/build-path (cell [2 3]) :east :road)
            (path/unbuild-path (cell [3 3]) :west)
            (path/path (cell [2 3]) :east) => FALSEY
            (path/path (cell [3 3]) :west) => FALSEY)

(world-fact [10 10]
            "A built path is an out-path in the cell it was built in, and an in-path in the adjacent cell."
            (path/build-path (cell [2 3]) :east :road)
            (path/out-path? (path/path (cell [2 3]) :east)) => TRUTHY
            (path/in-path? (path/path (cell [3 3]) :west)) => TRUTHY)
