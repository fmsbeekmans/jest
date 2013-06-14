(ns jest.world.path-test
  (:use midje.sweet
        jest.testutils
        [jest.world.cell :only [with-initialized-temp-world]]
        [jest.world :only [cell all-cells]])
  (:require [jest.world.path :as path]))


(world-fact [10 10]
  "No paths should exist after world initialization."
  (doall (map (fn [c]
                (and (empty? (path/in-paths c))
                     (empty? (path/out-paths c))))
              (all-cells))) => (n-of true 100))

(world-fact [10 10]
  "After making a path, it exists in the right direction,
in two adjacent cells."
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
  "A built path is an out-path in the cell it was built in,
and an in-path in the adjacent cell."
  (path/build-path (cell [2 3]) :east :road)
  (path/out-path? (path/path (cell [2 3]) :east)) => TRUTHY
  (path/in-path? (path/path (cell [3 3]) :west)) => TRUTHY)

(world-fact [10 10]
  "An incoming path combined with an outgoing path of the same
type is a complete path."
  (count (path/complete-paths (cell [3 2]))) => 0
  (path/build-path (cell [2 2]) :east :road)
  (count (path/complete-paths (cell [3 2]))) => 0
  (path/build-path (cell [3 2]) :east :road)
  (count (path/complete-paths (cell [3 2]))) => 1)

(world-fact [10 10]
  "An incoming path combined with an outgoing path of another
type is not a complete path."
  (path/build-path (cell [2 2]) :east :road)
  (path/build-path (cell [3 2]) :east :rails)
  (count (path/complete-paths (cell [3 2]))) => 0)

(world-fact [10 10]
  "An incoming path combined with multiple outgoing patsh of the
same type are multiple complete paths."
  (path/build-path (cell [2 2]) :east :road)
  (path/build-path (cell [3 2]) :east :road)
  (path/build-path (cell [3 2]) :north :road)
  (count (path/complete-paths (cell [3 2]))) => 2
  (path/build-path (cell [3 2]) :south :road)
  (count (path/complete-paths (cell [3 2]))) => 3)

(world-fact [10 10]
  "paths returns all defined paths for a cell"
  (empty? (path/paths (cell [2 2]))) => TRUTHY
  (path/build-path (cell [2 2]) :east :road)
  (path/paths (cell [2 2])) => (contains (path/path (cell [2 2]) :east)
                                         :in-any-order)
  (path/build-path (cell [2 2]) :west :rails)
  (path/paths (cell [2 2])) => (contains (path/path (cell [2 2]) :east)
                                         (path/path (cell [2 2]) :west)
                                         :in-any-order)
  (path/build-path (cell [2 2]) :north :canal)
  (path/paths (cell [2 2])) => (contains (path/path (cell [2 2]) :east)
                                         (path/path (cell [2 2]) :west)
                                         (path/path (cell [2 2]) :north)
                                         :in-any-order)
  (path/build-path (cell [2 2]) :south :road)
  (path/paths (cell [2 2])) => (contains (path/path (cell [2 2]) :east)
                                         (path/path (cell [2 2]) :west)
                                         (path/path (cell [2 2]) :north)
                                         (path/path (cell [2 2]) :south)
                                         :in-any-order))
