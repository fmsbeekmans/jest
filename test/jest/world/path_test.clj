(ns jest.world.path-test
  (:use midje.sweet
        jest.testutils
        [jest.world.cell :only [with-initialized-temp-world cell all-cells]])
  (:require [jest.world.path :as path]))


(with-initialized-temp-world [10 10]
  (fact "No paths should exist after world initialization."
        (doall (map (fn [c]
                      (and (empty? (path/in-paths c))
                           (empty? (path/out-paths c))))
                    (all-cells))) => (n-of true 100))

  (fact "After making a path, it exists in the right direction, in two adjacent cells."
        (path/build-path (cell [2 3]) :east :road)
        (path/out-paths (cell [2 3])) => (contains (fn [path]
                                                     (= (:direction path) :east)))
        (path/in-paths (cell [3 3])) => (contains (fn [path]
                                                     (= (:direction path) :west))))

  (fact "After unbuilding a path, it no longer exists."
        (path/unbuild-path (cell [2 3]) :east)
        (path/out-paths (cell [2 3])) =not=> (contains (fn [path]
                                                         (= (:direction path) :east)))
        (path/in-paths (cell [3 3])) =not=> (contains (fn [path]
                                                        (= (:direction path) :west))))

  (fact "Paths can be unbuilt in both directions."
        (path/build-path (cell [2 3]) :east :road)
        (path/unbuild-path (cell [3 3]) :west)
        (path/out-paths (cell [2 3])) =not=> (contains (fn [path]
                                                         (= (:direction path) :east)))
        (path/in-paths (cell [3 3])) =not=> (contains (fn [path]
                                                        (= (:direction path) :west)))))