(ns jest.world.building_test
  (:use midje.sweet
        jest.testutils
        [jest.world :only [cell all-cells coords]]
        [jest.world.cell :only [with-initialized-temp-world]]
        [clojure.test :only [deftest]])
  (:require [jest.world.building :as building]))

(world-fact [10 10]
            "After initialization, there are no buildings."
            (map building/building-type (all-cells)) => (n-of nil 100))

(world-fact [10 10]
            "Spawns can be built"
            (building/build-spawn (cell [3 3]) :truck)
            (building/building-type (cell [3 3])) => :spawn
            (building/vehicle-type (cell [3 3])) => :truck)

(world-fact [10 10]
            "Supplies can be built"
            (building/build-supply (cell [3 3]) :red)
            (building/building-type (cell [3 3])) => :supply
            (building/resource-type (cell [3 3])) => :red)

(world-fact [10 10]
            "Mixers can be built"
            (building/build-mixer (cell [3 3]))
            (building/building-type (cell [3 3])) => :mixer)

(world-fact [10 10]
            "Depots can be built"
            (building/build-depot (cell [3 3]) :blue 1000)
            (building/building-type (cell [3 3])) => :depot
            (building/resource-type (cell [3 3])) => :blue)

(world-fact [10 10]
            "Spawns can be destroyed"
            (building/build-spawn (cell [3 3]) :truck)
            (building/unbuild-spawn (cell [3 3]))
            (building/building-type (cell [3 3])) => nil)

(world-fact [10 10]
            "Supplies can be destroyed"
            (building/build-supply (cell [3 3]) :green)
            (building/unbuild-supply (cell [3 3]))
            (building/building-type (cell [3 3])) => nil)

(world-fact [10 10]
            "Mixers can be destroyed"
            (building/build-mixer (cell [3 3]))
            (building/unbuild-mixer (cell [3 3]))
            (building/building-type (cell [3 3])) => nil)

(world-fact [10 10]
            "Depots can be destroyed"
            (building/build-depot (cell [3 3]) :magenta 1000)
            (building/unbuild-depot (cell [3 3]))
            (building/building-type (cell [3 3])) => nil)

(with-initialized-temp-world [10 10]
  (let [spawns (set [[1 2] [3 4] [5 6]])
        supplies (set [[2 2] [4 4] [6 6]])
        mixers (set [[3 2] [5 4] [7 6]])
        depots (set [[4 2] [6 4] [8 6]])]
  (doseq [c (map cell spawns)]
    (building/build-spawn c :truck))
  (doseq [c (map cell supplies)]
    (building/build-supply c :red))
  (doseq [c (map cell mixers)]
    (building/build-mixer c))
  (doseq [c (map cell depots)]
    (building/build-depot c :blue 1000))

  (fact "Spawns can be found on a map."
    (set (map coords (building/all-spawns))) => spawns)
  (fact "Supplies can be found on a map."
    (set (map coords (building/all-supplies))) => supplies)
  (fact "Mixers can be found on a map."
    (set (map coords (building/all-mixers))) => mixers)
  (fact "Depots can be found on a map."
    (set (map coords (building/all-depots))) => depots)))
