(ns jest.behavior.spawn_test
  (:require [jest.behavior.spawn :as s]
            [jest.testutils :refer :all]
            [midje.sweet :refer :all]

            [jest.world.building :as b]
            [jest.world.path :as p]
            [jest.world.vehicle :as v :refer [vehicle]]
            [jest.world :as w :refer [cell]]))

(defmacro spawn-fact [doc & body]
  `(world-fact [10 10] ~doc
               (s/stop-spawning)
               (b/build-spawn (cell [5 5]) :truck)
               ~@body))

(spawn-fact "spawn spawns a vehicle on the given cell"
            (v/vehicles (cell [5 5])) => empty?
            (with-spawned-vehicle [v [5 5]]
              (v/vehicles (cell [5 5])) => (contains (vehicle v))))

(spawn-fact "A spawned vehicle has the cell it spawned on as its location"
            (with-spawned-vehicle [v [5 5]]
              (v/vehicle-cell (vehicle v)) => (cell [5 5])))

(spawn-fact "When spawned, a vehicle has the current game-time as its entry time, and no entry direction."
            (tick 42) ;;time is now 42
            (with-spawned-vehicle [v [5 5]]
              (:entry-time (vehicle v)) => 42
              (:entry-direction (vehicle v)) => nil))

(spawn-fact "two spawned vehicles have different ids"
            (dotimes [i 2] (s/spawn (cell [5 5])))
            (apply not= (map :id (v/vehicles (cell [5 5])))) => TRUTHY)

(spawn-fact "A vehicle spawned on a cell with no out paths is both spawning and exploding, and has no exit direction set."
            (with-spawned-vehicle [v [5 5]]
              (v/spawning? (vehicle v)) => TRUTHY
              (v/exploding? (vehicle v)) => TRUTHY
              (:exit-direction (vehicle v)) => nil))

(spawn-fact "A vehicle spawned on a cell with an out path is spawning but not exploding and has an exit direction set."
            (p/build-path (cell [5 5]) :east :road)
            (with-spawned-vehicle [v [5 5]]
              (v/spawning? (vehicle v)) => TRUTHY
              (v/exploding? (vehicle v)) => FALSEY
              (:exit-direction (vehicle v)) =not=> nil))

(spawn-fact "A spawn point that is not enabled will not be marked as active when starting the spawning process"
            (s/start-spawning)
            (s/active? (cell [5 5])) => FALSEY)

(spawn-fact "A spawn point that is enabled will be marked as active when starting the spawning process"
            (b/enable-spawner (cell [5 5]) 0 10)
            (s/start-spawning)
            (s/active? (cell [5 5])) => TRUTHY)

(spawn-fact "After stopping the spawning process, spawners are marked as inactive."
            (b/enable-spawner (cell [5 5]) 0 10)
            (s/start-spawning)
            (s/stop-spawning)
            (s/active? (cell [5 5])) => FALSEY)

(spawn-fact "An active spawner spawns vehicles at the given rate"
            (b/enable-spawner (cell [5 5]) 0 10)
            (s/start-spawning)
            (count (v/vehicles (cell [5 5]))) => 1
            (tick 9)
            (count (v/vehicles (cell [5 5]))) => 1
            (tick 1)
            (count (v/vehicles (cell [5 5]))) => 2)

(spawn-fact "an active spawner spawns vehicles with the given offset"
            (b/enable-spawner (cell [5 5]) 5 10)
            (s/start-spawning)
            (count (v/vehicles (cell [5 5]))) => 0
            (tick 4)
            (count (v/vehicles (cell [5 5]))) => 0
            (tick 1)
            (count (v/vehicles (cell [5 5]))) => 1
            (tick 10)
            (count (v/vehicles (cell [5 5]))) => 2)

(spawn-fact "After stopping the spawning process, no more vehicles are spawned."
            (b/enable-spawner (cell [5 5]) 0 10)
            (s/start-spawning)
            (tick 20)
            (count (v/vehicles (cell [5 5]))) => 3
            (s/stop-spawning)
            (tick 100)
            (count (v/vehicles (cell [5 5]))) => 3)
