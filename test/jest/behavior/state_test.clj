(ns jest.behavior.state-test
  (:require [jest.behavior.state :as s]
            [jest.world.vehicle :as v :refer [vehicle]]
            [jest.world.path :as p]
            [jest.world.building :as b]
            [jest.world :as w :refer [cell]]
            [jest.scheduler :as sc]

            [jest.testutils :refer :all]
            [midje.sweet :refer :all]))

(fact "Vehicles are incoming when the game-time is lower than :entry-time plus half the path duration."
      (with-game-time 10
        (let [vehicle (v/map->Vehicle {:type :truck
                                       :entry-time 0})]
          (s/vehicle-state-in-cell vehicle) => :incoming
          (s/incoming? vehicle) => TRUTHY
          (s/outgoing? vehicle) => FALSEY)))

(fact "Vehicles are outgoing when the game-time is equal to or higher than :entry-time plus half the path duration."
      (with-game-time (/ (p/path->duration :road) 2)
        (let [vehicle (v/map->Vehicle {:type :truck
                                       :entry-time 0})]
          (s/vehicle-state-in-cell vehicle) => :outgoing
          (s/incoming? vehicle) => FALSEY
          (s/outgoing? vehicle) => TRUTHY)))

(world-fact [1 1]
            "A vehicle that is set to despawn changes state and clears exit."
            (b/build-spawn (cell [0 0]) :truck)
            (dosync
             (v/load-vehicle (cell [0 0]) (v/map->Vehicle {:id 0
                                                           :type :truck
                                                           :exit-time 10
                                                           :exit-direction :east
                                                           :state :moving})))
            (s/start-despawning 0)
            (v/despawning? (vehicle 0)) => TRUTHY
            (:exit-direction (vehicle 0)) => nil)

(world-fact [1 1]
            "A vehicle that is set to explode changes state and clears exit."
            (dosync
             (v/load-vehicle (cell [0 0]) (v/map->Vehicle {:id 0
                                                           :type :truck
                                                           :exit-time 10
                                                           :exit-direction :east
                                                           :state :moving})))
            (s/start-exploding 0)
            (v/exploding? (vehicle 0)) => TRUTHY
            (:exit-direction (vehicle 0)) => nil)

(world-fact [1 1]
            "Calling update-vehicle-exit will cause a vehicle to explode if there's no exit path."
            (dosync
             (v/load-vehicle (cell [0 0]) (v/map->Vehicle {:id 0
                                                           :type :truck
                                                           :exit-time 10
                                                           :exit-direction :east
                                                           :state :moving}))
             (s/update-vehicle-exit 0))
            (v/exploding? (vehicle 0)) => TRUTHY
            (:exit-direction (vehicle 0)) => nil)

(world-fact [2 2]
            "Calling update-vehicle-exit will change a vehicle's exit path if required."
            (dosync
             (v/load-vehicle (cell [0 0]) (v/map->Vehicle {:id 0
                                                           :type :truck
                                                           :exit-time 10
                                                           :exit-direction :east
                                                           :state :moving})))
            (p/build-path (cell [0 0]) :south :road)
            (dosync
             (s/update-vehicle-exit 0))
            (:exit-direction (vehicle 0)) => :south)

(world-fact [2 2]
            "Calling update-vehicles-for-cell-changes will update all vehicles in a cell to new conditions"
            (with-game-time 1500
              (p/build-path (cell [0 0]) :south :road)
              (letfn [(load [id entry-time]
                        (v/load-vehicle (cell [0 0])
                                        (v/map->Vehicle {:id id
                                                         :type :truck
                                                         :entry-time entry-time
                                                         :exit-direction :east
                                                         :exit-time (+ entry-time (p/path->duration :road))
                                                         :state :moving})))]
                (dosync
                 (load 0 1500)
                 (load 1 1000)
                 (s/update-vehicles-for-cell-changes (cell [0 0]))
                 (:exit-direction (vehicle 0)) => :south
                 (v/moving? (vehicle 0)) => TRUTHY
                 (:exit-direction (vehicle 1)) => nil
                 (v/exploding? (vehicle 1)) => TRUTHY))))
