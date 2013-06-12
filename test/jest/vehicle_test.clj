(ns jest.vehicle-test
  (:use midje.sweet
        jest.testutils
        jest.color)
  (:require [jest.world.building :as b]
            [jest.world.path :as p]
            [jest.world.cell :as c]
            [jest.vehicle :as v]
            [jest.score :as s]))

(defmacro spawn-fact [doc & body]
  `(world-fact [10 10]
               ~doc
               (build-spawn-circle)
               ~@body))

(spawn-fact
 "Spawn returns the vehicle just spawned. This vehicle is also inside the cell."
 (let [vehicle (v/spawn (c/cell [5 5]))]
   (:vehicles (c/cell [5 5])) => (just [vehicle])))

(world-fact [10 10]
            "all-vehicles returns all vehicles."
            (b/build-spawn (c/cell [0 0]) :truck)
            (b/build-spawn (c/cell [0 1]) :boat)
            (b/build-spawn (c/cell [0 2]) :train)
            (b/build-spawn (c/cell [1 0]) :truck)
            (b/build-spawn (c/cell [1 1]) :boat)
            (b/build-spawn (c/cell [1 2]) :train)

            (let [vehicles (doall (for [x (range 2)
                                        y (range 3)
                                        i (range 10)]
                                    (v/spawn (c/cell [x y]))))]
              (v/all-vehicles) => (just vehicles :in-any-order))
            )

(spawn-fact
 "After spawning, entry time is the spawn time, exit time is spawn + path duration."
 (tick 5) ;game time is now 5
 (let [vehicle (v/spawn (c/cell [5 5]))]
   (:entry-time vehicle) => 5
   (:exit-time vehicle) => 15 ;5 + 10, because road
   ))

(spawn-fact
 "After spawning, a vehicle is in state :spawning"
 (:state (v/spawn (c/cell [5 5]))) => :spawning)

(spawn-fact
 "After spawning and half the path duration has passed, a vehicle is in state :moving"
 (v/spawn (c/cell [5 5]))
 (tick 4) ;time is 4
 (:state (first (:vehicles (c/cell [5 5])))) => :spawning
 (tick 1) ;time is 5, half the road duration
 (:state (first (:vehicles (c/cell [5 5])))) => :moving)

(world-fact [10 10]
            "After spawning a boat and waiting 20 ticks, the vehicle has moved"
            (b/build-spawn (c/cell [5 5]) :boat)
            (p/build-path (c/cell [5 5]) :east :canal)
            (p/build-path (c/cell [6 5]) :east :canal)
            (let [id (:id (v/spawn (c/cell [5 5])))]
              (tick 19)
              (:coords (v/vehicle id)) => [5 5]
              (tick 1)
              (:coords (v/vehicle id)) => [6 5]))

(world-fact [10 10]
            "After spawning a boat and waiting 20 ticks, the vehicle has moved"
            (b/build-spawn (c/cell [5 5]) :train)
            (p/build-path (c/cell [5 5]) :east :rails)
            (p/build-path (c/cell [6 5]) :east :rails)
            (let [id (:id (v/spawn (c/cell [5 5])))]
              (tick 4)
              (:coords (v/vehicle id)) => [5 5]
              (tick 1)
              (:coords (v/vehicle id)) => [6 5]))

(spawn-fact
 "After spawning and the whole path duration has passed, a vehicle has moved in its preferred direction. It then keeps moving."
 (let [v  (v/spawn (c/cell [5 5]))
       id (:id v)
       exit-direction (:exit-direction v)]
   (tick 9) ;time is 9, just before move
   (:coords (v/vehicle id)) => [5 5]
   (tick 1) ;time is 10, move should have happened
   (:coords (v/vehicle id)) => [6 5]
   (:vehicles (c/cell [5 5])) => empty?
   (:vehicles (c/cell [6 5])) => (just [(v/vehicle id)])
   (tick 10)
   (:coords (v/vehicle id)) => [6 6]))

(spawn-fact
 "A vehicle that enters a spawn point starts despawning."
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 59) ;vehicle should now be west of spawn, after having moved 5 cells
   (:coords (v/vehicle id)) => [4 5]
   (tick 1) ;vehicle is on spawn, should start despawning
   (:coords (v/vehicle id)) => [5 5]
   (:state (v/vehicle id)) => :despawning
   (tick 5) ;vehicle should be gone now
   (v/vehicle id) => nil
   ))

(spawn-fact
 "A vehicle that passes through a supply picks up a resource."
 (let [resource (hue :red)
       id (:id (v/spawn (c/cell [5 5])))]
   (b/build-supply (c/cell [6 6]) (hue :red))
   (tick 19) ; just before entering cell 6 6, should have nothing
   (v/cargo? (v/vehicle id)) => false
   (tick 1) ; moved into the supply
   (v/cargo-color (v/vehicle id)) => (roughly (hue :red) 0.01)))

(spawn-fact
 "A vehicle that passes through a depot without having cargo does nothing."
 (b/build-depot (c/cell [6 6]) (hue :red))
 (tick 20)
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (v/cargo? (v/vehicle id))) => false)


(spawn-fact
 "A vehicle that passes through a depot after passing through a supply with the same resource drops its resource."
 (b/build-supply (c/cell [6 6]) (hue :green))
 (b/build-depot (c/cell [4 6]) (hue :green))
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 39) ; should have cargo
   (v/cargo-color (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should drop cargo
   (v/cargo-color (v/vehicle id)) => nil))

(spawn-fact
 "A vehicle that passes through a depot after passing through a supply with another resource drops its resource."
 (b/build-supply (c/cell [6 6]) (hue :green))
 (b/build-depot (c/cell [4 6]) (hue :red))
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 39) ; should have cargo
   (v/cargo-color (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should not drop cargo
   (v/cargo? (v/vehicle id)) => true))

(spawn-fact
 "A vehicle that passes through a mixer while carrying cargo drops off the cargo"
 (b/build-supply (c/cell [6 5]) (hue :green))
 (b/build-mixer (c/cell [6 6]))
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 19)
   (v/cargo-color  (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1)
   (v/cargo-color (v/vehicle id)) => nil
   (:resource (c/cell [6 6])) => (just [(roughly (hue :green) 0.01) 1])
   (tick 10)
   (v/cargo-color (v/vehicle id)) => nil
   (:resource (c/cell [6 6])) => (just [(roughly (hue :green) 0.01) 1])
   ))

(world-fact [10 10]
 "After delivering two colors to a mixer, the mixer contains a mixture"
 (b/build-mixer (c/cell [7 5]))
 (b/build-spawn (c/cell [5 5]) :truck)
 (p/build-path (c/cell [5 5]) :east :road)
 (b/build-supply (c/cell [6 5]) (hue :red))
 (p/build-path (c/cell [6 5]) :east :road)
 (p/build-path (c/cell [7 5]) :east :road)
 (b/build-spawn (c/cell [7 3]) :truck)
 (p/build-path (c/cell [7 3]) :south :road)
 (b/build-supply (c/cell [7 4]) (hue :green))
 (p/build-path (c/cell [7 4]) :south :road)
 (p/build-path (c/cell [7 5]) :south :road)
 (v/spawn (c/cell [5 5]))
 (v/spawn (c/cell [7 3]))
 (tick 20)
 (:resource (c/cell [7 5])) => (just [(roughly (hue :yellow) 0.01) 2])
 )

(world-fact [10 10]
 "After delivering two colors to a mixer with different vehicles, the magnitude at the mixer should be the sum of the cargo counts"
 (b/build-mixer (c/cell [7 5]))
 (b/build-spawn (c/cell [5 5]) :truck)
 (p/build-path (c/cell [5 5]) :east :road)
 (b/build-supply (c/cell [6 5]) (hue :red))
 (p/build-path (c/cell [6 5]) :east :road)
 (p/build-path (c/cell [7 5]) :east :road)
 (b/build-spawn (c/cell [7 3]) :truck)
 (p/build-path (c/cell [7 3]) :south :road)
 (b/build-supply (c/cell [7 4]) (hue :green))
 (p/build-path (c/cell [7 4]) :south :road)
 (p/build-path (c/cell [7 5]) :south :road)
 (v/spawn (c/cell [5 5]))
 (v/spawn (c/cell [7 3]))
 (tick 20)
 (:resource (c/cell [7 5])) => (just [(roughly (hue :yellow) 0.01) (* 2 (v/cargo-capacity :truck))])
 )

(spawn-fact
 "A vehicle that enters a spawn point with cargo incurs a penalty."
 (b/build-supply (c/cell [6 5]) (hue :red))
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 59) ;vehicle should now be west of spawn, after having moved 5 cells
   (:coords (v/vehicle id)) => [4 5]
   (tick 1) ;vehicle is on spawn, should start despawning
   (:coords (v/vehicle id)) => [5 5]
   (:state (v/vehicle id)) => :despawning
   (tick 5) ;vehicle should be gone now
   (v/vehicle id) => nil

   ))

(world-fact [2 2]
 "a vehicle that despawns should not be scheduled to move again."
 (b/build-spawn (c/cell [0 0]) :truck)
 (b/build-spawn (c/cell [1 0]) :truck)
 (p/build-path (c/cell [0 0]) :east :road)
 (v/spawn (c/cell [0 0]))
 (tick 100) =not=> (throws Exception)
 )

(defn build-crossroad [west-east-type north-south-type]
  (let [west-east-vehicle (p/path->vehicle west-east-type)
        north-south-vehicle (p/path->vehicle north-south-type)]
    (b/build-mixer (c/cell [5 5]))
    (b/build-spawn (c/cell [3 5]) west-east-vehicle)
    (b/build-spawn (c/cell [6 5]) west-east-vehicle)
    (b/build-supply (c/cell [4 5]) (hue :red))
    (p/build-path (c/cell [3 5]) :east west-east-type)
    (p/build-path (c/cell [4 5]) :east west-east-type)
    (p/build-path (c/cell [5 5]) :east west-east-type)
    
    (b/build-spawn (c/cell [5 4]) north-south-vehicle)
    (b/build-spawn (c/cell [5 6]) north-south-vehicle)
    (p/build-path (c/cell [5 4]) :south north-south-type)
    (p/build-path (c/cell [5 5]) :south north-south-type)))

(world-fact [10 10]
            "An empty vehicle that enters a mixer that contains resources picks up resources"
            (build-crossroad :canal :canal)

            (v/spawn (c/cell [3 5]))
            (tick (* 3 (p/path->duration :canal))) ;boat dropped off cargo
            (v/resource-count (c/cell [5 5])) => 10
            (let [id (:id (v/spawn (c/cell [5 4])))]
              (tick 19)
              (v/cargo-count (v/vehicle id)) => 0
              (v/resource-count (c/cell [5 5])) => 10
              (tick 1)
              (v/cargo-count (v/vehicle id)) => 10
              (v/resource-count (c/cell [5 5])) => 0))

(world-fact [10 10]
            "When a vehicle can't pick up all the available content at a resource, it only picks up as much as it can carry"
            (build-crossroad :canal :rails)
            (v/spawn (c/cell [3 5]))
            (tick (* 3 (p/path->duration :canal))) ;cargo dropped off

            (let [id (:id (v/spawn (c/cell [5 4])))]
              (tick (p/path->duration :rails))
              (v/cargo-count (v/vehicle id)) => (v/cargo-capacity :train)
              (v/resource-count (c/cell [5 5])) => (- (v/cargo-capacity :boat)
                                                      (v/cargo-capacity :train))))

(world-fact [10 10]
            "When a vehicle can't pick up all the available content at a resource, it only picks up as much as it can carry"
            (build-crossroad :rails :canal)
            (v/spawn (c/cell [3 5]))
            (tick (* 3 (p/path->duration :rails))) ;cargo dropped off

            (let [id (:id (v/spawn (c/cell [5 4])))]
              (tick (p/path->duration :canal))
              (v/cargo-count (v/vehicle id)) => (v/cargo-capacity :train)
              (v/resource-count (c/cell [5 5])) => 0))

(fact "vehicle transition dispatch works"
      (v/vehicle-transition-state-dispatch ..vehicle-id..) => [..cargo-type.. ..building-type..]
      (provided (v/vehicle ..vehicle-id..) => ..vehicle..
                (v/cargo? ..vehicle..) => ..cargo-type..
                (v/vehicle-cell ..vehicle..) => {:building-type ..building-type..}))
