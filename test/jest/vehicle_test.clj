(ns jest.vehicle-test
  (:use midje.sweet
        jest.testutils
        jest.color)
  (:require [jest.world.building :as b]
            [jest.world.cell :as c]
            [jest.vehicle :as v]))

(defmacro spawn-fact [doc & body]
  `(world-fact [10 10]
               ~doc
               (build-spawn-circle)
               ~@body))

(spawn-fact
 "Spawn returns the vehicle just spawned. This vehicle is also inside the cell."
 (let [vehicle (v/spawn (c/cell [5 5]))]
   (:vehicles (c/cell [5 5])) => (just [vehicle])))

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
   (:cargo (v/vehicle id)) => nil
   (tick 1) ; moved into the supply
   (:cargo (v/vehicle id)) => (roughly (hue :red) 0.01)))

(spawn-fact
 "A vehicle that passes through a depot without having cargo does nothing."
 (b/build-depot (c/cell [6 6]) (hue :red))
 (tick 20)
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (:cargo (v/vehicle id))) => nil)


(spawn-fact
 "A vehicle that passes through a depot after passing through a supply with the same resource drops its resource."
 (b/build-supply (c/cell [6 6]) (hue :green))
 (b/build-depot (c/cell [4 6]) (hue :green))
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 39) ; should have cargo
   (:cargo (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should drop cargo
   (:cargo (v/vehicle id)) => nil))

(spawn-fact
 "A vehicle that passes through a depot after passing through a supply with another resource drops its resource."
 (b/build-supply (c/cell [6 6]) (hue :green))
 (b/build-depot (c/cell [4 6]) (hue :red))
 (let [id (:id (v/spawn (c/cell [5 5])))]
   (tick 39) ; should have cargo
   (:cargo (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should not drop cargo
   (:cargo (v/vehicle id)) =not=> nil))
