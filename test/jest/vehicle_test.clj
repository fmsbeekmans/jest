(ns jest.vehicle-test
  (:use midje.sweet
        jest.testutils)
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
   ))
