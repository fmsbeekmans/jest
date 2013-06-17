(ns jest.movement-test
  (:use midje.sweet
        jest.testutils
        jest.color)
  (:require [jest.world :as w]
            [jest.world.building :as b]
            [jest.world.path :as p]
            [jest.world.cell :as c]
            [jest.world.route :as r]
            [jest.vehicle :as v]
            [jest.movement :as m]))

(defmacro spawn-fact [doc & body]
  `(world-fact [10 10]
               ~doc
               (build-spawn-circle)
               ~@body))

(spawn-fact
 "Spawn returns the vehicle just spawned. This vehicle is also inside the cell."
 (let [vehicle (m/spawn (w/cell [5 5]))]
   (:vehicles (w/cell [5 5])) => (just [vehicle])))

(world-fact [10 10]
  "all-vehicles returns all vehicles."
  (b/build-spawn (w/cell [0 0]) :truck)
  (b/build-spawn (w/cell [0 1]) :boat)
  (b/build-spawn (w/cell [0 2]) :train)
  (b/build-spawn (w/cell [1 0]) :truck)
  (b/build-spawn (w/cell [1 1]) :boat)
  (b/build-spawn (w/cell [1 2]) :train)

  (let [vehicles (doall (for [x (range 2)
                              y (range 3)
                              i (range 10)]
                          (m/spawn (w/cell [x y]))))]
    (v/all-vehicles) => (just vehicles :in-any-order)))

(world-fact [10 10]
  "all-vehicles with-predicate filters correctly"
  (b/build-spawn (w/cell [0 0]) :truck)
  (b/build-spawn (w/cell [0 1]) :boat)
  (b/build-spawn (w/cell [0 2]) :train)
  (b/build-spawn (w/cell [1 0]) :truck)
  (b/build-spawn (w/cell [1 1]) :boat)
  (b/build-spawn (w/cell [1 2]) :train)

  (let [vehicles (doall (for [x (range 2)
                              y (range 3)
                              i (range 10)]
                          (m/spawn (w/cell [x y]))))]
    
    (v/all-vehicles v/truck?) => (just (filter v/truck? vehicles) :in-any-order)
    (count (v/all-vehicles v/truck?)) => 20))

(spawn-fact
 "After spawning, entry time is the spawn time, exit time is spawn +
path duration."
 (tick 5)                               ; game time is now 5
 (let [vehicle (m/spawn (w/cell [5 5]))]
   (:entry-time vehicle) => 5
   (:exit-time vehicle) => 15)) ;5 + 10, because road

(spawn-fact
 "After spawning, a vehicle is in state :spawning"
 (:state (m/spawn (w/cell [5 5]))) => :spawning)

(spawn-fact
 "After spawning and half the path duration has passed,
a vehicle is in state :moving"
 (m/spawn (w/cell [5 5]))
 (tick 4) ;time is 4
 (:state (first (:vehicles (w/cell [5 5])))) => :spawning
 (tick 1) ;time is 5, half the road duration
 (:state (first (:vehicles (w/cell [5 5])))) => :moving)

(world-fact [10 10]
  "After spawning a boat and waiting 20 ticks, the vehicle has moved"
  (b/build-spawn (w/cell [5 5]) :boat)
  (p/build-path (w/cell [5 5]) :east :canal)
  (p/build-path (w/cell [6 5]) :east :canal)
  (let [id (:id (m/spawn (w/cell [5 5])))]
    (tick 19)
    (:coords (v/vehicle id)) => [5 5]
    (tick 1)
    (:coords (v/vehicle id)) => [6 5]))

(world-fact [10 10]
  "After spawning a boat and waiting 20 ticks, the vehicle has moved"
  (b/build-spawn (w/cell [5 5]) :train)
  (p/build-path (w/cell [5 5]) :east :rails)
  (p/build-path (w/cell [6 5]) :east :rails)
  (let [id (:id (m/spawn (w/cell [5 5])))]
    (tick 4)
    (:coords (v/vehicle id)) => [5 5]
    (tick 1)
    (:coords (v/vehicle id)) => [6 5]))

(spawn-fact
 "After spawning and the whole path duration has passed,
a vehicle has moved in its preferred direction. It then keeps moving."
 (let [v  (m/spawn (w/cell [5 5]))
       id (:id v)
       exit-direction (:exit-direction v)]
   (tick 9) ;time is 9, just before move
   (:coords (v/vehicle id)) => [5 5]
   (tick 1) ;time is 10, move should have happened
   (:coords (v/vehicle id)) => [6 5]
   (:vehicles (w/cell [5 5])) => empty?
   (:vehicles (w/cell [6 5])) => (just [(v/vehicle id)])
   (tick 10)
   (:coords (v/vehicle id)) => [6 6]))

(spawn-fact
 "A vehicle that enters a spawn point starts despawning."
 (let [id (:id (m/spawn (w/cell [5 5])))]
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
       id (:id (m/spawn (w/cell [5 5])))]
   (b/build-supply (w/cell [6 6]) (hue :red))
   (tick 19) ; just before entering cell 6 6, should have nothing
   (v/cargo? (v/vehicle id)) => false
   (tick 1) ; moved into the supply
   (v/cargo-color (v/vehicle id)) => (roughly (hue :red) 0.01)))

(spawn-fact
 "A vehicle that passes through a depot without having cargo does nothing."
 (b/build-depot (w/cell [6 6]) (hue :red))
 (tick 20)
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (v/cargo? (v/vehicle id))) => false)


(spawn-fact
 "A vehicle that passes through a depot after passing
through a supply with the same resource drops its resource."
 (b/build-supply (w/cell [6 6]) (hue :green))
 (b/build-depot (w/cell [4 6]) (hue :green))
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick 39) ; should have cargo
   (v/cargo-color (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should drop cargo
   (v/cargo-color (v/vehicle id)) => nil))

(spawn-fact
 "A vehicle that passes through a depot after passing through
a supply with another resource drops its resource."
 (b/build-supply (w/cell [6 6]) (hue :green))
 (b/build-depot (w/cell [4 6]) (hue :red))
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick 39) ; should have cargo
   (v/cargo-color (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should not drop cargo
   (v/cargo? (v/vehicle id)) => true))

(spawn-fact
 "A vehicle that passes through a mixer while carrying cargo drops
off the cargo"
 (b/build-supply (w/cell [6 5]) (hue :green))
 (b/build-mixer (w/cell [6 6]))
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick 19)
   (v/cargo-color  (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1)
   (v/cargo-color (v/vehicle id)) => nil
   (:resource (w/cell [6 6])) => (just [(roughly (hue :green) 0.01) 1])
   (tick 10)
   (v/cargo-color (v/vehicle id)) => nil
   (:resource (w/cell [6 6])) => (just [(roughly (hue :green) 0.01) 1])
   ))

(world-fact [10 10]
 "After delivering two colors to a mixer, the mixer contains a mixture"
 (b/build-mixer (w/cell [7 5]))
 (b/build-spawn (w/cell [5 5]) :truck)
 (p/build-path (w/cell [5 5]) :east :road)
 (b/build-supply (w/cell [6 5]) (hue :red))
 (p/build-path (w/cell [6 5]) :east :road)
 (p/build-path (w/cell [7 5]) :east :road)
 (b/build-spawn (w/cell [7 3]) :truck)
 (p/build-path (w/cell [7 3]) :south :road)
 (b/build-supply (w/cell [7 4]) (hue :green))
 (p/build-path (w/cell [7 4]) :south :road)
 (p/build-path (w/cell [7 5]) :south :road)
 (m/spawn (w/cell [5 5]))
 (m/spawn (w/cell [7 3]))
 (tick 20)
 (:resource (w/cell [7 5])) => (just [(roughly (hue :yellow) 0.01) 2])
 )

(world-fact [10 10]
 "After delivering two colors to a mixer with different vehicles,
the magnitude at the mixer should be the sum of the cargo counts"
 (b/build-mixer (w/cell [7 5]))
 (b/build-spawn (w/cell [5 5]) :truck)
 (p/build-path (w/cell [5 5]) :east :road)
 (b/build-supply (w/cell [6 5]) (hue :red))
 (p/build-path (w/cell [6 5]) :east :road)
 (p/build-path (w/cell [7 5]) :east :road)
 (b/build-spawn (w/cell [7 3]) :truck)
 (p/build-path (w/cell [7 3]) :south :road)
 (b/build-supply (w/cell [7 4]) (hue :green))
 (p/build-path (w/cell [7 4]) :south :road)
 (p/build-path (w/cell [7 5]) :south :road)
 (m/spawn (w/cell [5 5]))
 (m/spawn (w/cell [7 3]))
 (tick 20)
 (:resource (w/cell [7 5]))
   => (just [(roughly (hue :yellow) 0.01) (* 2 (v/cargo-capacity :truck))]))

(spawn-fact
 "A vehicle that enters a spawn point with cargo incurs a penalty."
 (b/build-supply (w/cell [6 5]) (hue :red))
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick 59) ;vehicle should now be west of spawn, after having moved 5 cells
   (:coords (v/vehicle id)) => [4 5]
   (tick 1) ;vehicle is on spawn, should start despawning
   (:coords (v/vehicle id)) => [5 5]
   (:state (v/vehicle id)) => :despawning
   (tick 5) ;vehicle should be gone now
   (v/vehicle id) => nil))

(world-fact [2 2]
 "a vehicle that despawns should not be scheduled to move again."
 (b/build-spawn (w/cell [0 0]) :truck)
 (b/build-spawn (w/cell [1 0]) :truck)
 (p/build-path (w/cell [0 0]) :east :road)
 (m/spawn (w/cell [0 0]))
 (tick 100) =not=> (throws Exception))

(defn build-crossroad [west-east-type north-south-type]
  (let [west-east-vehicle (p/path->vehicle west-east-type)
        north-south-vehicle (p/path->vehicle north-south-type)]
    (b/build-mixer (w/cell [5 5]))
    (b/build-spawn (w/cell [3 5]) west-east-vehicle)
    (b/build-spawn (w/cell [6 5]) west-east-vehicle)
    (b/build-supply (w/cell [4 5]) (hue :red))
    (p/build-path (w/cell [3 5]) :east west-east-type)
    (p/build-path (w/cell [4 5]) :east west-east-type)
    (p/build-path (w/cell [5 5]) :east west-east-type)

    (b/build-spawn (w/cell [5 4]) north-south-vehicle)
    (b/build-spawn (w/cell [5 6]) north-south-vehicle)
    (p/build-path (w/cell [5 4]) :south north-south-type)
    (p/build-path (w/cell [5 5]) :south north-south-type)))

(world-fact [10 10]
  "An empty vehicle that enters a mixer that contains resources picks
up resources"
  (build-crossroad :canal :canal)

  (m/spawn (w/cell [3 5]))
  (tick (* 3 (p/path->duration :canal))) ;boat dropped off cargo
  (b/resource-count (w/cell [5 5])) => 10
  (let [id (:id (m/spawn (w/cell [5 4])))]
    (tick 19)
    (v/cargo-count (v/vehicle id)) => 0
    (b/resource-count (w/cell [5 5])) => 10
    (tick 1)
    (v/cargo-count (v/vehicle id)) => 10
    (b/resource-count (w/cell [5 5])) => 0))

(world-fact [10 10]
  "When a vehicle can't pick up all the available content at a resource,
it only picks up as much as it can carry"
  (build-crossroad :canal :rails)
  (m/spawn (w/cell [3 5]))
  (tick (* 3 (p/path->duration :canal))) ; cargo dropped off

  (let [id (:id (m/spawn (w/cell [5 4])))]
    (tick (p/path->duration :rails))
    (v/cargo-count (v/vehicle id)) => (v/cargo-capacity :train)
    (b/resource-count (w/cell [5 5])) => (- (v/cargo-capacity :boat)
                                            (v/cargo-capacity :train))))

(world-fact [10 10]
  "When a vehicle can't pick up all the available content at a resource,
it only picks up as much as it can carry"
            (build-crossroad :rails :canal)
            (m/spawn (w/cell [3 5]))
            (tick (* 3 (p/path->duration :rails))) ;cargo dropped off

            (let [id (:id (m/spawn (w/cell [5 4])))]
              (tick (p/path->duration :canal))
              (v/cargo-count (v/vehicle id)) => (v/cargo-capacity :train)
              (b/resource-count (w/cell [5 5])) => 0))

(fact "vehicle transition dispatch works"
  (m/vehicle-transition-state-dispatch ..vehicle-id..)
    => [..cargo-type.. ..building-type..]
  (provided (v/vehicle ..vehicle-id..)
    => ..vehicle..
  (v/cargo? ..vehicle..)
    => ..cargo-type..
  (v/vehicle-cell ..vehicle..)
    => {:building-type ..building-type..}))

(defmacro with-spawned-vehicle [[vehicle spawn-location] & body]
  `(let [~vehicle (:id (m/spawn (w/cell ~spawn-location)))]
     ~@body))

(defn tick-move [vehicle-id]
  (tick (v/vehicle->duration (v/vehicle vehicle-id))))

(world-fact [10 10]
  "when multiple out-paths are encountered without routes,
pick the first one in clockwise from :north"
  (b/build-spawn (w/cell [5 5]) :truck)
  (p/build-path (w/cell [5 5]) :south :road)
  (p/build-path (w/cell [5 6]) :south :road)
  (p/build-path (w/cell [5 6]) :west :road)
  (p/build-path (w/cell [5 6]) :east :road)

  (with-spawned-vehicle [truck [5 5]]
    (tick-move truck)
    (tick-move truck) ;should have picked east
    (:coords (v/vehicle truck)) => [6 6]))


(world-fact [10 10]
  "when multiple out-paths are encountered where one has a matching route,
pick the one with the route"
  (b/build-spawn (w/cell [5 4]) :truck)
  (b/build-supply (w/cell [5 5]) (hue :red))
  (p/build-path (w/cell [5 4]) :south :road)
  (p/build-path (w/cell [5 5]) :south :road)
  (p/build-path (w/cell [5 6]) :south :road)
  (p/build-path (w/cell [5 6]) :west :road)
  (p/build-path (w/cell [5 6]) :east :road)

  (r/build-route (w/cell [5 6]) :west (hue :red))
  (r/build-route (w/cell [5 6]) :south (hue :green))
  (r/build-route (w/cell [5 6]) :east (hue :blue))

  (with-spawned-vehicle [truck [5 4]]
    (tick-move truck)
    (v/cargo-count (v/vehicle truck)) => 1
    (tick-move truck)
    (tick-move truck) ;should have picked west
    (:coords (v/vehicle truck)) => [4 6]))

(world-fact [10 10]
  "when multiple out-paths are encountered with routes,
while not carrying any cargo, the first one in clockwise order"
  (b/build-spawn (w/cell [5 4]) :truck)
  (p/build-path (w/cell [5 4]) :south :road)
  (p/build-path (w/cell [5 5]) :south :road)
  (p/build-path (w/cell [5 6]) :south :road)
  (p/build-path (w/cell [5 6]) :west :road)
  (p/build-path (w/cell [5 6]) :east :road)

  (r/build-route (w/cell [5 6]) :west (hue :red))
  (r/build-route (w/cell [5 6]) :south (hue :green))
  (r/build-route (w/cell [5 6]) :east (hue :blue))

  (with-spawned-vehicle [truck [5 4]]
    (tick-move truck)
    (tick-move truck)
    (tick-move truck) ;should have picked east
    (:coords (v/vehicle truck)) => [6 6]))

(world-fact [10 10]
  "when multiple out-paths are encountered with routes,
while carrying non-matching cargo, the first one in clockwise order is selected"
  (b/build-spawn (w/cell [5 4]) :truck)
  (b/build-supply (w/cell [5 5]) (hue :yellow))
  (p/build-path (w/cell [5 4]) :south :road)
  (p/build-path (w/cell [5 5]) :south :road)
  (p/build-path (w/cell [5 6]) :south :road)
  (p/build-path (w/cell [5 6]) :west :road)
  (p/build-path (w/cell [5 6]) :east :road)

  (r/build-route (w/cell [5 6]) :west (hue :red))
  (r/build-route (w/cell [5 6]) :south (hue :green))
  (r/build-route (w/cell [5 6]) :east (hue :blue))

  (with-spawned-vehicle [truck [5 4]]
    (tick-move truck)
    (tick-move truck)
    (tick-move truck) ;should have picked east
    (:coords (v/vehicle truck)) => [6 6]))

(world-fact [10 10]
  "when multiple matching out-paths are encountered with routes,
while carrying non-matching cargo, the first matching one in
clockwise order is selected"
  (b/build-spawn (w/cell [5 4]) :truck)
  (b/build-supply (w/cell [5 5]) (hue :red))
  (p/build-path (w/cell [5 4]) :south :road)
  (p/build-path (w/cell [5 5]) :south :road)
  (p/build-path (w/cell [5 6]) :south :road)
  (p/build-path (w/cell [5 6]) :west :road)
  (p/build-path (w/cell [5 6]) :east :road)

  (r/build-route (w/cell [5 6]) :east (hue :red))
  (r/build-route (w/cell [5 6]) :south (hue :red))

  (with-spawned-vehicle [truck [5 4]]
    (tick-move truck)
    (tick-move truck)
    (tick-move truck) ;should have picked east
    (:coords (v/vehicle truck)) => [6 6]))
