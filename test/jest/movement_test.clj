(ns jest.movement-test
  (:use midje.sweet
        jest.testutils
        jest.color)
  (:require [jest.world :as w :refer [cell]]
            [jest.world.building :as b :refer [build-spawn build-supply build-depot build-mixer]]
            [jest.world.path :as p :refer [build-path]]
            [jest.world.cell :as c]
            [jest.world.route :as r :refer [build-route]]
            [jest.vehicle :as v :refer [vehicle]]
            [jest.movement :as m]))

(defn tick-move [vehicle-id]
  (tick (v/vehicle->duration (v/vehicle vehicle-id))))

(defn half-move [vehicle-id]
  (tick (m/half-duration (v/vehicle vehicle-id))))


(defmacro spawn-fact [doc & body]
  `(world-fact [10 10]
               ~doc
               (build-spawn-circle)
               ~@body))

(defmacro with-spawned-vehicle [[vehicle spawn-location] & body]
  `(let [~vehicle (:id (m/spawn (w/cell ~spawn-location)))]
     ~@body))

(def +truck-speed+ (p/path->duration :road))
(def +train-speed+ (p/path->duration :rails))
(def +boat-speed+ (p/path->duration :canal))

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

(world-fact [10 10]
  "vehicle-type predicates work correctly."
  (b/build-spawn (w/cell [0 0]) :truck)
  (b/build-spawn (w/cell [0 1]) :boat)
  (b/build-spawn (w/cell [0 2]) :train)
  (b/build-spawn (w/cell [1 0]) :truck)
  (b/build-spawn (w/cell [1 1]) :boat)
  (b/build-spawn (w/cell [1 2]) :train)

  (let [vehicles (doall (for [x (range 2)
                              y (range 3)]
                          (m/spawn (w/cell [x y]))))]
    (doseq [v (v/vehicles (w/cell [0 0]))]
      (v/truck? v) => truthy
      (v/boat? v) => falsey
      (v/train? v) => falsey)
    (doseq [v (v/vehicles (w/cell [0 1]))]
      (v/truck? v) => falsey
      (v/boat? v) => truthy
      (v/train? v) => falsey)
    (doseq [v (v/vehicles (w/cell [0 2]))]
      (v/truck? v) => falsey
      (v/boat? v) => falsey
      (v/train? v) => truthy)))

(spawn-fact
 "After spawning, entry time is the spawn time, exit time is spawn +
path duration."
 (tick 5)                               ; game time is now 5
 (let [vehicle (m/spawn (w/cell [5 5]))]
   (:entry-time vehicle) => 5
   (:exit-time vehicle) => (+ +truck-speed+ 5)))

(spawn-fact
 "After spawning, a vehicle is in state :spawning"
 (:state (m/spawn (w/cell [5 5]))) => :spawning)

(spawn-fact
 "After spawning and the path duration has passed,
a vehicle is in state :moving"
 (with-spawned-vehicle [truck [5 5]]
   (m/spawn (w/cell [5 5]))
   (tick (dec +truck-speed+))
   (:state (v/vehicle truck)) => :spawning
   (tick 1)
   (:state (v/vehicle truck)) => :moving))

(world-fact [10 10]
  "After spawning a boat and waiting 20 ticks, the vehicle has moved"
  (b/build-spawn (w/cell [5 5]) :boat)
  (p/build-path (w/cell [5 5]) :east :canal)
  (p/build-path (w/cell [6 5]) :east :canal)
  (let [id (:id (m/spawn (w/cell [5 5])))]
    (tick (dec +boat-speed+))
    (:coords (v/vehicle id)) => [5 5]
    (tick 1)
    (:coords (v/vehicle id)) => [6 5]))

(world-fact [10 10]
  "After spawning a train and waiting the right amount of ticks, the vehicle
has moved"
  (b/build-spawn (w/cell [5 5]) :train)
  (p/build-path (w/cell [5 5]) :east :rails)
  (p/build-path (w/cell [6 5]) :east :rails)
  (let [id (:id (m/spawn (w/cell [5 5])))]
    (tick (dec +train-speed+))
    (:coords (v/vehicle id)) => [5 5]
    (tick 1)
    (:coords (v/vehicle id)) => [6 5]))

(spawn-fact
 "After spawning and the whole path duration has passed,
a vehicle has moved in its preferred direction. It then keeps moving."
 (let [v  (m/spawn (w/cell [5 5]))
       id (:id v)
       exit-direction (:exit-direction v)]
   (tick (dec +truck-speed+)) ;time is just before move
   (:coords (v/vehicle id)) => [5 5]
   (tick 1) ;time is 10, move should have happened
   (:coords (v/vehicle id)) => [6 5]
   (:vehicles (w/cell [5 5])) => empty?
   (:vehicles (w/cell [6 5])) => (just [(v/vehicle id)])
   (tick +truck-speed+)
   (:coords (v/vehicle id)) => [6 6]))

(spawn-fact
 "A vehicle that enters a spawn point starts despawning."
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick (dec (* 6 +truck-speed+))) ;vehicle should now be west of spawn, after
                                    ;having moved 5 cells
   (:coords (v/vehicle id)) => [4 5]
   (v/despawning? (v/vehicle id)) => falsey
   (tick 1) ;vehicle is on spawn, should start despawning
   (:coords (v/vehicle id)) => [5 5]
   (v/despawning? (v/vehicle id)) => truthy
   (tick (dec +truck-speed+))
   (v/vehicle id) => truthy
   (tick 1)     ;vehicle should be gone now
   (v/vehicle id) => nil
   ))

(spawn-fact
 "A vehicle that passes through a supply picks up a resource."
 (let [resource (hue :red)
       id (:id (m/spawn (w/cell [5 5])))]
   (b/build-supply (w/cell [6 6]) :red)
   (tick (dec (* 2.5 +truck-speed+))) ; just before entering cell 6 6,
                                    ; should have nothing
   (v/cargo? (v/vehicle id)) => false
   (tick 1) ; moved into the supply
   (v/cargo-color (v/vehicle id)) => (roughly (hue :red) 0.1)))

(spawn-fact
 "A vehicle that passes through a depot without having cargo does nothing."
 (b/build-depot (w/cell [6 6]) :red 1000)
 (tick 20)
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (v/cargo? (v/vehicle id))) => false)


(spawn-fact
 "A vehicle that passes through a depot after passing
through a supply with the same resource drops its resource."
 (b/build-supply (w/cell [6 6]) :green)
 (b/build-depot (w/cell [4 6]) :green 1000)
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick (dec (* 4.5 +truck-speed+))) ; should have cargo
   (v/cargo-color (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should drop cargo
   (v/cargo-color (v/vehicle id)) => nil))

(spawn-fact
 "A vehicle that passes through a depot after passing through
a supply with another resource drops its resource."
 (b/build-supply (w/cell [6 6]) :green)
 (b/build-depot (w/cell [4 6]) :red 1000)
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick (dec (* 4 +truck-speed+))) ; should have cargo
   (v/cargo-color (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1) ;at depot, should not drop cargo
   (v/cargo? (v/vehicle id)) => true))

(spawn-fact
 "A vehicle that passes through a mixer while carrying cargo drops
off the cargo"
 (b/build-supply (w/cell [6 5]) :green)
 (b/build-mixer (w/cell [6 6]))
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick (dec (* 2.5 +truck-speed+)))
   (v/cargo-color  (v/vehicle id)) => (roughly (hue :green) 0.1)
   (tick 1)
   (v/cargo-color (v/vehicle id)) => nil
   (:resource (w/cell [6 6])) => (just [(roughly (hue :green) 0.1) 1])
   (tick +truck-speed+)
   (v/cargo-color (v/vehicle id)) => nil
   (:resource (w/cell [6 6])) => (just [(roughly (hue :green) 0.1) 1])
   ))

(world-fact [10 10]
 "After delivering two colors to a mixer, the mixer contains a mixture"
 (b/build-mixer (w/cell [7 5]))
 (b/build-spawn (w/cell [5 5]) :truck)
 (p/build-path (w/cell [5 5]) :east :road)
 (b/build-supply (w/cell [6 5]) :red)
 (p/build-path (w/cell [6 5]) :east :road)
 (p/build-path (w/cell [7 5]) :east :road)
 (b/build-spawn (w/cell [7 3]) :truck)
 (p/build-path (w/cell [7 3]) :south :road)
 (b/build-supply (w/cell [7 4]) :green)
 (p/build-path (w/cell [7 4]) :south :road)
 (p/build-path (w/cell [7 5]) :south :road)
 (m/spawn (w/cell [5 5]))
 (m/spawn (w/cell [7 3]))
 (tick (* 2.5 +truck-speed+))
 (:resource (w/cell [7 5])) => (just [(roughly (hue :yellow) 0.1) 2])
 )

(world-fact [10 10]
 "After delivering two colors to a mixer with different vehicles,
the magnitude at the mixer should be the sum of the cargo counts"
 (b/build-mixer (w/cell [7 5]))
 (b/build-spawn (w/cell [5 5]) :truck)
 (p/build-path (w/cell [5 5]) :east :road)
 (b/build-supply (w/cell [6 5]) :red)
 (p/build-path (w/cell [6 5]) :east :road)
 (p/build-path (w/cell [7 5]) :east :road)
 (b/build-spawn (w/cell [8 5]) :truck)
 (b/build-spawn (w/cell [7 3]) :truck)
 (p/build-path (w/cell [7 3]) :south :road)
 (b/build-supply (w/cell [7 4]) :green)
 (p/build-path (w/cell [7 4]) :south :road)
 (m/spawn (w/cell [5 5]))
 (m/spawn (w/cell [7 3]))
 (tick (* 2.5 +truck-speed+))
 (:resource (w/cell [7 5]))
   => (just [(roughly (hue :yellow) 0.1) (* 2 (v/cargo-capacity :truck))]))

(spawn-fact
 "A vehicle that enters a spawn point with cargo incurs a penalty."
 (b/build-supply (w/cell [6 5]) :red)
 (let [id (:id (m/spawn (w/cell [5 5])))]
   (tick (* 6 +truck-speed+))
   (:coords (v/vehicle id)) => [5 5]
   (v/despawning? (v/vehicle id)) => truthy
   (tick (dec +truck-speed+))
   (v/vehicle id) => truthy
   (tick 1) ;vehicle should be gone now
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
    (b/build-supply (w/cell [4 5]) :red)
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
  (tick (* 3 +boat-speed+)) ;boat dropped off cargo
  (b/resource-color (w/cell [5 5])) => (roughly 0.0 0.01)
  (b/resource-count (w/cell [5 5])) => 10
  (let [id (:id (m/spawn (w/cell [5 4])))]
    (tick (dec (* 1.5 +boat-speed+)))
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
    (tick (* 1.5 (p/path->duration :rails)))
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
              (half-move id)
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
  (b/build-supply (w/cell [5 5]) :red)
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
    (half-move truck)
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
  (b/build-supply (w/cell [5 5]) :yellow)
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
  (b/build-supply (w/cell [5 5]) :red)
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

(world-fact [10 10]
            "When a nil route is added, vehicles without cargo should prefer this"
            (build-spawn (cell [5 4]) :truck)
            (build-path (cell [5 4]) :south :road)
            (doto (cell [5 5])
              (build-path :south :road)
              (build-path :east :road)
              (build-route :south nil))

            (with-spawned-vehicle [truck [5 4]]
              (tick-move truck)
              (:exit-direction (vehicle truck)) => :south))

(world-fact [10 10]
            "When a nil route is added, this shouldn't affect cargo-ed vehicles."
            (build-spawn (cell [5 4]) :truck)
            (build-path (cell [5 4]) :south :road)
            (build-path (cell [5 5]) :south :road)
            (build-supply (cell [5 5]) :red)
            (doto (cell [5 6])
              (build-path :south :road)
              (build-path :east :road)
              (build-route :south nil))

            (with-spawned-vehicle [truck [5 4]]
              (tick-move truck)
              (tick-move truck)
              (:exit-direction (vehicle truck)) => :east)

            (build-route (cell [5 6]) :south (hue :red))
            (with-spawned-vehicle [truck [5 4]]
              (tick-move truck)
              (tick-move truck)
              (:exit-direction (vehicle truck)) => :south))


(world-fact [10 10]
            "A vehicle for which no exit applies explodes and despawns"
            (b/build-spawn (w/cell [5 5]) :truck)
            (p/build-path (w/cell [5 5]) :east :road)
            (with-spawned-vehicle (truck [5 5])
              (tick-move truck)
              ;;truck is now in a cell with no valid exit
              (:exit-direction (v/vehicle truck)) => nil
              (v/exploding? (v/vehicle truck)) => truthy
              (tick (dec +truck-speed+))
              (v/vehicle truck) => truthy
              (tick)
              (v/vehicle truck) => nil))

(world-fact [10 10]
            "A vehicle spawned on a cell with no exit paths explodes."
            (b/build-spawn (w/cell [5 5]) :truck)
            (with-spawned-vehicle (truck [5 5])
              (:exit-direction (v/vehicle truck)) => nil
              (v/spawning? (v/vehicle truck)) => truthy
              (v/exploding? (v/vehicle truck)) => truthy
              (tick +truck-speed+)
              (v/vehicle truck) => nil))

(world-fact [10 10]
            "A vehicle for which no exit applies explodes even if there's a (non-spawn) building at that point"
            (b/build-spawn (w/cell [5 5]) :truck)
            (p/build-path (w/cell [5 5]) :east :road)
            (b/build-supply (w/cell [6 5]) :supply)
            (with-spawned-vehicle (truck [5 5])
              (tick (dec +truck-speed+))
              (v/exploding? (v/vehicle truck)) => falsey
              (tick)
              (v/exploding? (v/vehicle truck)) => truthy
              (tick (dec +truck-speed+))
              (v/vehicle truck) => truthy
              (tick)
              (v/vehicle truck) => nil
))

(spawn-fact "A vehicle sets all entry/exit times correctly"
            (with-spawned-vehicle (truck [5 5])
              (dotimes [i 7]
                (:entry-time (v/vehicle truck)) => (* i +truck-speed+)
                (:exit-time (v/vehicle truck)) => (* (inc i) +truck-speed+)
                (tick-move truck))))
