(ns jest.world.cell-test
  (:use midje.sweet
        jest.testutils)
  (:require [jest.world.cell :as cell]
            [jest.world :as world]))

(fact "The world is initialized to the right size."
  (world/with-temp-world
    (cell/initialize-world 10 10)
    (world/world-size) => [10 10]))

(cell/with-initialized-temp-world [10 10]
  (fact "Each cell in the world grid exists."
    (for-cells [c x y] [10 10] c) => (n-of truthy 100))

  (fact "Each cell matches the coords that are used to look up this cell."
    (for-cells [c x y] [10 10]
                   (= (world/coords c) [x y])) => (n-of true 100))

  (fact "The borders of the map are unconnected on their border edge."
     (doall (for [x '(0) y (range 10)]
              (world/direction-exists? (world/cell [x y]) :west)))
                => (n-of false 10)

     (doall (for [x '(9) y (range 10)]
              (world/direction-exists? (world/cell [x y]) :east)))
                => (n-of false 10)

     (doall (for [x (range 10) y '(0)]
              (world/direction-exists? (world/cell [x y]) :north)))
                => (n-of false 10)

     (doall (for [x (range 10) y '(9)]
              (world/direction-exists? (world/cell [x y]) :south)))
                => (n-of false 10))

  (fact "All non-border cells are connected to all sides."
    (doall (for [x (map inc (range 8)) y (map inc (range 8))]
             (let [c (world/cell [x y])]
               (and
                (world/direction-exists? c :north)
                (world/direction-exists? c :south)
                (world/direction-exists? c :west)
                (world/direction-exists? c :east))))) => (n-of true 64))

  (fact "All cells are back-connected."
    (for-cells [c x y] [10 10]
               (and (if (world/direction-exists? c :north)
                      (= c (world/direction (world/direction c :north) :south))
                      true)
                    (if (world/direction-exists? c :south)
                      (= c (world/direction (world/direction c :south) :north))
                      true)
                    (if (world/direction-exists? c :west)
                      (= c (world/direction (world/direction c :west) :east))
                      true)
                    (if (world/direction-exists? c :east)
                      (= c (world/direction (world/direction c :east) :west))
                      true))) => (n-of true 100)))

(cell/with-initialized-temp-world [1 1]
        (let [c #(world/cell [0 0])]
          (fact "Cell has no background when constructed"
                (:background (c)) => nil)
          (fact "Cell background can be set"
                (do
                  (cell/set-background (c) ..anything..)
                  (:background (c)) => ..anything..))))
