(ns jest.world.cell-test
  (:use midje.sweet
        jest.testutils)
  (:require [jest.world.cell :as cell]))

(fact "The world is initialized to the right size."
      (cell/with-temp-world
        (cell/initialize-world 10 10)
        (cell/world-size) => [10 10]))

(cell/with-initialized-temp-world [10 10]
  (fact "Each cell in the world grid exists."
        (for-cells [c x y] [10 10] c) => (n-of truthy 100))

  (fact "Each cell matches the coords that are used to look up this cell."
        (for-cells [c x y] [10 10]
                   (= (cell/coords c) [x y])) => (n-of true 100))

  (fact "The borders of the map are unconnected on their border edge."
        (doall (for [x '(0) y (range 10)]
                 (cell/direction-exists? (cell/cell [x y]) :west))) => (n-of false 10)
  
        (doall (for [x '(9) y (range 10)]
                 (cell/direction-exists? (cell/cell [x y]) :east))) => (n-of false 10)
  
        (doall (for [x (range 10) y '(0)]
                 (cell/direction-exists? (cell/cell [x y]) :north))) => (n-of false 10)
  
        (doall (for [x (range 10) y '(9)]
                 (cell/direction-exists? (cell/cell [x y]) :south))) => (n-of false 10))
  
  (fact "All non-border cells are connected to all sides."
        (doall (for [x (map inc (range 8)) y (map inc (range 8))]
                 (let [c (cell/cell [x y])]
                   (and
                    (cell/direction-exists? c :north)
                    (cell/direction-exists? c :south)
                    (cell/direction-exists? c :west)
                    (cell/direction-exists? c :east))))) => (n-of true 64))

  (fact "All cells are back-connected."
        (for-cells [c x y] [10 10]
                   (and (if (cell/direction-exists? c :north)
                          (= c (cell/direction (cell/direction c :north) :south))
                          true)
                        (if (cell/direction-exists? c :south)
                          (= c (cell/direction (cell/direction c :south) :north))
                          true)
                        (if (cell/direction-exists? c :west)
                          (= c (cell/direction (cell/direction c :west) :east))
                          true)
                        (if (cell/direction-exists? c :east)
                          (= c (cell/direction (cell/direction c :east) :west))
                          true))) => (n-of true 100)))
                        
                   