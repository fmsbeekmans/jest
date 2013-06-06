(ns jest.visualize.visualize-test
  (:use jest.testutils
        midje.sweet)
  (:require [jest.visualize :as visualize]
            [brick.drawable :as drawable]
            [jest.world.building :as building]))

(facts "Line"
  (fact "Line creates a 1-arity fn"
    (let [l (visualize/line [2] [4])]
      l => fn?
      (l) => (throws clojure.lang.ArityException)
      (l nil) =not=> (throws clojure.lang.ArityException)
      (l nil nil) => (throws clojure.lang.ArityException)))

  (fact "Line basic functionality"
    ((visualize/line [3] [7]) 0.5) => [5.0]
    ((visualize/line [3 4] [7 0]) 0.25) => [4.0 3.0]

    ((visualize/line [1 2 3 4 5 6] [4 5 6 7 8 9]) 0.5) => [2.5 3.5 4.5 5.5 6.5 7.5]

    ;begin
    ((visualize/line [3] [7]) 0) => [3.0]
    ;end
    ((visualize/line [3] [7]) 1) => [7.0])
  
  (fact "From point a to point a is point a"
    ((visualize/line [1.1] [1.1]) 0.9) => [1.1]
    ((visualize/line [1.1] [1.1]) 0.5) => [1.1]
    ((visualize/line [1.1] [1.1]) 0.1234) => [1.1]))

(world-fact [10 10]
            "layer from world state builds a valid grid drawable"
            (build-spawn-circle)
            (let [grid (visualize/layer-from-world-state (fn [c]
                                                         (if (building/spawn? c)
                                                           (drawable/->Stack [])
                                                           (drawable/->Nothing))))]
              (get-in grid [:grid [5 5]]) => (drawable/->Stack [])
              (get-in grid [:grid [5 6]]) => (drawable/->Nothing)))
