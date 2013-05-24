(ns jest.testutils
  (:use [jest.world.cell :only [cell]]
        [jest.world.path :only [build-path complete-paths]]
        [jest.world.building :only [build-spawn]]))

(defmacro for-cells
  "test macro, give sx and sy and it will do something for each cell."
  [[cell x y] [sx sy] & body]
  `(doall (for [~x (range ~sx)
                ~y (range ~sy)]
            (let [~cell (cell/cell [~x ~y])]
              ~@body))))

(defmacro world-fact [[sx sy] fact-text & body]
  `(cell/with-initialized-temp-world [~sx ~sy]
     (fact ~fact-text ~@body)))

(defn build-spawn-circle []
  {:post [(seq (complete-paths (cell [5 5])))]}
  (build-spawn (cell [5 5]) :fireball)
  (build-path (cell [5 5]) :east :pipe)
  (build-path (cell [6 5]) :south :pipe)
  (build-path (cell [6 6]) :west :pipe)
  (build-path (cell [5 6]) :west :pipe)
  (build-path (cell [4 6]) :north :pipe)
  (build-path (cell [4 5]) :east :pipe))
