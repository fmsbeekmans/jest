(ns jest.testutils
  (:require [jest.world.cell :as cell]))

(defmacro for-cells
  "test macro, give sx and sy and it will do something for each cell."
  [[cell x y] [sx sy] & body]
  `(doall (for [~x (range ~sx)
                ~y (range ~sy)]
            (let [~cell (cell/cell [~x ~y])]
              ~@body))))

