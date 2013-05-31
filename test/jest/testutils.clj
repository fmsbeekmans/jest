(ns jest.testutils
  (:require [jest.world.cell :as cell])
  (:use midje.sweet))

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
