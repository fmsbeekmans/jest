(ns jest.testutils
  (:require [jest.scheduler :as scheduler])
  (:use [jest.world.cell :only [cell with-initialized-temp-world]]
        [jest.world.path :only [build-path complete-paths]]
        [jest.world.building :only [build-spawn]]
        midje.sweet))
         

(defmacro for-cells
  "test macro, give sx and sy and it will do something for each cell."
  [[cell x y] [sx sy] & body]
  `(doall (for [~x (range ~sx)
                ~y (range ~sy)]
            (let [~cell (cell [~x ~y])]
              ~@body))))

(defmacro world-fact [[sx sy] fact-text & body]
  `(with-initialized-temp-world [~sx ~sy]
     (fact ~fact-text ~@body)))

(defn build-spawn-circle []
  {:post [(seq (complete-paths (cell [5 5])))]}
  (build-spawn (cell [5 5]) :truck)
  (build-path (cell [5 5]) :east :road)
  (build-path (cell [6 5]) :south :road)
  (build-path (cell [6 6]) :west :road)
  (build-path (cell [5 6]) :west :road)
  (build-path (cell [4 6]) :north :road)
  (build-path (cell [4 5]) :east :road))

(defn mock-schedule
  "mock version for schedule. Ignores the schedule time and runs the function right away. Useful in tests."
  [task time]
  (task))

(defn mock-calculate-game-time
  "mock version of calculate-game-time. Always returns 0."
  []
  0)

(defmacro with-mock-scheduler [& body]
  `(with-redefs [scheduler/schedule mock-schedule
                 scheduler/calculate-game-time mock-calculate-game-time]
     ~@body))

(defn vec-roughly
  [target margin]
  ; maak roughly-fn's aan in volgorde.
  (fn [actual]
    (= (count actual)
       (count (take-while truthy
                          (map (fn [t a]
                                 ((roughly t margin) a))
                               target
                               actual))))))

(defn roughly-angle
  [target margin]
  (fn [actual]
    (or
     ((roughly (mod (+ (* 2 Math/PI) target) (* 2 Math/PI)) margin)
      (mod (+ (* 2 Math/PI) actual) (* 2 Math/PI)))
     ((roughly (mod (+ (* 3 Math/PI) target) (* 2 Math/PI)) margin)
      (mod (+ (* 3 Math/PI) actual) (* 2 Math/PI))))))
