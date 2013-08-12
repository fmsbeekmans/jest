(ns jest.testutils
  (:require [jest.scheduler :as scheduler])
  (:use [jest.world :only [cell]]
        [jest.world.cell :only [with-initialized-temp-world]]
        [jest.world.path :only [build-path complete-paths]]
        [jest.world.building :only [build-spawn]]
        [jest.score :only [reset-score]]
        [jest.input.interaction :only [interaction-setup on-down on-move on-up]]
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
     (reset-score)
     (with-mock-scheduler
       (fact ~fact-text ~@body))))

(defmacro interaction-fact [[sx sy] fact-text & body]
  `(world-fact [~sx ~sy] ~fact-text
               (interaction-setup)
               ~@body))

(defn build-spawn-circle []
  {:post [(seq (complete-paths (cell [5 5])))]}
  (build-spawn (cell [5 5]) :truck)
  (build-path (cell [5 5]) :east :road)
  (build-path (cell [6 5]) :south :road)
  (build-path (cell [6 6]) :west :road)
  (build-path (cell [5 6]) :west :road)
  (build-path (cell [4 6]) :north :road)
  (build-path (cell [4 5]) :east :road))

(def mock-game-time)
(def mock-tasks)

(defn mock-calculate-game-time
  "mock version of calculate-game-time. Always returns the current mock time,
which can be forwarded using tick."
  []
  @mock-game-time)

(defn mock-schedule
  "mock version for schedule. Ignores the schedule time and
runs the function right away. Useful in tests."
  [task time]
  (dosync
   (if (<= time @scheduler/game-time)
     (task)
     (alter mock-tasks update-in [time] conj task))))

(defn tick
  "forwards the scheduler n milliseconds,
running all scheduled tasks in order.By default n is 1"
  ([] (tick 1))
  ([n] (dotimes [_ n]
         (swap! mock-game-time inc)
         (doseq [task (@mock-tasks @scheduler/game-time)]
           (task)))
     @scheduler/game-time))

(defn new-mock-game-time [] (atom 0) )
(defn new-mock-tasks [] (ref {}))

(defmacro with-mock-scheduler [& body]
  `(with-redefs [mock-game-time (new-mock-game-time)
                 mock-tasks (new-mock-tasks)
                 scheduler/schedule mock-schedule
                 scheduler/calculate-game-time mock-calculate-game-time]
     ~@body))

(def tmp-schedule)
(def tmp-calculate-game-time)

(defn swap-scheduler []
  (if (= scheduler/schedule mock-schedule)
    (do (alter-var-root #'scheduler/schedule
                        (constantly tmp-schedule))
        (alter-var-root #'scheduler/calculate-game-time
                        (constantly tmp-calculate-game-time))
        (alter-var-root #'mock-game-time
                        (constantly (new-mock-game-time)))
        (alter-var-root #'mock-tasks
                        (constantly (new-mock-tasks))))

    (do (alter-var-root #'tmp-schedule
                        (constantly scheduler/schedule))
        (alter-var-root #'tmp-calculate-game-time
                        (constantly scheduler/calculate-game-time))
        (alter-var-root #'mock-game-time
                        (constantly (new-mock-game-time)))
        (alter-var-root #'mock-tasks
                        (constantly (new-mock-tasks)))
        (alter-var-root #'scheduler/schedule
                        (constantly mock-schedule))
        (alter-var-root #'scheduler/calculate-game-time
                        (constantly mock-calculate-game-time)))))

(defn vec-roughly
  [target margin]
  ; make ordered roughly-fns
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


(defn gesture [id & [fp & points]]
  (on-down id fp)
  (loop [prev fp
         [p & points] points]
    (when-not (nil? p)
      (on-move id prev p)
      (recur p points)))
  (on-up id (last points)))

(defmacro with-spawned-vehicle [[vehicle spawn-location] & body]
  `(let [~vehicle (:id (s/spawn (w/cell ~spawn-location)))]
     ~@body))
