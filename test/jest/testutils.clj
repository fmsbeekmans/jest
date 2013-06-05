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
     (with-mock-scheduler
       (fact ~fact-text ~@body))))

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
  "mock version of calculate-game-time. Always returns the current mock time, which can be forwarded using tick."
  []
  @mock-game-time)

(defn mock-schedule
  "mock version for schedule. Ignores the schedule time and runs the function right away. Useful in tests."
  [task time]
  (let [mockagent (agent nil)]
    (send mockagent (fn [_]
                      (if (<= time @scheduler/game-time)
                        (task)
                        (swap! mock-tasks update-in [time] conj task))))))

(defn tick
  "forwards the scheduler n milliseconds, running all scheduled tasks in order. By default n is 1"
  ([] (tick 1))
  ([n] (dotimes [i n]
         (swap! mock-game-time inc)
         (doseq [task (@mock-tasks @scheduler/game-time)]
           (task)))
     @scheduler/game-time))

(defmacro with-mock-scheduler [& body]
  `(with-redefs [mock-game-time (atom 0)
                 mock-tasks (atom {})
                 scheduler/schedule mock-schedule
                 scheduler/calculate-game-time mock-calculate-game-time]
     ~@body))

(def tmp-schedule)
(def tmp-calculate-game-time)

(defn idfn [x] (fn [& _] x))

(defn swap-scheduler []
  (if (= scheduler/schedule mock-schedule)
    (do (alter-var-root #'scheduler/schedule (idfn tmp-schedule))
        (alter-var-root #'scheduler/calculate-game-time (idfn tmp-calculate-game-time))
        (alter-var-root #'mock-game-time (idfn nil))
        (alter-var-root #'mock-tasks (idfn nil)))

    (do (alter-var-root #'tmp-schedule (idfn scheduler/schedule))
        (alter-var-root #'tmp-calculate-game-time (idfn scheduler/calculate-game-time))
        (alter-var-root #'mock-game-time (idfn (atom 0)))
        (alter-var-root #'mock-tasks (idfn (atom {})))
        (alter-var-root #'scheduler/schedule (idfn mock-schedule))
        (alter-var-root #'scheduler/calculate-game-time (idfn mock-calculate-game-time)))))
