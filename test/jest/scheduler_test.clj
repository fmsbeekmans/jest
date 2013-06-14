(ns jest.scheduler-test
  (:use midje.sweet
        jest.testutils
        jest.color)
  (:require [jest.scheduler :as s])
  (:import java.util.Date))

(defmacro scheduler-fact [fact-text & body]
  `(fact ~fact-text
         (s/scheduler-reset!)
         ~@body
         (s/scheduler-reset!)))

(scheduler-fact
  "Before starting the scheduler,
the scheduler is neither started nor paused"
  (s/started?) => FALSEY
  (s/paused?) => FALSEY)

(scheduler-fact
  "After starting the scheduler,
the scheduler is started and not paused"
  (s/start!)
  (s/started?) => TRUTHY
  (s/paused?) => FALSEY)

(scheduler-fact
  "After pausing the scheduler,
the scheduler is started and paused"
  (s/start!)
  (s/pause!)
  (s/started?) => TRUTHY
  (s/paused?) => TRUTHY)

(scheduler-fact
  "After resuming a paused scheduler,
the scheduler is started and not paused"
  (s/start!)
  (s/pause!)
  (s/resume!)
  (s/started?) => TRUTHY
  (s/paused?) => FALSEY)

(scheduler-fact
  "After stopping a running scheduler,
the scheduler is stopped and not paused"
  (s/start!)
  (s/stop!)
  (s/started?) => FALSEY
  (s/paused?) => FALSEY)

(scheduler-fact
  "After stopping a paused scheduler,
 the scheduler is stopped and not paused"
  (s/start!)
  (s/pause!)
  (s/stop!)
  (s/started?) => FALSEY
  (s/paused?) => FALSEY)

(fact
  "@game-time returns the value returned by (calculate-game-time)"
  @s/game-time => ..game-time..
  (provided (s/calculate-game-time) => ..game-time..))

(scheduler-fact
  "A fact that is scheduled is run"
  (s/start!)
  (let [returned (promise)]
    (s/schedule #(deliver returned :done) 0)
    @returned => :done))

(scheduler-fact
  "facts that are scheduled while paused are run after resuming."
  (s/start!)
  (s/pause!)
  (let [results (for [i (range 3)] (promise))]
    (doseq [r results]
      (s/schedule #(deliver r :done) 0))
    (s/resume!)
    (doseq [r results]
      @r => :done)))


(fact
  "offset calculates right offsets"
  (s/offset 1) => 9001
  (provided (s/calculate-game-time) => 9000)
  (s/offset 2 :milliseconds) => 9002
  (provided (s/calculate-game-time) => 9000)
  (s/offset 3 :seconds) => 12000
  (provided (s/calculate-game-time) => 9000)
  (s/offset 4 :minutes) => 249000
  (provided (s/calculate-game-time) => 9000))
