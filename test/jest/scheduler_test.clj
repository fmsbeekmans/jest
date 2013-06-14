(ns jest.scheduler-test
  (:use midje.sweet
        jest.testutils
        jest.color)
  (:require [jest.scheduler :as s])
  (:import java.util.Date))

(defmacro scheduler-fact [fact-text & body]
  `(fact ~fact-text
         (s/scheduler-reset!)
         ~@body))

(scheduler-fact "Before starting the scheduler, the scheduler is neither started nor paused"
                (s/started?) => FALSEY
                (s/paused?) => FALSEY
                )

(scheduler-fact "After starting the scheduler, the scheduler is started and not paused"
                (s/start!)
                (s/started?) => TRUTHY
                (s/paused?) => FALSEY)

(scheduler-fact "After pausing the scheduler, the scheduler is started and paused"
                (s/start!)
                (s/pause!)
                (s/started?) => TRUTHY
                (s/paused?) => TRUTHY)

(scheduler-fact "After resuming a paused scheduler, the scheduler is started and not paused"
                (s/start!)
                (s/pause!)
                (s/resume!)
                (s/started?) => TRUTHY
                (s/paused?) => FALSEY)

(scheduler-fact "After stopping a running scheduler, the scheduler is stopped and not paused"
                (s/start!)
                (s/stop!)
                (s/started?) => FALSEY
                (s/paused?) => FALSEY)

(scheduler-fact "After stopping a paused scheduler, the scheduler is stopped and not paused"
                (s/start!)
                (s/pause!)
                (s/stop!)
                (s/started?) => FALSEY
                (s/paused?) => FALSEY)
