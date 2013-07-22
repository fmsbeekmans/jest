(ns jest.level
  (:require [jest.tiled.validation :as validation]
            [jest.tiled.import :as import]
            [jest.world :as world :refer [reset-world]]
            [jest.scheduler :refer [scheduler-reset! start!]]
            [jest.movement :refer [start-spawning stop-spawning]]
            [jest.score :refer [reset-score]]
            [clojure.core.incubator :refer [-?>]]))

(def valid-schema?
  (validation/create-validator validation/meta-schema-url identity))

(def valid-level?
  (validation/create-validator validation/level-schema-url valid-schema?))

(defn load-level [level-resource-path]
  (let [level
        (-?> level-resource-path
             clojure.java.io/resource
             validation/read-json
             valid-level?
             import/parse-world)]
    level))

(def current-level (atom nil))

(defn initialize-level []
  (stop-spawning)
  (reset-world {})
  (scheduler-reset!)
  (reset-score))

(defn reset-level []
  (start-level @current-level))

(defn start-level [level-fn]
  (reset! current-level level-fn)
  (initialize-level)

  (level-fn)
  (start!)
  (start-spawning)
  )

(defn stop-level []
  (stop-spawning))
