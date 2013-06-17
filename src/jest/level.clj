(ns jest.level
  (:require [jest.tiled.validation :as validation]
            [jest.tiled.import :as import]
            [jest.world :as world]
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
