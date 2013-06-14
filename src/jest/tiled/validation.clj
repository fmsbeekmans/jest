(ns jest.tiled.validation
  "Functions for validating game-levels"
  (:require [clojure.data.json :as json]
            [jest.util :as util]
            [jest.util.json-validation :as json-val]
            [clojure.core.incubator :refer [-?>]]))

;; Json utility functions
; A JSON description of the tiled-map is converted to a clojure map,
; with keywords for keys.

(defn read-json
  "Reads json-data as a clojure data-structure from json-path, which
  should be a valid path or URL object"
  [json-path]
  (json/read-str (slurp json-path)
                 :key-fn keyword))

(def meta-schema-url
  "Location of the schema-validating schema"
  (clojure.java.io/resource "meta.schema"))

(def level-schema-url
  "Location of the level-validating schema"
  (clojure.java.io/resource "level.schema"))

(defn create-validator
  "Constructs a validator fn for schema-path by iff the application of
  f to the internal json representation of the schema-file results in
  a not-nil value. The validator expects a json data-structure"
  [schema-path f]
  (let [level-validator
        (-?> schema-path
             read-json
             f
             json-val/validator
             json-val/boolean-validator)]
    (or level-validator
        (constantly nil))))
