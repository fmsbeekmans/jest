(ns jest.tiled.validation
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.util :as util]
            [jest.util.json-validation :as json-val]
            [clojure.core.incubator :refer [-?>]]))

;; Json utility functions
; A JSON description of the tiled-map is converted to a clojure map,
; with keywords for keys.

(defn read-json
  [json-path]
  (json/read-str (slurp json-path)
                 :key-fn keyword))

(def meta-schema-url (clojure.java.io/resource "meta.schema"))
(def level-schema-url (clojure.java.io/resource "level.schema"))

(defn create-validator [schema f]
  (let [level-validator
        (-?> schema
             read-json
             f
             json-val/validator
             json-val/boolean-validator)
        wrapper (or level-validator
                    (fn [_] nil))]
    (fn [json-schema]
      (wrapper json-schema))))
