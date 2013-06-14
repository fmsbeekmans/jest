(ns jest.tiled.validation-test
  (:use midje.sweet
        jest.testutils
        [clojure.test :only [deftest]])
  (:require [jest.tiled.validation :as validation]))

(def simple-level
  {:height 4
   :width 4
   :tileheight 32
   :tilewidth 32
   :tilesets []
   :layers []})

(def simple-level-string
  "{\"height\": 4
\"width\":4
\"tileheight\": 32
\"tilewidth\": 32
\"tilesets\": []
\"layers\": []}")

(def invalid-level {})

(def valid-schema?
  (validation/create-validator validation/meta-schema-url identity))

(def valid-level?
  (validation/create-validator validation/level-schema-url valid-schema?))

(fact "The meta-schema-url should not be nil"
  validation/meta-schema-url =not=> nil)

(fact "The level-schema-url should not be nil"
  validation/level-schema-url =not=> nil)

(fact "a validator which somewhere along it's creation
process stumbles upon a nil, should always return nil"
  (let [nil-all (validation/create-validator nil anything)]
    (nil-all anything) => nil))

(fact "valid-level? should validate and return a valid level"
  (valid-level? simple-level) => simple-level)

(fact "valid-level? should invalidate an invalid-level and return nil"
  (valid-level? invalid-level) => nil)

(fact "Reading a valid json-string should lead to a valid json-structure"
  (validation/read-json ..some-file..) => simple-level
  (provided
   (slurp ..some-file..) => simple-level-string))
