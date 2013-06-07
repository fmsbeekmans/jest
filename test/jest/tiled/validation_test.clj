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

(def invalid-level
  {})

(fact "valid-level? should validate and return a simple-level"
      (validation/valid-level? simple-level) => simple-level)

(fact "valid-level? should invalidate an invalid-level and return nil"
      (validation/valid-level? invalid-level) => nil)

(fact "Reading a valid json-string should lead to a valid json-structure"
      (validation/read-json anything) => simple-level
      (provided
       (slurp anything) => simple-level-string))
