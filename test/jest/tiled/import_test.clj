(ns jest.tiled.import_test
  (:use midje.sweet
        jest.testutils
        [jest.world.cell :only [with-initialized-temp-world cell all-cells coords]]
        [clojure.test :only [deftest]])
  (:require [jest.world.building :as building]))
