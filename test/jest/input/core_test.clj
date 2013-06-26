(ns jest.input.core_test
  (:use midje.sweet)
  (:require [jest.input.core :as ic]
            [jest.visualize.visualize :as v]
            [jest.world :as w]))

(fact "pixel->tile correctly calculates a tile based on a pixel"
      (ic/pixel->tile 0 0) => [0 0]
      (ic/pixel->tile 12 22) => [1 2]
      (ic/pixel->tile 200 180) => [9 9]
      (ic/pixel->tile -12 -15) => [0 0]
      (against-background
       (v/sketch-size) => [100 100]
       (w/world-size) => [10 10]))
