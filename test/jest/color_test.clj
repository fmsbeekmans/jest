(ns jest.color-test
  (:use [midje.sweet :only [fact roughly]])
  (:require [jest.color :as c]))

(fact "The average of red and green is yellow"
      (c/average-hue (c/hue :red) (c/hue :green))
      => (roughly (c/hue :yellow) 0.01))


