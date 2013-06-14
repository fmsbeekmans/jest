(ns jest.color-test
  (:use [midje.sweet :only [fact roughly]])
  (:require [jest.color :as c]))

(fact "The average of red and green is yellow"
      (c/average-hue (c/hue :red) (c/hue :green))
      => (roughly (c/hue :yellow) 0.01))

(fact "The hue difference between 0.1*pi and pi is 0.9*pi"
      (c/hue-difference (* 0.1 Math/PI) Math/PI)
      => (roughly (* 0.9 Math/PI) 0.01))

(fact "The hue difference between 0.1*pi and 1.9*pi is 0.2*pi"
      (c/hue-difference (* 0.1 Math/PI) (* 1.9 Math/PI))
      => (roughly (* 0.2 Math/PI) 0.01))
