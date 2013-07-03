(ns jest.color-test
  (:use [midje.sweet :only [fact roughly]])
  (:require [jest.color :as c]))

(fact "The average of red and green is yellow"
      (c/average-hue (c/hue :red) (c/hue :green))
      => (roughly (c/hue :yellow) 0.01))

(fact "hue->int translates a hue to a 255-normalized int"
  (c/hue->int 360) => 0
  (c/hue->int 180) => (roughly 127 1)
  (c/hue->int 720) => 0)

(fact "hue->hsb gives the wanted color vec"
  (c/hue->hsb 360) => [0 255 255]
  (c/hue->hsb 180) => [127 255 255]
  (c/hue->hsb 720) => [0 255 255]

  (c/hue->hsb nil) => [255 0 255]
  (c/hue->hsb :red) => [0 255 255])

