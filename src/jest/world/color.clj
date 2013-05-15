(ns jest.color
  "Resource colors and some helper functions.")

;; This is not supposed to be in the public API
(def color-set
  #{:red
    :green
    :blue
    :white
    :black})

(defn colors
  "Returns the set of principal resource colors used in the game"
  []
  (set color-set))

(defn nearest-color
  "Returns the color that is nearest to the colors used in-game"
  [color]
  (or (color-set color)
      (:white)))
