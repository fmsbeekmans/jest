(ns jest.visualize.building
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)
  (:require [brick.drawable :as drawable]
            [quil.core :as quil])
  (:require [jest.world.building :as building])
  (:require [jest.visualize.resource :as resource]))

(defn cell-building
  "Which building tile-key fits this cell?"
  [tile-fn c]
  (if-let [type (building/building-type c)]
    (let [resource-vis (comp resource/drawable-from-resource-rate
                             resource/building-resource-rate)]
      (case type
        :spawn (tile-fn
                (hyphenate-keywords :spawn (building/vehicle-type c)))
        :mixer (drawable/->Stack [(tile-fn :mixer)
                                  (resource-vis c)])
        :supply (drawable/->Stack [(tile-fn :supply-red)
                                   (resource-vis c)])
        :depot (drawable/->Stack [(tile-fn :dirt)
                                  (resource-vis c)])))
    (tile-fn nil)))
