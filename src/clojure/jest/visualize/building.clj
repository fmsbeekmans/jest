(ns jest.visualize.building
  "Functions to facilitate the visualisation of the world state."
  (:require [jest.util :as u])
  (:require [brick.drawable :as drawable]
            [quil.core :as quil])
  (:require [jest.world.building :as building])
  (:require [jest.visualize.resource :as resource]))

(defn cell-building
  "Return the drawable for the building in cell c using the sprites in tile-fn."
  [tile-fn c]
  (if-let [type (building/building-type c)]
    (let [resource-vis (comp resource/drawable-from-resource-rate
                             resource/building-resource-rate)]
      (case type
        :spawn (drawable/->Nothing)
        :mixer (drawable/->Stack [(tile-fn :mixer)
                                  (resource-vis c)])
        :supply (drawable/->Stack [(tile-fn :supply)
                                   (resource-vis c)])
        :depot (drawable/->Stack [(tile-fn :depot)
                                  (resource-vis c)])
        :restricted (reify drawable/Drawable
                      (draw [this [w h]]
                        (quil/push-style)
                        (quil/fill 0 0 0)
                        (quil/rect 0 0 w h)
                        (quil/pop-style)))))
    (tile-fn nil)))

(defn cell-spawn
  "Return the drawable for a possible spawn in cell c using the sprites in tile-fn."
  [tile-fn c]
  (if-let [type (building/building-type c)]
    (let [resource-vis (comp resource/drawable-from-resource-rate
                             resource/building-resource-rate)]
      (if (= type :spawn)
        (tile-fn
         (u/hyphenate-keywords :spawn (building/vehicle-type c)))
        (drawable/->Nothing)))
    (tile-fn nil)))
