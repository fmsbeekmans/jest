(ns jest.input.interaction-test
  (:require [midje.sweet :refer :all]
            [jest.input.interaction :as i]
            [jest.testutils :refer :all]
            [jest.world :refer [cell]]
            [jest.world.building :refer [build-spawn build-supply]]
            [jest.world.path :refer [path->duration build-path paths path]]
            [jest.world.vehicle :refer [vehicles moving? exploding?]]
            [jest.behavior.spawn :refer [spawn]]
            [jest.scheduler :refer [game-time]]
            [jest.color :refer [hue]]))

(interaction-fact [5 5]
                  "Upon adding a route, vehicles affected by that route are updated."
                  (build-spawn (cell [1 1]) :truck)
                  (build-path (cell [1 1]) :south :road)
                  (build-supply (cell [1 2]) :red)
                  (build-path (cell [1 2]) :south :road)
                  (doseq [dir [:south :west :east]]
                    (build-path (cell [1 3]) dir :road))
                  
                  (spawn (cell [1 1]))
                  (tick (/ (path->duration :road) 2))
                  (spawn (cell [1 1]))

                  (tick (* 2  (path->duration :road)))

                  ;;ensure all vehicles are in the cell
                  (count (vehicles (cell [1 3]))) => 2
                  (gesture 1 [1 3] [1 4])

                  ;;route is set
                  (:routes (path (cell [1 3]) :south)) => (contains [(hue :red)])


                  (let [vs (vehicles (cell [1 3]))]
                    (count vs) => 2
                    (count (filter moving? vs)) => 2

                    (letfn [(exit= [dir v]
                              (= (:exit-direction v) dir))]
                      (count (filter (partial exit= :south) vs)) => 1
                      (count (filter (partial exit= :east) vs)) => 1)))

(interaction-fact [5 5]
                  "When trying to build a route while the only vehicle is on an out path, do not build the route."
                  (build-spawn (cell [1 1]) :truck)
                  (doseq [dir [:east :south]]
                    (build-path (cell [1 1]) dir :road))
                  (spawn (cell [1 1]))
                  (tick (/ (path->duration :road) 2))
                  (gesture 1 [1 1] [1 2])
                  (:routes (path (cell [1 1]) :south)) => empty?)
