;(set! *warn-on-reflection* true)

(ns user
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.visualize.resource
        [jest.visualize.visualize :exclude [min-borders]]
        jest.visualize.util
        jest.world
        jest.world.building
        jest.movement
        jest.util
        jest.world.path
        jest.level
        jest.input.highlight
        jest.color
        jest.score

        brick.drawable
        brick.image

        jest.testutils
        jest.scheduler

        jest.input.quil
        jest.input.wm-touch
        jest.input.core
        jest.input.interaction
        jest.vehicle
        jest.world.route))

(defn build-level []
  (initialize-world 20 14)
  (build-spawn (cell [1 1]) :truck)
  (build-path (cell [1 1]) :south :road)
  (build-supply (cell [1 2]) :blue)
  (build-path (cell [1 2]) :south :road)
  (build-path (cell [1 3]) :east :road)
  (build-spawn (cell [2 1]) :truck)
  (build-path (cell [2 1]) :south :road)
  (build-supply (cell [2 2]) :red)
  (build-path (cell [2 2]) :south :road)
  (build-path (cell [2 3]) :east :road)

  (build-depot (cell [4 3]) :red 1000)
  (build-depot (cell [4 4]) :blue 1000)

  (build-spawn (cell [5 2]) :truck))


(defn build-another-level []
  (initialize-world 8 8)
  (build-spawn (cell [1 1]) :truck)
  (build-spawn (cell [2 1]) :truck)
  (build-spawn (cell [3 1]) :truck)
  (build-supply (cell [1 2]) :red)
  (build-supply (cell [2 2]) :green)
  (build-mixer (cell [3 3]))
  (build-spawn (cell [4 3]) :truck)
  (build-depot (cell [3 4]) :yellow 1000)
  (build-spawn (cell [3 5]) :truck)

  (build-path (cell [1 1]) :south :road)
  (build-path (cell [2 1]) :south :road)
  (build-path (cell [3 1]) :south :road)
  (build-path (cell [1 2]) :south :road)
  (build-path (cell [2 2]) :south :road)
  (build-path (cell [3 2]) :south :road)
  (build-path (cell [1 3]) :east :road)
  (build-path (cell [2 3]) :east :road)
  (build-path (cell [3 3]) :east :road)
  (build-path (cell [3 3]) :south :road)
  (build-path (cell [3 4]) :south :road))

(defn common-setup []
  (scheduler-reset!)
  (reset-score)
  (interaction-setup)
  (load-level "levels/alpha_ugly.json")
  (build-another-level)

  (build-spawn (cell [4 2]) :truck)
  (start!)
  (pause!)
)

(defn user-setup []
  (setup-quil-mouse-input)
  (common-setup)
  (sketch!))

(defn user-setup-touch []
  (common-setup)
  (sketch!)
  (undecorate-sketch @world-sketch)
  (ensure-wm-touch-input-setup!))
