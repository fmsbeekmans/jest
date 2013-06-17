(ns user
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.vehicle
        jest.world
        jest.movement

        jest.visualize.visualize

        jest.testutils
        jest.scheduler))

(initialize-world 10 10)
(build-spawn-circle)

(if (started?)
  (stop!))

(start!)
(spawn (cell [5 5]))
(world->drawable)
