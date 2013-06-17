(ns user
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.visualize.visualize
        jest.world
        jest.movement

        brick.drawable

        jest.testutils
        jest.scheduler))


(defn main []
  (initialize-world 10 10)
  (build-spawn-circle)

  (if (started?)
    (stop!))

  (start!)
  (spawn (cell [5 5]))

  (drawable->sketch! (world->drawable)))

