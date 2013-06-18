(ns user
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.visualize.visualize
        jest.world
        jest.movement
        jest.util
        jest.level
        
        brick.drawable

        jest.testutils
        jest.scheduler))


(defn user-setup []
  (load-level "levels/alpha_ugly.json")
  (drawable->sketch! @world-sketch))

;(user-setup)



