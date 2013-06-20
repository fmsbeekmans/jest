(ns user
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.visualize.visualize
        jest.world
        jest.world.building
        jest.movement
        jest.util
        jest.world.path
        jest.level
        
        brick.drawable

        jest.testutils
        jest.scheduler))


(defn user-setup []
  (scheduler-reset!)
  (load-level "levels/alpha_ugly.json")
  
  (start!)
  (println "Started?" (started?))
  (pause!)
  (build-spawn (cell [4 2]) :truck)
  (println "build spawn")
  (spawn (cell [4 2]))
  (println "Spaned car")
  (stop!)
  (sketch!))
