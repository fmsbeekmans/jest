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
        jest.scheduler

        jest.input.quil
        jest.input.core))


(def inv-directions (clojure.set/map-invert directions))


(defn rough-staging-on-move [id pos1 pos2]
  (let [c1 (cell pos1)
        c2 (cell pos2)
        direction (inv-directions (map - pos2 pos1))]
    (if (path c1 direction)
      (if (in-path? (path c1 direction))
        (unbuild-path c1 direction))
      (build-path c1 direction :road))
    ))

(defn user-setup []
  (scheduler-reset!)
  (setup-quil-mouse-input)
  (set-input-handler! :on-move #(rough-staging-on-move %1 %2 %3))
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
