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
        jest.input.core
        jest.vehicle
        jest.world.route))


(def inv-directions (clojure.set/map-invert directions))

(defn build-dumb-level []
  (initialize-world 8 8)
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

  (build-depot (cell [4 3]) :red)
  (build-depot (cell [4 4]) :blue)

  (build-spawn (cell [5 2]) :truck))

(defn stupid-on-down [id pos]
  (case pos
    [1 0]
    (spawn (cell [1 1]))
    [2 0]
    (spawn (cell [2 1]))

    [0 0]
    (if (paused?)
      (resume!)
      (pause!))
    nil))

(defn maybe-build-route [c dir]
  (if-let [v (first (vehicles c))]
    (when (cargo? v)
      (doseq [p (paths-with-route c (cargo-color v))]
        (unbuild-route c (:direction p) (cargo-color v)))
      (build-route c dir (cargo-color v))
      (dosync 
       (doseq [vehicle (vehicles c)]
         (update-vehicle (:id vehicle)
                         #(assoc %
                            :exit-direction (:direction  (preferred-path %)))))))))

(defn rough-staging-on-move [id pos1 pos2]
  (let [c1 (cell pos1)
        c2 (cell pos2)
        direction (inv-directions (map - pos2 pos1))]
    (if (path c1 direction)
      (if (in-path? (path c1 direction))
        (unbuild-path c1 direction)
        (maybe-build-route c1 direction)
)
      (if (or (seq (in-paths c1))
              (spawn? c1))
        (build-path c1 direction :road)))
    ))

(defn user-setup []
  (scheduler-reset!)
  (setup-quil-mouse-input)
  (set-input-handler! :on-move #(rough-staging-on-move %1 %2 %3))
  (set-input-handler! :on-down #(stupid-on-down %1 %2))
  (load-level "levels/alpha_ugly.json")
  (build-dumb-level)

  (start!)
  (println "Started?" (started?))
;  (pause!)
;  (build-spawn (cell [4 2]) :truck)
;  (println "build spawn")
;  (spawn (cell [4 2]))
;  (println "Spaned car")
;  (stop!)
  (sketch!))
