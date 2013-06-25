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
        jest.input.interaction
        jest.vehicle
        jest.world.route))

(defn build-level []
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


(defn build-another-level []
  (initialize-world 8 8)
  (build-spawn (cell [1 1]) :truck)
  (build-spawn (cell [2 1]) :truck)
  (build-spawn (cell [3 1]) :truck)
  (build-supply (cell [1 2]) :red)
  (build-supply (cell [2 2]) :green)
  (build-mixer (cell [3 3]))
  (build-spawn (cell [4 3]) :truck)
  (build-depot (cell [3 4]) :yellow)
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

(let [get-frame (comp :target-obj meta)
      decorate-sketch
      (fn [sketch decorate]
        (let [frame-atom (get-frame sketch)]
          (println frame-atom)
          (swap! frame-atom
                 #(doto %
                    (.dispose)
                    (.setUndecorated (not decorate))
                    (.setVisible true)))))]
  (defn undecorated-sketch
    [sketch]
    (decorate-sketch sketch false))
  (defn decorated-sketch
    [sketch]
    (decorate-sketch sketch true)))

(defn user-setup []
  (scheduler-reset!)
  (setup-quil-mouse-input)
  (interaction-setup)
  (load-level "levels/alpha_ugly.json")
  (build-level)

  (start!)
  (println "Started?" (started?))
  (pause!)
  (build-spawn (cell [4 2]) :truck)
  (sketch!))
