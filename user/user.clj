;(set! *warn-on-reflection* true)

(ns user
  (:require [quil.core :as quil])
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.visualize.resource
        [jest.visualize.visualize :exclude [min-borders]]
        jest.visualize.util
        jest.tiled.validation
        jest.world
        jest.world.building
        jest.movement
        jest.util
        jest.world.path
        jest.level
        jest.input.highlight
        jest.color
        jest.score
        jest.tileset
        jest.interface

        brick.drawable
        brick.image

        jest.testutils
        jest.scheduler

        jest.input.quil
        jest.input.wm-touch
        jest.input.core
        jest.input.interaction
        jest.vehicle
        jest.world.route)
  (:require [jest.input.editor :refer [disable-editor edit]]))

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

  (build-depot (cell [4 3]) :red 30)
  (build-depot (cell [4 4]) :blue 30)

  (build-spawn (cell [5 2]) :truck))

(def levels
  {:tutorial
   [(fn level1 []
      (initialize-world 21 17)
      (enable-spawner (build-spawn (cell [3 6]) :truck) 0 1000)
      (dotimes [i 4]
        (build-path (cell [4 (+ 6 i)]) :south :road))
      (dotimes [i 3]
        (build-path (cell [(- 4 i) 10]) :west :road))
      (dotimes [i 4]
        (build-path (cell [1 (- 10 i)]) :north :road))
      (dotimes [i 3]
        (build-path (cell [(+ 1 i) 6]) :east :road))
      (build-path (cell [14 8]) :east :road)
      (build-spawn (cell [15 8]) :truck)
      (build-path (cell [6 8]) :east :road)
      (build-supply (cell [7 8]) :red)
      (build-path (cell [7 8]) :east :road)
      (build-path (cell [10 8]) :east :road)
      (build-depot (cell [11 8]) :red 10)
      (build-path (cell [11 8]) :east :road)
      )
    (fn level2 []
      (initialize-world 21 17)
      (enable-spawner (build-spawn (cell [3 6]) :truck) 0 4000)
      (enable-spawner (build-spawn (cell [3 10]) :truck) 2000 4000)
      (dotimes [i 4]
        (build-path (cell [(+ 3 i) 6]) :east :road)
        (build-path (cell [(+ 3 i) 10]) :east :road))
      (dotimes [i 4]
        (build-path (cell [(+ 10 i) 6]) :east :road)
        (build-path (cell [(+ 10 i) 10]) :east :road))

      (dotimes [i 2]
        (build-path (cell [7 (+ 6 i)]) :south :road)
        (build-path (cell [7 (- 10 i)]) :north :road)
        (build-path (cell [10 (+ 8 i)]) :south :road)
        (build-path (cell [10 (- 8 i)]) :north :road)
        )
      (dotimes [i 3]
        (build-path (cell [(+ 7 i) 8]) :east :road))

      (build-supply (cell [4 6]) :red)
      (build-supply (cell [4 10]) :blue)
      (build-depot (cell [13 6]) :blue 10)
      (build-depot (cell [13 10]) :red 10)

      (build-spawn (cell [14 6]) :truck)
      (build-spawn (cell [14 10]) :truck)
      )
    (fn level3 []
      (initialize-world 21 17)

      (jest.world.building/enable-spawner (build-spawn (jest.world/cell [4 4]) :truck) 500 4000)
      (jest.world.building/enable-spawner (build-spawn (jest.world/cell [4 12]) :truck) 1500 4000)
      (jest.world.building/enable-spawner (build-spawn (jest.world/cell [10 6]) :truck) 1000 4000)
      (jest.world.building/enable-spawner (build-spawn (jest.world/cell [11 7]) :truck) 0 4000)
      (build-spawn (jest.world/cell [12 9]) :truck)
      (build-supply (jest.world/cell [8 7]) :red)
      (build-supply (jest.world/cell [8 9]) :green)
      (build-mixer (jest.world/cell [10 8]))
      (build-depot (jest.world/cell [11 9]) :yellow 10)
      (jest.world.path/build-path (jest.world/cell [4 3]) :north :road)
      (jest.world.path/build-path (jest.world/cell [6 5]) :west :road)
      (jest.world.path/build-path (jest.world/cell [8 7]) :south :road)
      (jest.world.path/build-path (jest.world/cell [9 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [10 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [4 4]) :north :road)
      (jest.world.path/build-path (jest.world/cell [5 5]) :west :road)
      (jest.world.path/build-path (jest.world/cell [4 5]) :north :road)
      (jest.world.path/build-path (jest.world/cell [8 9]) :north :road)
      (jest.world.path/build-path (jest.world/cell [8 11]) :west :road)
      (jest.world.path/build-path (jest.world/cell [7 11]) :west :road)
      (jest.world.path/build-path (jest.world/cell [8 12]) :north :road)
      (jest.world.path/build-path (jest.world/cell [6 11]) :west :road)
      (jest.world.path/build-path (jest.world/cell [8 13]) :north :road)
      (jest.world.path/build-path (jest.world/cell [5 11]) :west :road)
      (jest.world.path/build-path (jest.world/cell [8 14]) :north :road)
      (jest.world.path/build-path (jest.world/cell [4 11]) :south :road)
      (jest.world.path/build-path (jest.world/cell [7 14]) :east :road)
      (jest.world.path/build-path (jest.world/cell [4 12]) :south :road)
      (jest.world.path/build-path (jest.world/cell [6 14]) :east :road)
      (jest.world.path/build-path (jest.world/cell [4 13]) :south :road)
      (jest.world.path/build-path (jest.world/cell [5 14]) :east :road)
      (jest.world.path/build-path (jest.world/cell [4 14]) :east :road)
      (jest.world.path/build-path (jest.world/cell [12 5]) :west :road)
      (jest.world.path/build-path (jest.world/cell [8 2]) :south :road)
      (jest.world.path/build-path (jest.world/cell [11 5]) :west :road)
      (jest.world.path/build-path (jest.world/cell [12 6]) :north :road)
      (jest.world.path/build-path (jest.world/cell [7 2]) :east :road)
      (jest.world.path/build-path (jest.world/cell [8 3]) :south :road)
      (jest.world.path/build-path (jest.world/cell [10 5]) :south :road)
      (jest.world.path/build-path (jest.world/cell [12 7]) :west :road)
      (jest.world.path/build-path (jest.world/cell [12 7]) :north :road)
      (jest.world.path/build-path (jest.world/cell [6 2]) :east :road)
      (jest.world.path/build-path (jest.world/cell [8 4]) :south :road)
      (jest.world.path/build-path (jest.world/cell [10 6]) :south :road)
      (jest.world.path/build-path (jest.world/cell [11 7]) :west :road)
      (jest.world.path/build-path (jest.world/cell [12 8]) :north :road)
      (jest.world.path/build-path (jest.world/cell [5 2]) :east :road)
      (jest.world.path/build-path (jest.world/cell [8 5]) :west :road)
      (jest.world.path/build-path (jest.world/cell [10 7]) :south :road)
      (jest.world.path/build-path (jest.world/cell [12 9]) :north :road)
      (jest.world.path/build-path (jest.world/cell [4 2]) :east :road)
      (jest.world.path/build-path (jest.world/cell [7 5]) :west :road)
      (jest.world.path/build-path (jest.world/cell [10 8]) :south :road)
      (jest.world.path/build-path (jest.world/cell [11 9]) :east :road))
    (fn level4 []
      (initialize-world 21 17)

      (jest.world.building/enable-spawner (build-spawn (jest.world/cell [4 7]) :truck) 0 4000)
      (jest.world.building/enable-spawner (build-spawn (jest.world/cell [4 9]) :truck) 0 4000)
      (build-spawn (jest.world/cell [17 8]) :truck)
      (build-supply (jest.world/cell [7 7]) :purple)
      (build-supply (jest.world/cell [7 9]) :purple)
      (build-depot (jest.world/cell [14 8]) :purple 10)
      (jest.world.path/build-path (jest.world/cell [8 7]) :east :road)
      (jest.world.path/build-path (jest.world/cell [10 9]) :north :road)
      (jest.world.path/build-path (jest.world/cell [7 7]) :east :road)
      (jest.world.path/build-path (jest.world/cell [9 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [6 7]) :east :road)
      (jest.world.path/build-path (jest.world/cell [8 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [5 7]) :east :road)
      (jest.world.path/build-path (jest.world/cell [7 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [4 7]) :east :road)
      (jest.world.path/build-path (jest.world/cell [6 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [5 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [4 9]) :east :road)
      (jest.world.path/build-path (jest.world/cell [16 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [15 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [14 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [13 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [12 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [10 7]) :south :road)
      (jest.world.path/build-path (jest.world/cell [11 8]) :east :road)
      (jest.world.path/build-path (jest.world/cell [9 7]) :east :road)
      (jest.world.path/build-path (jest.world/cell [10 8]) :east :road))]})

(defn build-another-level []
  (initialize-world 8 8)
  (enable-spawner (build-spawn (cell [1 1]) :truck) 0 3000)
  (enable-spawner (build-spawn (cell [2 1]) :truck) 1000 3000)
  (enable-spawner (build-spawn (cell [3 1]) :truck) 2000 3000)
  (build-supply (cell [1 2]) :red)
  (build-supply (cell [2 2]) :green)
  (build-mixer (cell [3 3]))
  (build-spawn (cell [4 3]) :truck)
  (build-depot (cell [3 4]) :yellow 30)
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

(defn graceful-exit []
  (if-let [world-bricklet @world-bricklet]
    (let [queue (:command-queue world-bricklet)]
      (swap! queue conj (fn [_] (quil/exit)) ))))

(defn common-setup []
  (graceful-exit)
  (initialize-world 0 0)
  (scheduler-reset!)
  (reset-score)
  (interaction-setup)
  ;(load-level "levels/alpha_ugly.json")
  (setup (load-tileset "tileset.json"))
  (scheduler-reset!)
  ((get-in levels [:tutorial 0]))

  (build-spawn (cell [4 2]) :truck)
  (start!)
  (start-spawning)
  (pause!))

(defn user-setup []
  (setup-quil-mouse-input)
  (common-setup)
  (sketch!)
)

(defn user-setup-touch []
  (common-setup)
  (sketch!)
  (undecorate-sketch @world-sketch)
  (ensure-wm-touch-input-setup!))

(defn path-definition-for [p]
  `(build-path (cell ~(:coords p)) ~(:direction p) ~(:type p)))

(defn extract-path-definitions []
  (map path-definition-for (flatten (map out-paths (all-cells)))))

(defn print-path-definitions []
  (doseq [pd (extract-path-definitions)]
    (println pd)))

(defn building-for [c]
  (let [type (building-type c)
        form
        `(~(symbol (str "build-" (subs (str  type) 1))) (cell ~(coords c))
          ~@(case type
              :spawn [(:vehicle-type c)]
              :supply [(:resource-type c)]
              :mixer []
              :depot [(:resource-type c) (:quotum c)]))]
    (if (and (spawn? c)
             (:spawning? c))
      `(enable-spawner ~form ~(:spawn-offset c) ~(:spawn-rate c))
      form)))

(defn extract-building-definitions []
  (map building-for (concat (all-spawns)
                            (all-supplies)
                            (all-mixers)
                            (all-depots))))

(defn print-level []
  (doseq [e (concat (extract-building-definitions)
                    (extract-path-definitions))]
    (println e)))

(defmacro at-pointer-cell [[c] & body]
  `(let [~c (cell (first (vals (all-pointers))))]
     ~@body))

(defn make-mixer-empty [m]
  (dosync
   (alter-cell m assoc :resource nil)))

(defn- level-helper [selector]
    (get-in levels selector))

(def ex-conf
  {:fn-s [{:f pause! :t "Pause"}
           {:f resume! :t "Resume"}
           {:f (level-helper [:tutorial 0]) :t "Level 0"}
           {:f (level-helper [:tutorial 1]) :t "Level 1"}]
   :watches [{:w world-sketch, :t "World Sketch"}
             {:w world-bricklet, :t "World bricklet"}]})

(defn demo-conf []
  {:fn-s (vec (concat [{:f pause! :t "Pause"}
                       {:f resume! :t "Resume"}
                       {:f start-spawning :t "Start spawning"}
                       {:f stop-spawning :t "Stop spawning"}]
                      (map-indexed
                       (fn [i f] {:f f :t (str "Level " i)})
                       (:tutorial levels))))
   :watches (vec (concat (if-let [wb @world-bricklet]
                           [{:w (:command-queue wb) :t "Command-queue" :tf count}])))})

;; usage: (temp-gui (create-control-panel (demo-conf)))
