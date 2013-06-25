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
  (build-path (cell [3 4]) :south :road)
)


(defn demo-on-down [id pos]
  (if (spawn? (direction (cell pos) :south))
    (spawn (direction (cell pos) :south))
    (case pos
      [0 0]
      (if (paused?)
        (resume!)
        (pause!))
      nil)))

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
  (set-input-handler! :on-move #(rough-staging-on-move %1 %2 %3))
  (set-input-handler! :on-down #(demo-on-down %1 %2))
  (load-level "levels/alpha_ugly.json")
  (build-level)

  (start!)
  (println "Started?" (started?))
  (pause!)
  (build-spawn (cell [4 2]) :truck)
  (sketch!))
