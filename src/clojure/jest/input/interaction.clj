(ns jest.input.interaction
  (:require [jest.input.core :refer [set-input-handler!]]
            [jest.world :refer [directions direction cell]]
            [jest.world.building :refer [spawn?]]
            [jest.world.path :refer [path in-path? build-path unbuild-path in-paths]]
            [jest.world.route :refer [paths-with-route build-route unbuild-route]]
            [jest.vehicle :refer [vehicles cargo? cargo-color update-vehicle]]
            [jest.movement :refer [spawn preferred-path]]
            [jest.scheduler :refer [paused? resume! pause!]]))

(def ^:private inv-directions (clojure.set/map-invert directions))

(defn on-down [id pos]
  (if (spawn? (direction (cell pos) :south))
    (spawn (direction (cell pos) :south))
    (case pos
      [0 0]
      (if (paused?)
        (resume!)
        (pause!))
      nil)))

(defn- maybe-build-route [c dir]
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

(defn on-move [id pos1 pos2]
  (let [c1 (cell pos1)
        c2 (cell pos2)
        direction (inv-directions (map - pos2 pos1))]
    (if (path c1 direction)
      (if (in-path? (path c1 direction))
        (unbuild-path c1 direction)
        (maybe-build-route c1 direction))

      (if (or (seq (in-paths c1))
              (spawn? c1))
        (build-path c1 direction :road)))
    ))

(defn interaction-setup []
  (set-input-handler! :on-move on-move)
  (set-input-handler! :on-down on-down))
