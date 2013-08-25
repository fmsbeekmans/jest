;(set! *warn-on-reflection* true)

(ns user
  (:require [quil.core :as quil])
  (:use clojure.repl
        clojure.pprint
        jest.world.cell
        jest.visualize.resource
        [jest.visualize.visualize :exclude [min-borders]]
        jest.visualize.util
        ;jest.tiled.validation
        jest.world
        jest.world.building
        jest.behavior.movement
        jest.behavior.spawn
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
        jest.world.vehicle
        jest.world.route)
  (:require [jest.input.editor :refer [disable-editor edit]]))

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
  (setup (load-tileset "tileset.json"))
  (start-level-cycle ["tut1" "tut2" "tut3"]))

(defn user-setup []
  (setup-quil-mouse-input)
  (common-setup)
  (sketch!))

(defn user-setup-touch []
  (common-setup)
  (sketch!)
  (undecorate-sketch @world-sketch)
  (ensure-wm-touch-input-setup!))

(defmacro at-pointer-cell [[c] & body]
  `(let [~c (cell (first (vals (all-pointers))))]
     ~@body))

(defn make-mixer-empty [m]
  (dosync
   (alter-cell m assoc :resource nil)))

(defn clear-level
  "empty all buildings and unload all vehicles"
  []
  (dosync
   (doseq [v (all-vehicles)] (unload-vehicle v))
   (doseq [m (all-mixers)] (alter-cell m assoc :resource nil))
   (doseq [d (all-depots)] (alter-cell d assoc :amount 0))))

