(ns jest.world.cell
  "Functions for managing the world grid."
  (:require [jest.world :as world]))

(defrecord Cell [coord paths background building-type
                 vehicle-type resource-type vehicles])

(defn- world-grid
  "Generates and returns a world grid of the given dimension."
  [sx sy]
  (reduce conj {}
          (for [x (range sx)
                y (range sy)]
            [[x y] (ref (map->Cell {:coord [x y]
                                    :paths {}
                                    :vehicles #{}}))])))

(defn initialize-world
  "Initializes the world grid with the specified dimensions."
  [sx sy]
  (world/reset-world (world-grid sx sy)))

(defn set-background
  "Alters world state by setting the background on cell c to the given
  background."
  [c background]
  (dosync
   (world/alter-cell c assoc :background background)))

(defmacro with-initialized-temp-world
  "For testing purposes, rebind the world state to an initialized map."
  [[sx sy] & body]
  `(world/with-temp-world
     (initialize-world ~sx ~sy)
     ~@body))
