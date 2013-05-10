(ns jest.world.cell
  "Functions for managing the world grid."
  (:use jest.util))

(defrecord Cell [coord paths background type])

(defonce
  #^{:dynamic true
     :private true
     :doc "Binding containing the world state as an atom containing a map from [x y] coordinates to cells."} world
  (atom {}))

(defn- world-grid
  "generates and returns a world grid of the given dimension."
  [sx sy]
  (reduce conj {}
          (for [x (range sx)
                y (range sy)]
            [[x y] (ref (map->Cell {:coord [x y]
                                    :paths {}
                                    :type :normal
                                    :background :none}))])))

(defn initialize-world
  "Initializes the world grid with the specified dimensions."
  [sx sy]
  (reset! world (world-grid sx sy)))

(defn cell
  "Returns the cell on the given coordinates"
  ([x y]
     @(@world [x y]))
  ([[x y]]
     (cell x y)))

(defn coords
  "Returns the coordinates of the given cell/ref"
  [c]
  (:coord (maybe-deref c)))

(def directions
  {:north [0 -1]
   :south [0 1]
   :west [-1 0]
   :east [1 0]})

(defn direction
  "returns cell in the given direction"
  [c dir]
  (cell (map +
             (coords (maybe-deref c))
             (dir directions))))

(defn all-cells
  "returns a list of all cells, optionally filtered by a predicate"
  ([]
     (map (comp deref second) @world))
  ([pred]
     (filter pred (all-cells))))

(defn alter-cell
  "Alters a cell. This must be called within a transaction."
  ([c f & args]
     (apply alter (@world (coords c)) f args)))
     

(defn set-background
  "Alters world state by setting the background on cell c to the given background."
  [c background]
  (dosync
   (alter-cell c assoc :background background)))
   
