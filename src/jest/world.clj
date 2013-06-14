(ns jest.world
  "Public interface for the world state."
  (:use jest.util))

(defonce
  #^{:dynamic true
     :private true
     :doc "Binding containing the world state as an atom containing a map from [x y] coordinates to cells."}
  *world*
  (atom {}))

(defn reset-world
  "Forcefully sets the world to value v."
  [v]
  (reset! *world* v))

(defn world-width
  "Returns the width of the world."
  []
  (inc
   (apply max (map first (keys @*world*)))))

(defn world-height
  "Returns the height of the world."
  []
  (inc
   (apply max (map second (keys @*world*)))))

(defn world-size
  "Returns the size of the world as [sx sy]"
  []
  [(world-width) (world-height)])

(defmacro with-temp-world
  "For testing purposes, rebind the world state to an empty map."
  [& body]
  `(binding [*world* (atom {})]
     ~@body))

(defn- cell-ref
  "Returns the cell ref for the given coordinate"
  [[x y]]
  (@*world* [x y]))

(defn cell
  "Returns the cell on the given coordinates"
  [[x y]]
  @(cell-ref [x y]))

(defn coords
  "Returns the coordinates of the given cell/ref"
  [c]
  (:coord (maybe-deref c)))

(def directions
  {:north [0 -1]
   :south [0 1]
   :west [-1 0]
   :east [1 0]})

(defn- calculate-coord [cell dir]
  (vec (map + (coords cell) (dir directions))))

(defn direction
  "returns cell in the given direction"
  [c dir]
  (cell (calculate-coord c dir)))

(defn direction-exists?
  "returns whether the cell is connected in the given direction"
  [c dir]
  (boolean (cell-ref (calculate-coord c dir))))

(defn all-cells
  "returns a list of all cells, optionally filtered by a predicate"
  ([]
     (map (comp deref second) @*world*))
  ([pred]
     (filter pred (all-cells))))

(defn alter-cell
  "Alters a cell. This must be called within a transaction."
  ([c f & args]
     (apply alter (@*world* (coords c)) f args)))
