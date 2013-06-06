(ns jest.world.cell
  "Functions for managing the world grid."
  (:use jest.util))

(defrecord Cell [coord paths background building-type vehicle-type resource-type vehicles])

(defonce
  #^{:dynamic true
     :private true
     :doc "Binding containing the world state as an atom containing a map from [x y] coordinates to cells."}
  *world*
  (atom {}))

(defn- world-grid
  "generates and returns a world grid of the given dimension."
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
  (reset! *world* (world-grid sx sy)))

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

(defmacro with-initialized-temp-world
  "For testing purposes, rebind the world state to an initialized map."
  [[sx sy] & body]
  `(binding [*world* (atom {})]
     (initialize-world ~sx ~sy)
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

(defn set-background
  "Alters world state by setting the background on cell c to the given background."
  [c background]
  (dosync
   (alter-cell c assoc :background background)))
