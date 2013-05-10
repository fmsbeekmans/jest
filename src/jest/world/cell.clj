(ns jest.world.cell
  (:use jest.util))

(defrecord Cell [coord paths background type])

(defonce
  #^{:dynamic true
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

(defn initialize-world [sx sy]
  (reset! world (world-grid sx sy)))

(defn cell
  "Returns the cell on the given coordinates"
  ([x y]
     (@world [x y]))
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
  "returns a list of all cell refs"
  []
  @world)

(defn all-cells-type
  "returns all cells in the currently bound world grid with the given building type"
  [type]
  (map first (filter (fn [[_ cell]]
                       (= (:type @cell) type)) (all-cells))))

(defn set-background
  "sets the background on refcell c to the given background."
  [c background]
  (dosync
   (alter c assoc :background background)))
