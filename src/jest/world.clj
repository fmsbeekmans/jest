(ns jest.world)

(defrecord Cell [x y])

(def ^:dynamic world
  "Binding containing the world state as a map from [x y] coordinates to cells."
  {})

(defn world-grid
  "generates and returns a world grid of the given dimension."
  [sx sy]
  (reduce conj {}
          (for [x (range sx)
                y (range sy)]
            [[x y] (Cell. x y)])))

(defn cell
  "Returns the cell on the given coordinates"
  [x y]
  (world [x y]))

(defn coords
  "Returns the coordinates of the given cell"
  [c]
  [(:x c) (:y c)])

(defn- direction-m [[dir dx dy]]
  `(defn ~dir
     ~(format "Returns the cell in direction %s from the given cell." dir)
     ~'[c]
     (apply cell (map +
                      (coords ~'c)
                      ~[dx dy]))))

(defmacro directions [& dirs]
  `(do ~@(map direction-m dirs)))

(directions [north 0 -1]
            [south 0 1]
            [west -1 0]
            [east 1 0])
