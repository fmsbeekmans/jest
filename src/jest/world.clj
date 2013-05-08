(ns jest.world)

(defrecord Cell [x y paths])
(defrecord Path [type direction inout resources])

(def ^:dynamic world
  "Binding containing the world state as an atom containing a map from [x y] coordinates to cells."
  (atom {}))

(defn world-grid
  "generates and returns a world grid of the given dimension."
  [sx sy]
  (reduce conj {}
          (for [x (range sx)
                y (range sy)]
            [[x y] (ref (map->Cell {:x x
                                    :y y
                                    :paths {}
                                    :background :none}))])))

(defn cell
  "Returns the cell on the given coordinates"
  [x y]
  (@world [x y]))

(defn coords
  "Returns the coordinates of the given cell"
  [c]
  [(:x c) (:y c)])

(defn derefable?
  "Returns true if the argument can be dereferenced, false otherwise."
  [x]
  (instance? clojure.lang.IDeref x))

(def directions
  {:north [0 -1]
   :south [0 1]
   :west [-1 0]
   :east [1 0]})

(defn direction
  "returns cell in the given direction"
  [c dir]
  (if (derefable? c)
    (direction @c dir)
    (@world (map +
                 (coords c)
                 (dir directions)))))

(letfn [(some-paths [c inout]
  (for [[dir path] (:paths c)
        :when (= inout (:inout path))]
    path))]
  (defn- in-paths
    "Returns all incoming paths for the given cell"
    [c]
    (some-paths c :in))
       
  (defn- out-paths
    "Returns all outgoing paths for the given cell"
    [c]
    (some-paths c :out)))
       
(defn- complete-paths
  "Returns all in-out path pairs of this cell"
  [c]
  (for [in (in-paths c)
        out (out-paths c)
        :when (= (:type in) (:type out))]
    [in out]))

(defn- add-path
  "adds the given path to the cell"
  [c direction type inout]
  {:pre [(not (get-in c [:paths direction]))]}
  (assoc-in c [:paths direction]
            (map->Path {:type type
                        :direction direction
                        :inout inout})))

(defn- remove-path
  "removes the path from the cell for the given direction"
  [c direction]
  {:pre [(get-in c [:paths direction])]}
  (update-in c [:paths]
             #(dissoc % direction)))

(def opposite-dirs
  {:north :south,
   :south :north,
   :west :east,
   :east :west})

(defn build-path
  "builds a path from refcell c to the given direction"
  [c dir type]
  {:pre [(direction c dir)]}
  (dosync
   (alter c add-path dir type :out)
   (alter (direction c dir) add-path (opposite-dirs dir) type :in)))


(defn unbuild-path
  "builds a path from refcell c to the given direction"
  [c dir]
  {:pre [(direction c dir)]}
  (dosync
   (alter c remove-path dir)
   (alter (direction c dir) remove-path (opposite-dirs dir))))
