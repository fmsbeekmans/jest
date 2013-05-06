(ns jest.world)

(defrecord Cell [x y paths])
(defrecord Path [type direction inout resources])
(defn- path-type [type]
  `(do (defrecord ~type ~'[direction inout resource])
       (derive ~type ::path)))
  

(defmacro def-path-types [& types]
  `(do ~@(map path-type types)))

(def-path-types Road Rails Canal)

(defrecord Path [type in out])

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

(defn- direction-m [[dir dx dy]]
  `(defn ~dir
     ~(format "Returns the cell in direction %s from the given cell" dir)
     ~'[c]
     (if (derefable? ~'c)
       (recur (deref ~'c))
       (apply cell (map +
                        (coords ~'c)
                        ~[dx dy])))))

(defmacro def-directions [& dirs]
  `(do ~@(map direction-m dirs)
       (def dirfn ~(reduce conj {}
                           (map (fn [[dir _ _]]
                                  [(keyword dir) dir])
                                dirs)))))

(def-directions
  [north 0 -1]
  [south 0 1]
  [west -1 0]
  [east 1 0])

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
  [c direction type]
  {:pre [((dirfn direction) c)]}
  (dosync
   (alter c add-path direction type :out)
   (alter ((dirfn direction) c) add-path (opposite-dirs direction) type :in)))


(defn unbuild-path
  "builds a path from refcell c to the given direction"
  [c direction]
  {:pre [((dirfn direction) c)]}
  (dosync
   (alter c remove-path direction)
   (alter ((dirfn direction) c) remove-path (opposite-dirs direction))))


(defn moo [x]
  {:pre [(>= x 0)]
   :post [(>= % 0)]}
  (dec x))
