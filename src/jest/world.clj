(ns jest.world)

(defrecord Cell [coord paths background type])
(defrecord Path [type direction inout resources])

(defonce
  #^{:dynamic true
     :doc "Binding containing the world state as an atom containing a map from [x y] coordinates to cells."} world
  (atom {}))

(defn world-grid
  "generates and returns a world grid of the given dimension."
  [sx sy]
  (reduce conj {}
          (for [x (range sx)
                y (range sy)]
            [[x y] (ref (map->Cell {:coord [x y]
                                    :paths {}
                                    :type :normal
                                    :background :none}))])))

(defn cell
  "Returns the cell on the given coordinates"
  [x y]
  (@world [x y]))

(defn derefable?
  "Returns true if the argument can be dereferenced, false otherwise."
  [x]
  (instance? clojure.lang.IDeref x))

(defn maybe-deref [x]
  (if (derefable? x)
    (deref x)
    x))

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
  (@world (map +
               (coords (maybe-deref c))
               (dir directions))))

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

(defn- add-building
  "adds a building to the given cell"
  [c type]
  {:pre [(= (:type c) :normal)]}
  (assoc c :type type))

(defn- remove-building
  "removes a building from the given cell"
  ([c]
     {:pre [(not= (:type c) :normal)]}
     (assoc c :type :normal))
  ([c type]
     {:pre [(= (:type c) type)]}
     (remove-building c)))

(defn- all-cells-type
  "returns all cells in the currently bound world grid with the given building type"
  [type]
  (map first (filter (fn [[_ cell]]
                       (= (:type @cell) type)) @world)))

(defmacro defbuilding [name]
  (let [add (symbol (str "add-" name))
        remove (symbol (str "remove-" name))
        build (symbol (str "build-" name))
        unbuild (symbol (str "unbuild-" name))
        pred (symbol (str name "?"))
        all (symbol (str "all-" name "s"))]
    `(do (defn- ~add
           ~(format "adds a %s to the given cell" name)
           [~'c]
           (add-building ~'c ~(keyword name)))
         (defn- ~remove
           ~(format "removes a %s from the given cell" name)
           [~'c]
           (remove-building ~'c ~(keyword name)))
         
         (defn- ~pred
           ~(format "returns whether or not this cell/refcell is of type %s" name)
           ([~'c]
              (= (:type (maybe-deref ~'c)) ~(keyword name)))
           ([~'x ~'y]
              (~pred (cell ~'x ~'y))))
         
         (defn- ~all
           ~(format "returns all cells with building type %s" name)
           []
           (all-cells-type ~(keyword name)))
         
         (defn ~build
           ~(format "builds a %s to the given cell ref" name)
           ([~'c]
              (dosync (alter ~'c ~add)))
           ([~'x ~'y]
              (dosync (alter (cell ~'x ~'y) ~add))))
         
         (defn ~unbuild
           ~(format "unbuilds a %s from the given cell ref" name)
           ([~'c]
              (dosync (alter ~'c ~remove)))
           ([~'x ~'y]
              (~unbuild (cell ~'x ~'y)))))))

(defmacro buildings [& names]
  `(do ~@(map #(list 'defbuilding %) names)))

(buildings spawn
           supply
           depot
           mixer)

(defn set-background
  "sets the background on refcell c to the given background."
  [c background]
  (dosync
   (alter c assoc :background background)))
