(ns jest.world.path
  (:use jest.util
        jest.world.cell))

(defrecord Path [type direction inout resources])

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

