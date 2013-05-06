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
                                    :roads {}
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
  `(do ~@(map direction-m dirs)))

(def-directions
  [north 0 -1]
  [south 0 1]
  [west -1 0]
  [east 1 0])

(defn add-path
  "adds the given path to the cell"
  [c type in outs]
  (update-in c [:paths]
             (fn [paths]
               (apply merge
                      (assoc paths in (map->Path {:type type
                                                  :direction in
                                                  :inout :in}))
                      (map #(assoc paths % (map->Path {:type type
                                                       :direction %
                                                       :inout :out}))
                           outs)))))

;(defn remove-path
;  "removes the given path from the cell"

(defn in-paths
  "Returns all incoming paths for the given cell"
  [c]
  (for [[dir {:keys [inout type]}] (:paths c)
        :when (= :in inout)]
    [dir type]))
       
(defn out-paths
  "Returns all outgoing paths for the given cell"
  [c]
  (for [[dir {:keys [inout type]}] (:paths c)
        :when (= :out inout)]
    [dir type]))
       

(defn complete-paths
  "Returns all in-out path pairs of this cell in the format [type [in out]]"
  [c]
  (for [[indir intype] (in-paths c)
        [outdir outtype] (out-paths c)
        :when (= intype outtype)]
    [intype [indir outdir]]))
