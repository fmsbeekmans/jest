(ns jest.path)

(defonce ^:dynamic paths (atom {}))


(defn- add-connection
  ([paths [x1 y1] [x2 y2]]
     {:pre [(or (= x1 x2)
                (= y1 y2))]}
     (assoc paths
       [x1 y1]
       (conj (set (paths [x1 y1]))
             [x2 y2])
       [x2 y2]
       (conj (set (paths [x2 y2]))
             [x1 y1])))

  ([paths [x1 y1] [x2 y2] & points]
     (reduce #(add-connection %1 [x1 y1] %2)
             paths
             (conj points [x2 y2]))))

(defn- on-path? [[px py] [sx sy] [ex ey]]
  (or (= px sx ex)
      (= py sy ey)))

(defn- paths-for [paths [x y]]
  (map (partial hash-set [x y])
       (paths [x y])))

(defn- all-paths [paths]
  (distinct (mapcat (fn [[p _]]
                 (paths-for paths p))
               paths)))

(defn- paths-with [paths [x y]]
  (filter (partial apply on-path? [x y])
          (all-paths paths)))
                   
