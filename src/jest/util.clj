(ns jest.util
  "Miscellaneous utility functions.")

(defn derefable?
  "Returns true if d can be dereferenced, false otherwise."
  [d]
  (instance? clojure.lang.IDeref d))

(defn maybe-deref
  "Returns the value referred to by d, or d itself if d is not derefable"
  [d]
  (if (derefable? d)
    (deref d)
    d))

(defn plural
  "Returns a best guess of the plural of the given word"
  [word]
  (cond
   (= \y (last word))
    (format "%sies" (subs word 0 (dec (count word))))
   (= \s (last word))
    word
   :else
    (format "%ss" word)))

(defn ncomp
  "Returns a function which is the composition of n f functions."
  [f n]
  (apply comp (repeat n f)))

(defn offset-vec
  "Creates a map from (index + offset) to values of v"
  [v offset]
  (zipmap (map (ncomp inc offset) (range)) v))

(defn offset-map
  "Creates a map from (key + offset) to values om m. All keys should
  be representable as an int"
  [m offset]
  (let [offset-f (ncomp inc offset)
        keyword->int #(Integer/valueOf (name %1))]
    (zipmap (map (comp offset-f keyword->int)
                 (keys m))
            (map keyword (vals m)))))

(defn two-step-map
  "Returns a two-step map, with keys from m1 mapping to values in m2"
  [m1 m2]
  (into {}
        (for [ [k v] m1
               :when (contains? m2 v)]
          [k (m2 v)])))

(defn remap-maps
  [m1 m2]
  (into {}
        (for [ [k v] m1
               :when (contains? m2 k)]
          [(m2 k) v])))

(defn hyphenate-keywords
  "Return a keyword from keywords interposed by hyphens."
  [& ks]
  {:pre [(every? identity (map keyword? ks))]}
  (keyword (apply str (interpose "-" (map name ks)))))

(defn group-seq
  "Partition a seq into groups."
  [s group-ps]
  (into {}
        (map (fn [[group p]]
               [group (filter p s)]) group-ps)))

