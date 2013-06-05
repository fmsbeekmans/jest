(ns jest.util
  "Miscellaneous utility functions.")

(defn derefable?
  "Returns true if the argument can be dereferenced, false otherwise."
  [x]
  (instance? clojure.lang.IDeref x))

(defn maybe-deref
  "Returns the derefed x if x is a derefable. if x is not a derefable, it is returned directly."
  [x]
  (if (derefable? x)
    (deref x)
    x))

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
  "Returns a function which is a composition of n f functions."
  [f n]
  (apply comp (repeat n f)))

(defn offset-vec
  "Creates a map from (index + offset) to value"
  [v offset]
  (zipmap (map (ncomp inc offset) (range)) v))

(defn offset-map
  "Creates a map from (key + offset) to value. Key should be
  representable as an int"
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
