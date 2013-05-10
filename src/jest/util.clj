(ns jest.util)

(defn derefable?
  "Returns true if the argument can be dereferenced, false otherwise."
  [x]
  (instance? clojure.lang.IDeref x))

(defn maybe-deref
  "returns the derefed x if x is a derefable. if x is not a derefable, it is returned directly."
  [x]
  (if (derefable? x)
    (deref x)
    x))

