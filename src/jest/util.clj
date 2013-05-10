(ns jest.util)

(defn derefable?
  "Returns true if the argument can be dereferenced, false otherwise."
  [x]
  (instance? clojure.lang.IDeref x))

(defn maybe-deref [x]
  (if (derefable? x)
    (deref x)
    x))

