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
  (apply comp (repeat f n)))
