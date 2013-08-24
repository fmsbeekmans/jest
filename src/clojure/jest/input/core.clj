(ns jest.input.core
  "Transforms device-agnostic input from pixel-based to tile-based."
  (:require [jest.input.highlight :refer [highlight-cell remove-highlighted-cells
                                          get-highlighted-cells]]
            [jest.visualize.visualize :refer [with-tile]]
            [clojure.core.incubator :refer [defmacro-]]))

(def ^:private pointers (atom {}))
(defn pointer
  "Given a pointer id, returns the position of this pointer."
  [id]
  (@pointers id))

(defn all-pointers
  "Returns all pointers."
  []
  (seq @pointers))

(defn reset-pointers!
  "Clears the list of known pointers."
  []
  (reset! pointers {}))

(def ^:private handlers (atom {}))

(defmacro- defhandler
  "Helper macro to define input handler types."
  [type & args]
  (let [fn-name (symbol (str (name type) "-handler"))]
    `(defn- ~fn-name [~@args]
       (let [fn# (~type @handlers)]
         (if fn#
           (fn# ~@args))))))

(defhandler :on-down id p)
(defhandler :on-up id p)
(defhandler :on-move id p1 p2)

(defn set-input-handler!
  "Sets an input handler.
   on needs to be either :on-down, :on-up or :on-move."
  [on f]
  {:pre [(#{:on-down :on-up :on-move} on)]}
  (swap! handlers assoc on f))

(defn reset-input-handlers!
  "Unsets all set input handlers."
  []
  (reset! handlers {}))

;; Interface for hardware backends
(defn receive-down
  "Receive a down event. To be called by a hardware backend.
   id is a unique identifier for this pointer. The hardware backend should use the same id for the same pointer.
   p is a coordinate in pixels."
  [id p]
  (with-tile [t p]
    (swap! pointers assoc id t)
    (highlight-cell id t)
    (on-down-handler id t)))

(defn receive-up [id]
  "Receive an up event. To be called by a hardware backend.
   id is a unique identifier for this pointer that the backend has used before to refer to this pointer."
  (on-up-handler id (@pointers id))
  (remove-highlighted-cells id)
  (swap! pointers dissoc id))

(defn- step [a b]
  (cond (= a b) 0
        (neg? (- b a)) -1
        :default 1))

;;dumbly interpolates by first moving to the right x, then to the right y
(defn- interpolate [t1 [x y]]
  (loop [[px py] t1
         acc []]
    (let [sx (step px x)
          sy (if (zero? sx)
               (step py y)
               0)
          next (map + [px py] [sx sy])]
      (if (= [0 0] [sx sy])
        acc
        (recur next (conj acc next))))))

(defn receive-move
  "Receive an up event. To be called by a hardware backend.
   id is a unique identifier for this pointer that the backend has used before to refer to this pointer.
   p is a coordinate in pixels."
  [id p]
  (with-tile [t p]
    (let [prev (@pointers id)]
      (when-not (or (nil? prev)
                    (= prev t))
        (loop [prev prev
               [t & tnext] (interpolate prev t)]
          (swap! pointers assoc id t)
          (highlight-cell id t)
          (on-move-handler id prev t)
          (if tnext
            (recur t tnext)))))))

