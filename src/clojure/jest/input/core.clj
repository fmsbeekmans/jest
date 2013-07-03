(ns jest.input.core
  "Transforms device-agnostic input from pixel-based to tile-based."
  (:use [jest.visualize.visualize :only [sketch-size
                                         world-bricklet]]
        [jest.input.highlight :only [highlight-cell remove-highlighted-cells
                                     get-highlighted-cells]]
        [jest.world :only [world-size]]))

(def ^:private pointers (atom {}))
(defn pointer [id]
  (@pointers id))

(defn all-pointers []
  (seq @pointers))

(defn reset-pointers! []
  (reset! pointers {}))

(def ^:private handlers (atom {}))

(defmacro defhandler [type & args]
  (let [fn-name (symbol (str (name type) "-handler"))]
    `(defn- ~fn-name [~@args]
       (let [fn# (~type @handlers)]
         (if fn#
           (fn# ~@args))))))

(defhandler :on-down id p)
(defhandler :on-up id p)
(defhandler :on-move id p1 p2)

(defn set-input-handler! [on fn]
  {:pre [(#{:on-down :on-up :on-move} on)]}
  (swap! handlers assoc on fn))

(defn reset-input-handlers! []
  (reset! handlers {}))

(defn tl []
  ((juxt :border-w :border-h)
   @(:target-drawable @world-bricklet)))

(defn br []
  (map (partial - 1)
       ((juxt :border-w :border-h)
        @(:target-drawable @world-bricklet))))

(defn pixel->tile [x y]
  (let [tl (map * (tl) (sketch-size))
        br (map * (br) (sketch-size))
        map-size (map - br tl)
        cell-size (map / map-size (world-size))
        ppos (map - [x y] tl)
        tpos (map (comp int /) ppos cell-size)
        tpos (map max [0 0]
                  (map min tpos (map dec (world-size))))]
    tpos))

(defmacro with-tile [[t c] & body]
  `(let [~t (apply pixel->tile ~c)]
     ~@body))

(defn receive-down [id p]
  (with-tile [t p]
    (swap! pointers assoc id t)
    (highlight-cell id t)
    (on-down-handler id t)))

(defn receive-up [id]
  (on-up-handler id (@pointers id))
  (remove-highlighted-cells id)
  (swap! pointers dissoc id))

(defn step [a b]
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

(defn receive-move [id p]
  (with-tile [t p]
    (let [prev (@pointers id)]
      (when-not (= prev t)
        (loop [prev prev
               [t & tnext] (interpolate prev t)]
          (swap! pointers assoc id t)
          (highlight-cell id t)
          (on-move-handler id prev t)
          (if tnext
            (recur t tnext)))))))
