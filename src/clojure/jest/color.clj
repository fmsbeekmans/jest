(ns jest.color
  "Color transformation functions."
  (:require [jest.util :as util]))

(defn- hue->vector
  "Given a hue (represented as a radian), return a direction vector on the color
   circle"
  [h]
  [(Math/cos h)
   (Math/sin h)])

(defn- vector->hue
  "Given a direction vector on the color circle, return a hue represented as a
   radian."
  [[x y]]
  (let [atan (Math/atan2 y x)]
    (if (>= atan 0)
      atan
      (+ atan (* 2 Math/PI)))))

(defn average-hue
  "Returns the average of the given hues."
  [& hs]
  (vector->hue (apply map +
                      (map hue->vector hs))))

(let [circle-arc (* 2 Math/PI)]
  (defn circle-overflow [angle]
    (mod angle circle-arc)))

(defn circle-delta-wrap [angle-delta]
  (if (> (Math/abs (- angle-delta Math/PI)) Math/PI)
    (- (* 2 Math/PI) angle-delta)
    angle-delta))

(defn circle-divider [pieces]
  (let [piece-arc (/ (* Math/PI 2)
                     pieces)]
    (take pieces (iterate (partial + piece-arc) 0))))

(let [hue-snapper #(comp
                    (util/snapper circle-delta-wrap (circle-divider %))
                    circle-overflow)]
  (defn hue-snap
    "Snaps a hue to its closest approximation. This approximation is a multiple of h/n. n defaults to 32."
    ([h n]
       ((hue-snapper n) h))
    ([h]
       (hue-snap h 32))))

(defmulti hue
  "Given either an amount of integer degrees or a keyword, return a color."
  type)

(defmethod hue Long [deg]
  (hue-snap (mod (Math/toRadians deg) (* 2 Math/PI))))

(defmethod hue Double [rad]
  (hue-snap (mod rad (* 2 Math/PI))))


(def colors
  ^{:doc "Returns a hue for the given color keyword."}
  {:red (hue 0)
   :yellow (hue 60)
   :green (hue 120)
   :blue (hue 240)
   :purple (hue 300)})

(defmethod hue clojure.lang.Keyword [color-name]
  (colors color-name))

(def ^:private +delta+ 0.0001)

(defn <=delta?
  "Returns true iff the given value is less or equal to the hue delta, which is
   the maximum hue difference where two hues are seen as the same."
  [dh]
  (<= dh +delta+))

(defn hue-matches?
  "Returns true iff the hue difference between h1 and h2 is less than or equal
   to the delta."
  [h1 h2]
  (if (or (nil? h1)
          (nil? h2))
    (= h1 h2)
    (<=delta? (Math/abs (- (hue-snap h1)
                           (hue-snap h2))))))

(defn contains-hue?
  "Returns true if one of the colors in the collection matches the given."
  [coll h]
  (boolean (seq (filter (partial hue-matches? h)
                        coll))))

(defn hue->int
  [h]
  (int (mod (* 255 (/ (hue h) (* 2 Math/PI)))
            255)))

(defn hue->hsb
  ([hue]
     (hue->hsb hue [255 0 255]))
  ([hue alt]
     (if hue
       [(hue->int hue) 255 255]
       alt)))
