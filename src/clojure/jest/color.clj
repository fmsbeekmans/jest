(ns jest.color
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

(defn hue-snap
  ([h n]
     (let [divisor (/ (* 2 Math/PI) n)]
       (* divisor (int (+ (/ h divisor)
                          0.5)))))
  ([h]
     (hue-snap h 32)))

(defmulti hue
  "Given either an amount of integer degrees or a keyword, return a color."
  type)

(defmethod hue Integer [deg]
  (mod (Math/toRadians deg) (* 2 Math/PI)))

(defmethod hue Long [deg]
  (mod (Math/toRadians deg) (* 2 Math/PI)))

(defmethod hue Double [rad]
  (mod rad (* 2 Math/PI)))

(defmethod hue Float [rad]
  (mod rad (* 2 Math/PI)))

(def colors
  ^{:doc "Returns a hue for the given color keyword."}
  {:red (hue 0)
   :yellow (hue 60)
   :green (hue 120)
   :blue (hue 240)})

(defmethod hue clojure.lang.Keyword [color-name]
  (colors color-name))

(def ^:private +delta+ (/ Math/PI 8))

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
    (<=delta? (util/angle-difference h1 h2))))

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

