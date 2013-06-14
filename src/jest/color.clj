(ns jest.color)

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

(defmulti hue
  "Given either an amount of integer degrees or a keyword, return a color."
  type)

(defmethod hue Number [deg]
  (Math/toRadians deg))

(def colors
  ^{:doc "Returns a hue for the given color keyword."}
  {:red (hue 0)
   :yellow (hue 60)
   :green (hue 120)
   :blue (hue 240)})

(defmethod hue clojure.lang.Keyword [color-name]
  (colors color-name))

(defn hue-difference
  "Returns the difference between twho hues."
  [h1 h2]
  (let [diff (Math/abs (- h1 h2))]
    (if (>= diff Math/PI)
      (Math/abs (- (* 2 Math/PI) (- h1) h2))
      diff)))

(def ^:private +delta+ (/ Math/PI 8))

(defn <=delta?
  "Returns true iff the given value is less or equal to the hue delta, which is
   the maximum hue difference where two hues are seen as the same."
  [dh]
  (<= dh +delta+))

(defn hue-matches?
  "Returns true iff the hue difference between h1 and h2 is less than or equal to
   the delta."
  [h1 h2]
  (<=delta? (hue-difference h1 h2)))

(defn contains-hue?
  "Returns true if one of the colors in the collection matches the given."
  [coll h]
  (boolean (seq (filter (partial hue-matches? h)
                        coll))))
