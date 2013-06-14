(ns jest.color)

(defn hue->vector [h]
  [(Math/cos h)
   (Math/sin h)])

(defn vector->hue [[x y]]
  (let [atan (Math/atan2 y x)]
    (if (>= atan 0)
      atan
      (+ atan (* 2 Math/PI)))))
	   
(defn average-hue [& hs]
  (vector->hue (apply map +
                      (map hue->vector hs))))

(defmulti hue type)

(defmethod hue Number [deg]
  (Math/toRadians deg))

(def colors
  {:red (hue 0)
   :yellow (hue 60)
   :green (hue 120)
   :blue (hue 240)}) 

(defmethod hue clojure.lang.Keyword [color-name]
  (colors color-name))

(defn hue-difference [h1 h2]
  (Math/abs (- h1 h2)))

(def +delta+ (/ Math/PI 8))

(defn <=delta? [dh]
  (<= dh +delta+))

(defn hue-matches? [h1 h2]
  (<=delta? (hue-difference h1 h2)))

(defn contains-hue? [coll h]
  (boolean (seq (filter (partial hue-matches? h)
                        coll))))
