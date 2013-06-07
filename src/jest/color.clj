(ns jest.color)

(defn hue->vector [h]
  [(Math/cos h)
   (Math/sin h)])

(defn vector->hue [[x y]]
  (let [atan (Math/atan2 y x)]
    (if (>= atan 0)
      atan
      (+ atan (* 2 Math/PI)))))
	   
(defn average-hue [h1 h2]
  (vector->hue (map +
                      (hue->vector h1)
                      (hue->vector h2))))

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
