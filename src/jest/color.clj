(ns jest.color)

(defn hue->radian [h]
	(/ (* (mod h 256) 2 Math/PI)
	   256))

(defn radian->vector [r]
  [(Math/cos r)
   (Math/sin r)])

(def hue->vector (comp radian->vector hue->radian))

(defn vector->radian [[x y]]
  (let [atan (Math/atan2 y x)]
    (if (>= atan 0)
      atan
      (+ atan (* 2 Math/PI)))))
	   
(defn radian->hue [r]
  (mod  (Math/round  (* 256
                        (/ r (* 2 Math/PI))))
        256))

(def vector->hue (comp radian->hue vector->radian))


(defn average-hue [h1 h2]
  (vector->hue (map +
                      (hue->vector h1)
                      (hue->vector h2))))
