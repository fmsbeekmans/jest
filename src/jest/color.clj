(ns jest.color)

(defn hue-to-radian [h]
	(/ (* (mod h 256) 2 Math/PI)
	   256))

(defn radian-to-vector [r]
  [(Math/cos r)
   (Math/sin r)])

(def hue-to-vector (comp radian-to-vector hue-to-radian))

(defn vector-to-radian [[x y]]
  (let [atan (Math/atan2 y x)]
    (if (>= atan 0)
      atan
      (+ atan (* 2 Math/PI)))))
	   
(defn radian-to-hue [r]
  (mod  (Math/round  (* 256
                        (/ r (* 2 Math/PI))))
        256))

(def vector-to-hue (comp radian-to-hue vector-to-radian))


(defn average-hue [h1 h2]
  (vector-to-hue (map +
                      (hue-to-vector h1)
                      (hue-to-vector h2))))
