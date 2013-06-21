(ns jest.visualize.points
  "Helps to define routes through n-dimentional spaces and
   get the needed. Defined are simple and composite.")


;; Multi methods for strokes.

(defmulti length
  "Return the length of a stroke based on it's type."
  (fn [s]
    (:stroke-type (meta s))))

(defmulti tangent
  "Return the tangent of a stroke at progress p from dimensions d1 to d2."
  (fn [s _ [_ _]]
    (:stroke-type (meta s))))

(defmulti point
  "Return a point at a certain progress in a stroke."
  (fn [s _]
    (:stroke-type (meta s))))

;; Helper functions

(defn start-point
  "Short hand for the point at the start."
  [l]
  (point l 0))

(defn end-point
  "Short hand for point at the end."
  [l]
  (point l 1))

;; Simple stroke

(defn stroke
  "Returns a linear stroke between the n-dimentional points from and to."
  [from to]
  (with-meta
    (fn [progress]
      {:pre [(>= progress 0)
             (<= progress 1)]}
      (let [togo (- 1 progress)]
        (vec (map (fn [from-c to-c]
                    (+
                     (* progress to-c)
                     (* togo from-c)))
                  from
                  to))))
    {:stroke-type :simple}))

(defmethod length
  :simple
  [l]
  (Math/sqrt
   (apply +
          (map (fn [p1 p2]
                 (let [d (- p2 p1)]
                   (* d d)))
               (l 0)
               (l 1)))))

(defmethod point
  :simple
  [s p]
  (s p))

(defmethod tangent
  :simple
  [l _ [d1 d2]]
  (let [p1 (start-point l)
        p2 (l 1)
        dx (- (p2 d1)
              (p1 d1))
        dy (- (p2 d2)
              (p1 d2))]
    (Math/atan2 dy
                dx)))

;; Composed

(defn strokes-connected?
  "Does each stroke start at the end of the stroke before it?"
  [ss]
  (if (seq ss)
    (loop [stroke (first ss)
           last-p (start-point stroke)
           ss' ss]
      (if (seq ss')
        (if (= (set (map (fn [last first]
                           (= (double first)
                              (double last)))
                         last-p (start-point stroke)))
               #{true})
          (recur (second ss')
                 (end-point stroke)
                 (rest ss'))
          false)
        true))
    true))

(defn index-sub-strokes
  "Make a map of a vector of strokes, where the key is a relative progress
interval and as value a map of offset, howmuch stroke comes before this
starts in absolute? progress, relative, how long is this sub-stroke? and the
stroke itself."
  [ss]
  {:pre [(strokes-connected? ss)]}
  (let [total-length (apply + (map length ss))]
    (loop [sum 0
           ss' ss
           m {}]
      (if (seq ss')
        (let [sub-stroke (first ss')
              length' (/ (length sub-stroke) total-length)
              sum' (+ sum length')]
          (recur
           sum'
           (rest ss')
           (assoc m [sum sum']
                  {:offset sum
                   :progress length'
                   :stroke sub-stroke})))
        m))))

(defn sub-stroke
  "At which sub-stroke is p?"
  [ss p]
  {:pre [(= (:stroke-type (meta ss)) :composed)]}
  (first
   (if (zero? p)
     (keep
      (fn [[[start end] sub-stroke]]
        (if (zero? start)
          sub-stroke))
      (:indexed-sub-strokes (meta ss)))
     (keep
      (fn [[[start end] sub-stroke]]
        (if (and
             (> p start)
             (<= p end))
          sub-stroke))
      (:indexed-sub-strokes (meta ss))))))

(defn stroke-comp
  "Compose strokes in order, return a front-end that let's them behave as one."
  [ss]
  {:pre [(strokes-connected? ss)]}
  (with-meta
    ss
    {:stroke-type :composed
     :indexed-sub-strokes (index-sub-strokes ss)}))

(defmethod point :composed
  [s p]
  (let [{offset :offset
         p' :progress
         sub-stroke :stroke} (sub-stroke s p)]
    (sub-stroke (/ (- p offset)
                   p'))))

(defmethod length :composed
  [ss]
  (apply + (map length
                (map :stroke (vals (:indexed-sub-strokes (meta ss)))))))

(defmethod tangent :composed
  [ss p [d1 d2]]
  (let [{offset :offset
         p' :progress
         sub-stroke :stroke} (sub-stroke ss p)]
    (tangent sub-stroke
             (/ (- p offset)
                p')
             [d1 d2])))
