(ns jest.visualize.points)

(defmulti length
  (fn [s]
    (:stroke-type (meta s))))

(defmulti tangent
  (fn [s p [d1 d2]]
    (:stroke-type (meta s))))

(defmulti point
  (fn [s p]
    (:stroke-type (meta s))))

(defn stroke
  #^{:doc (str "Returns a 1-arity function that calculates "
               "an n-dimentional point at pro")}
  [from to]
  {:pre [(= (count from) (count to))
         (not= from to)
         ]}
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

(defn start-point [l]
  (point l 0))

(defn end-point [l]
  (point l 1))

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
  ([l _ [d1 d2]]
      (let [p1 (start-point l)
            p2 (l 1)
            dx (- (p2 d1)
                  (p1 d1))
            dy (- (p2 d2)
                  (p1 d2))]
        (Math/atan2 dy
                    dx))))

(defn strokes-connected?
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
    false))

(defn index-sub-strokes
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
  [ss p]
  {:pre [(= (:stroke-type (meta ss)) :composed)]}
  (first
   (if (= 0 p)
     (keep
      (fn [[[start end] sub-stroke]]
        (if (= 0 start)
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
  [ss]
  {:pre [(strokes-connected? ss)]}
  (with-meta
    (fn [p]
      (start-point (first ss))
      (first (keep (fn [[[start end] {offset :offset
                                     p' :progress
                                     stroke :stroke
                                     }]]
                     (if (and
                          (> p start)
                          (<= p end))
                       (stroke (/ (- p offset)
                                  p'))))
                   (index-sub-strokes ss))))
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
