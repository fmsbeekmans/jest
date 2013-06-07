(ns jest.visualize.points)

(defn stroke
  #^{:doc (str "Returns a 1-arity function that calculates "
               "an n-dimentional point at pro")}
  [from to]
  {:pre [(= (count from) (count to))]}
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
  (l 0))

(defn end-point [l]
  (l 1))

(defmulti length
  (fn [l]
    (:stroke-type (meta l))))

(defmulti tangent
  (fn [l p [d1 d2]]
    (:stroke-type (meta l))))

(defn strokes-connected?
  [ss]
  (if (seq ss)
    (loop [stroke (first ss)
           last-p (start-point stroke)
           ss' ss]
      (if  (seq ss')
        (if (= last-p (start-point stroke))
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

(defn stroke-comp
  [ss]
  {:pre [(strokes-connected? ss)]}
  (with-meta
    (fn [p]
      (if (= p 0)
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
                     (index-sub-strokes ss)))))
    {:stroke-type :composed
     :sub-strokes (index-sub-strokes ss)}))

(defmethod length :simple
  [l]
  (Math/sqrt
   (apply +
          (map (fn [p1 p2]
                 (let [d (- p2 p1)]
                   (* d d)))
               (l 0)
               (l 1)))))

(defmethod tangent
  :simple
  ([l _ [d1 d2]]
      (let [p1 (start-point l)
            p2 (l 1)
            dx (- (p2 d1)
                  (p1 d1))
            dy (- (p2 d2)
                  (p1 d2))]
        (if (= dx 0)
          (* Math/PI (if (> dy 0)
                       0.5
                       1.5))
          (Math/atan (/ dy
                        dx))))))
