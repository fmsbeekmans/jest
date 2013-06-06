(ns jest.visualize.points)

(defn line
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
    {:line-type :simple}))

(defn start-point [l]
  (l 0))

(defn end-point [l]
  (l 1))

(defmulti length
  (fn [l]
    (:line-type (meta l))))

(defmulti tangent
  (fn [l]
    (:line-type (meta l))))

(defn lines [ls]
  (let [total-length (apply + (map length ls))
        sub-lines (loop [s 0
                         ls' ls
                         m {}]
                    (if (seq ls')
                      (let [sub-line (first ls')
                            length' (/ (length sub-line) total-length)
                            s' (+ s length')]
                        (println sub-line)
                        (recur
                         s'
                         (rest ls')
                         (assoc m [s s'] [[s length'] sub-line])))
                      m))]
    sub-lines))

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
  ([l]
      (let [p1 (start-point l)
            p2 (l 1)
            dx (- (p2 0)
                  (p1 0))
            dy (- (p2 1)
                  (p1 1))]
        (Math/atan (/ dy
                      dx)))))
