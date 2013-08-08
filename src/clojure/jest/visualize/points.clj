(ns jest.visualize.points
  "Helps to define routes through n-dimentional spaces and
   get the needed. Defined are simple and composite."
  (:require [clojure.algo.generic.math-functions :refer [sin cos]]))

;; Helper functions

(defn start-point
  "Short hand for the point at the start."
  [l]
  (.point l 0))

(defn end-point
  "Short hand for point at the end."
  [l]
  (.point l 1))

(defn progress
  [p]
  (cond
   (> 0 p) 0
   (< 1 p) 1
   :default p))

;; Stroke protocol

(defprotocol Stroke
  (length [this])
  (point [this p])
  (tangent [this p]))

;; Linear stroke

(defrecord Linear [start end]
  Stroke
  (length [this]
    (Math/sqrt
     (apply +
            (map (fn [p1 p2]
                   (let [d (- p2 p1)]
                     (* d d)))
                 (:start this)
                 (:end this)))))
  (point [this p]
    (let [p' (progress p)
          togo (- 1 p')]
      (vec (map (fn [from-c to-c]
                  (+
                   (* p' to-c)
                   (* togo from-c)))
                (:start this)
                (:end this)))))
  (tangent [this p]
    (let [p1 (start-point this)
          p2 (end-point this)
          dx (- (p2 0)
                (p1 0))
          dy (- (p2 1)
                (p1 1))]
      (Math/atan2 dy
                  dx))))

(defn index-sub-strokes
  "Make a map of a vector of strokes, where the key is a relative progress
interval and as value a map of offset, howmuch stroke comes before this
starts in absolute? progress, relative, how long is this sub-stroke? and the
stroke itself."
  [ss]
  (let [total-length (apply + (map length ss))]
    (loop [sum 0
           ss' ss
           m {}]
      (if (seq ss')
        (let [sub-stroke (first ss')
              length' (/ (length sub-stroke) total-length)
              sum' (if (empty? (rest ss'))
                       1
                       (+ sum length'))]
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
  [composed p]
;  {:pre [(= (:stroke-type (meta composed)) :composed)]}
  (let [p' (progress p)]
    (first
     (cond
      (zero? p') (keep
                 (fn [[[start end] sub-stroke]]
                   (if (zero? start)
                     sub-stroke))
                 (:indexed-sub-strokes (meta composed)))
      (zero? (- 1 p')) (keep
                        (fn [[[start end] sub-stroke]]
                          (if (zero? (- 1 end))
                            sub-stroke))
                        (:indexed-sub-strokes (meta composed)))
      :default (keep
                (fn [[[start end] sub-stroke]]
                  (if (and
                       (> p' start)
                       (<= p' end))
                    sub-stroke))
                (:indexed-sub-strokes (meta composed)))))))

(defrecord ComposedStroke [sub]
  Stroke
  (length [this]
    (apply + (map (comp length :stroke)
                  (vals (:indexed-sub-strokes (meta this))))))
  (point [this p]
    (let [{offset :offset
           p' :progress
           sub-stroke :stroke} (sub-stroke this (progress p))
           p'' (if (zero? p')
                 0
                 (progress (/ (- (progress p) offset)
                              (progress p'))))]
      (.point sub-stroke (cond
                          (> p'' 1) 1
                          (< p'' 0) 0
                          :default p''))))
  (tangent [this p]
    (let [{offset :offset
           p' :progress
           sub-stroke :stroke} (sub-stroke this (progress p))]
      (tangent sub-stroke
               (/ (- (progress p) offset)
                  (progress p'))))))

(defn ->ComposedStroke [sub]
  (with-meta (ComposedStroke. sub)
    {:indexed-sub-strokes (index-sub-strokes sub)}))
