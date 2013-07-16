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

;; Stroke protocol

(defprotocol Stroke
  (length [this])
  (point [this p])
  (tangent [this p]))

;; Linear stroke

(defrecord Linear [start end]
  Stroke
  (point [this p]
    (let [togo (- 1 p)]
      (vec (map (fn [from-c to-c]
                  (+
                   (* p to-c)
                   (* togo from-c)))
                (:start this)
                (:end this)))))
  (length [this]
    (Math/sqrt
     (apply +
            (map (fn [p1 p2]
                   (let [d (- p2 p1)]
                     (* d d)))
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
;;  {:pre [(strokes-connected? ss)]}
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
  [composed p]
;  {:pre [(= (:stroke-type (meta composed)) :composed)]}
  (first
   (cond
    (zero? p) (keep
               (fn [[[start end] sub-stroke]]
                 (if (zero? start)
                   sub-stroke))
               (:indexed-sub-strokes (meta composed)))
    (= 1 p) (keep
             (fn [[[start end] sub-stroke]]
               (if (= 1 start)
                 sub-stroke))
             (:indexed-sub-strokes (meta composed)))
    :default (keep
              (fn [[[start end] sub-stroke]]
                (if (and
                     (> p start)
                     (<= p end))
                  sub-stroke))
              (:indexed-sub-strokes (meta composed))))))

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

(defrecord ComposedStroke [sub]
  Stroke
  (point [this p]
    (let [{offset :offset
           p' :progress
           sub-stroke :stroke} (sub-stroke this p)
           p'' (/ (- p offset)
                  p')]
      (.point sub-stroke (cond
                          (> p'' 1) 1
                          (< p'' 0) 0
                          :default p''))))
  (length [this]
    (apply + (map (comp length :stroke)
                  (vals (:indexed-sub-strokes (meta this))))))
  (tangent [this p]
    (let [{offset :offset
           p' :progress
           sub-stroke :stroke} (sub-stroke this p)]
      (tangent sub-stroke
               (/ (- p offset)
                  p')))))

(defn ->ComposedStroke [sub]
  (with-meta (ComposedStroke. sub)
    {:indexed-sub-strokes (index-sub-strokes sub)}))

(defn angle
  [a]
  (mod a (* 2 Math/PI)))

(defrecord Arc
  [center r start end dir]
  Stroke
  (length
    [this]
    (let [dif (* (- (angle (:end this))
                    (angle (:start this)))
                 (:r this))]
      (case (:dir this)
        :clock-wise (- (* 2 Math/PI) dif)
        :counter-clock-wise dif)))
  (tangent
    [this p]
    (let [zero-between (and
                        (neg? (:start this))
                        (pos? (:end this)))
          through-zero (if (= (:dir this) :clock-wise)
                         (not zero-between)
                         zero-between)
          [p'] (.point (if through-zero
                         (->ComposedStroke
                          [(->Linear [(:start this)]
                                     [0])
                           (->Linear [0]
                                     [(:end this)])])
                         (->Linear [(:start this)]
                                    [(:end this)])) p)]
      ((case (:dir this)
          :clock-wise -
          :counter-clock-wise +) p' (* 0.5 Math/PI))))
  (point
    [this p]
    (let [zero-between (and
                        (neg? (:start this))
                        (pos? (:end this)))
          through-zero (if (= (:dir this) :clock-wise)
                         (not zero-between)
                         zero-between)
          [p'] (.point (if through-zero
                         (->ComposedStroke
                          [(->Linear [(:start this)]
                                     [0])
                           (->Linear [0]
                                     [(:end this)])])
                         (->Linear [(:start this)]
                                   [(:end this)])) p)]
      (doall (map (fn [c fn]
                    (+ c (* (:r this) (apply fn p'))))
                  (:center this)
                  [cos sin])))))

