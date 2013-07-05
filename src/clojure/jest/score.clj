(ns jest.score
  (:require [jest.vehicle :refer [cargo-count]]))

(defonce current-score (ref nil))

(defn reset-score []
  (dosync
   (ref-set current-score 0)))

(defn add-score [n]
  (alter current-score + n))

(def scoring
  {:deliver [90 10]
   :explode [-50 -10]
   :despawn-with-cargo [-20 -10]})

(defn get-scoring [event]
  (get scoring event [0 0]))

(def scoring-base (comp first get-scoring))
(def scoring-extra (comp second get-scoring))

(defn calculate-score [event multiplier]
  (+ (scoring-base event)
                   (* multiplier (scoring-extra event))))

(defn score
  ([event multiplier]
     (add-score (calculate-score event multiplier)))
  ([event]
     (score event 1)))

(defn score-vehicle [event v]
  (score event (cargo-count v)))
