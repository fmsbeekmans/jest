(ns jest.score
  (:require [jest.world :refer [coords]]
            [jest.vehicle :refer [cargo-count vehicle-cell]]
            [jest.visualize.visualize :refer [animate-score-on-tile]]))

(defonce current-score (ref nil))

(defn reset-score []
  (dosync
   (ref-set current-score 0)))


(def score-agent (agent nil :error-mode :continue))


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
  ([tile event multiplier]
     (println :hoi tile)
     (add-score (calculate-score event multiplier))
     (send score-agent
           (fn [_] (animate-score-on-tile (calculate-score event multiplier) tile :wut 60))))
  ([tile event]
     (score tile event 1)))

(defn score-vehicle [event v]
  (score (coords (vehicle-cell v)) event (cargo-count v)))
