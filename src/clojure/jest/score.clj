(ns jest.score
  (:require [jest.world :refer [coords]]
            [jest.vehicle :refer [cargo-count vehicle-cell]]))

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

(def visualize-score-fn (atom (fn [score tile type])))
(defn set-visualize-score-fn! [f]
  (reset! visualize-score-fn f))

(defn score
  ([tile event multiplier]
     (let [sc (calculate-score event multiplier)]
       (add-score sc)
       (send score-agent
             (fn [_] (@visualize-score-fn sc tile :wut)))))
  ([tile event]
     (score tile event 1)))

(defn score-vehicle [event v]
  (score (coords (vehicle-cell v)) event (cargo-count v)))
