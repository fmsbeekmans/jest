(ns jest.score
  "Functions for keeping score."
  (:require [jest.world :refer [coords]]
            [jest.world.vehicle :refer [cargo-count vehicle-cell]]))

(defonce current-score (ref nil))

(defn reset-score
  "Resets the score to 0."
  []
  (dosync
   (ref-set current-score 0)))


(def score-agent (agent nil :error-mode :continue))


(defn add-score
  "Adds n to the current score."
  [n]
  (alter current-score + n))

(def scoring
  {:get-cargo [0 -10]
   :drop-cargo [0 100]
   :spawn [-5 0]
   :despawn [5 0]})

(defn get-scoring [event]
  (get scoring event [0 0]))

(def scoring-base (comp first get-scoring))
(def scoring-extra (comp second get-scoring))

(defn calculate-score [event multiplier]
  (+ (scoring-base event)
                   (* multiplier (scoring-extra event))))

(def visualize-score-fn
  "should be a 3-arity fn where the args are the score, tile and type"
  (atom (fn [_ _ _])))

(defn set-visualize-score-fn!
  "Sets callback function to perform score visualisation task when a score event happens."
  [f]
  (reset! visualize-score-fn f))

(defn score
  "Add or subtract an amount from the current score based on a particular event and a multiplier."
  ([tile event multiplier]
     (let [sc (calculate-score event multiplier)]
       (add-score sc)
       (send score-agent
             (fn [_] (@visualize-score-fn sc tile :wut)))))
  ([tile event]
     (score tile event 1)))

(defn score-vehicle
  "Add or subtract an amount from the current score based on a particular event and a vehicle."
  [event v]
  (score (coords (vehicle-cell v)) event (cargo-count v)))
