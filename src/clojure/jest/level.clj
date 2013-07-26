(ns jest.level
  (:require [jest.tiled.validation :as validation]
            [jest.tiled.import :as import]
            [jest.world :as world :refer [reset-world]]
            [jest.scheduler :refer [scheduler-reset! start! pause!]]
            [jest.movement :refer [start-spawning stop-spawning
                                   set-depots-filled-callback! reset-depots-filled-callback!
                                   set-no-more-vehicles-callback! reset-no-more-vehicles-callback!]]
            [jest.score :refer [reset-score]]
            [clojure.core.incubator :refer [-?>]]
            [jest.visualize.visualize :refer [visible]]))

(def valid-schema?
  (validation/create-validator validation/meta-schema-url identity))

(def valid-level?
  (validation/create-validator validation/level-schema-url valid-schema?))

(defn load-level [level-resource-path]
  (let [level
        (-?> level-resource-path
             clojure.java.io/resource
             validation/read-json
             valid-level?
             import/parse-world)]
    level))

(defonce current-level (atom nil))

(defn all-depots-filled []
  (stop-spawning)
  (set-no-more-vehicles-callback! win-level))

(defn win-level []
  (reset-depots-filled-callback!)
  (reset-no-more-vehicles-callback!)
  (reset! visible true)
  (pause!))

(defn initialize-level []
  (stop-spawning)
  (reset-world {})
  (scheduler-reset!)
  (reset-score)
  (reset-depots-filled-callback!)
  (reset-no-more-vehicles-callback!)
  (reset! visible false))

(defn start-level
  ([level-fn]
     (start-level level-fn false))
  ([level-fn edit-mode?]
     (reset! current-level level-fn)
     (initialize-level)
     (when-not edit-mode?
       (set-depots-filled-callback! #(send (agent nil) (fn [_] (all-depots-filled)))))

     (level-fn)

     (start!)
     (start-spawning)))

(defn reset-level []
  (start-level @current-level))


