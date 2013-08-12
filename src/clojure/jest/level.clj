(ns jest.level
  (:require [jest.world :as world :refer [cell all-cells reset-world world-width world-height]]
            [jest.world.cell :refer [initialize-world]]
            [jest.world.building :refer
             [enable-spawner build-restricted build-mixer build-supply build-depot build-spawn
              spawn? building-type all-spawns all-supplies all-mixers all-depots
              all-restricteds]]
            [jest.scheduler :refer [scheduler-reset! start! pause!]]
            [jest.world.path :refer [out-paths build-path]]
            [jest.behavior.movement :refer [start-spawning stop-spawning
                                            set-done-callback! reset-done-callback!]]
            [jest.score :refer [reset-score]]
            [clojure.core.incubator :refer [-?>]]
            [jest.visualize.visualize :refer [visible]]))

(defonce current-level (atom nil))

(defn win-level []
  (reset! visible true)
  (stop-spawning)
  (pause!))

(defn initialize-level []
  (stop-spawning)
  (reset-world {})
  (scheduler-reset!)
  (reset-score)
  (reset-done-callback!)
  (reset! visible false))

(defn start-level
  ([level-fn]
     (start-level level-fn false))
  ([level-fn edit-mode?]
     (reset! current-level level-fn)
     (initialize-level)
     (when-not edit-mode?
       (set-done-callback! #(send (agent nil) (fn [_] (win-level)))))

     (level-fn)

     (start!)
     (start-spawning)))

(defn reset-level []
  (start-level @current-level))

;; World saving
(defn world-size-definition []
  `(initialize-world ~(world-width) ~(world-height)))

(defn path-definition-for [p]
  `(build-path (cell ~(:coords p)) ~(:direction p) ~(:type p)))

(defn extract-path-definitions []
  (map path-definition-for (flatten (map out-paths (all-cells)))))

(defn print-path-definitions []
  (doseq [pd (extract-path-definitions)]
    (println pd)))

(defn building-for [c]
  (let [type (building-type c)
        form
        `(~(symbol (str "build-" (subs (str  type) 1))) (cell ~(:coord c))
          ~@(case type
              :spawn [(:vehicle-type c)]
              :supply [(:resource-type c)]
              :mixer []
              :restricted []
              :depot [(:resource-type c) (:quotum c)]))]
    (if (and (spawn? c)
             (:spawning? c))
      `(enable-spawner ~form ~(:spawn-offset c) ~(:spawn-rate c))
      form)))

(defn extract-building-definitions []
  (map building-for (concat (all-spawns)
                            (all-supplies)
                            (all-mixers)
                            (all-depots)
                            (all-restricteds))))

(defn print-level []
  (doseq [e (concat [(world-size-definition)]
             (extract-building-definitions)
             (extract-path-definitions))]
    (println e)))

(defn save-world [file-name]
  (spit file-name (with-out-str (print-level))))

;; This is probably the most evil thing I have ever done. So happy my name is still not correctly tied to this commit ~J
(let [this-ns *ns*]
  (defn load-world [file-name]
    (binding [*ns* this-ns]
      (load-file file-name))))

(defn level-helper [level]
  (partial load-world (str "levels/" level ".level")))


