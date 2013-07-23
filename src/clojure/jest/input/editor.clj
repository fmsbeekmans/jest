(ns jest.input.editor
  (:require [jest.input.core :refer [set-input-handler!]]
            [jest.input.interaction :as i]
            [jest.world :refer [cell]]
            [jest.world.building :refer [spawn? build-spawn unbuild-spawn
                                         supply? build-supply unbuild-supply
                                         depot? build-depot unbuild-depot
                                         mixer? build-mixer unbuild-mixer
                                         building-type build-restricted
                                         unbuild-restricted]]))

(defn disable-editor []
  (i/interaction-setup))

(defmacro editor [name [& args] & body]
  `(defn ~(symbol (str name "-editor")) [~@args]
     (let [tracker# (atom {})]
       {:on-down (fn [id# pos#]
                   (swap! tracker# assoc id# pos#))
        :on-up (fn [id# ~'pos]
                 (if (= ~'pos (@tracker# id#))
                   ~@body))})))

(defn set-editor [handlers]
  (doseq [[type handler] handlers]
    (set-input-handler! type handler)))

(defn unbuild-any-building [c]
  (if-let [type (building-type c)]
    (@(ns-resolve 'jest.world.building (symbol (str "unbuild-" (subs (str type) 1)))) c)))

(defmacro building-editor [type & args]
  `(editor ~type [~@args]
           (let [c# (cell ~'pos)]
             (if (building-type c#)
               (unbuild-any-building c#)
               (~(symbol (str "build-" type)) c# ~@args)))))

(building-editor spawn type)
(building-editor supply color)
(building-editor depot color goal)
(building-editor mixer)
(building-editor restricted)

(defmacro edit [type & args]
  `(set-editor (~(symbol "jest.input.editor" (str type "-editor")) ~@args)))
