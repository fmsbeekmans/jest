(ns jest.input.editor
  "Alternative interaction functions for level editing."
  (:require [jest.input.core :refer [set-input-handler!]]
            [jest.input.interaction :as i]
            [jest.world :refer [cell]]
            [jest.world.building :refer [spawn? build-spawn unbuild-spawn
                                         supply? build-supply unbuild-supply
                                         depot? build-depot unbuild-depot
                                         mixer? build-mixer unbuild-mixer
                                         building-type build-restricted
                                         unbuild-restricted]]

            [clojure.core.incubator :refer [defmacro-]]))

(defn disable-editor
  "Disables the editor - restores normal interaction functions."
  []
  (i/interaction-setup))

(defmacro- editor
  "Helper macro for creating the various editor functions."
  [name [& args] & body]
  `(defn ~(symbol (str name "-editor"))
     ~(str "Returns a map of interaction handlers to be used with set-editor to build a " name ".")
     [~@args]
     (let [tracker# (atom {})]
       {:on-down (fn [id# pos#]
                   (swap! tracker# assoc id# pos#))
        :on-up (fn [id# ~'pos]
                 (if (= ~'pos (@tracker# id#))
                   ~@body))})))

(defn set-editor
  "Enables an editor. handlers should be the output from one of the editor functions."
  [handlers]
  (doseq [[type handler] handlers]
    (set-input-handler! type handler)))

(defn unbuild-any-building
  "Unbuilds whatever building is on the given cell."
  [c]
  (if-let [type (building-type c)]
    (@(ns-resolve 'jest.world.building (symbol (str "unbuild-" (subs (str type) 1)))) c)))

(defmacro- building-editor
  "Helper macro for creating building editors."
  [type & args]
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

(defmacro edit
  "Starts the editor for the given edit type. Args are editor type specific."
  [type & args]
  `(set-editor (~(symbol "jest.input.editor" (str type "-editor")) ~@args)))
