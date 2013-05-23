(ns jest.world.building
  "Functions for adding, removing and searching for buildings."
  (:use clojure.core.incubator
        jest.util
        jest.world.cell))

(defn all-cells-type
  "returns all cells in the currently bound world grid with the given building type"
  [type]
  (all-cells #(= (:type %1) type)))

(defn- add-building
  "adds a building to the given cell"
  [c type & other-fields]
  {:pre [(= (:type c) :none)]}
  (apply assoc c
         :type type
         other-fields))

(defn- remove-building
  "removes a building from the given cell"
  ([c]
     {:pre [(not= (:type c) :none)]}
     (-> c
         (assoc :type :none)
         (dissoc :resource)))
  ([c type]
     {:pre [(= (:type c) type)]}
     (remove-building c)))

(defmacro- defbuilding
  "creates building helper functions"
  [type & fields]
  (let [add (symbol (str "add-" type))
        remove (symbol (str "remove-" type))
        build (symbol (str "build-" type))
        unbuild (symbol (str "unbuild-" type))
        pred (symbol (str type "?"))
        all (symbol (str "all-" (plural (str type))))
        field-syms (map (comp symbol name) fields)
        field-args (interleave fields field-syms)]
    `(do
       (defn- ~add
         ~(format "adds a %s to the given cell." type)
         [~'c ~@field-syms]
         (add-building ~'c ~(keyword type) ~@field-args))

       (defn ~build
         ~(format "Alters world state by building a %s to the given cell.\n" type)
         ([~'c ~@field-syms]
            (dosync (alter-cell ~'c ~add ~@field-syms)))
         ([~'x ~'y ~@field-syms]
            (~build (cell ~'x ~'y) ~@field-syms)))


       (defn- ~remove
         ~(format "removes a %s from the given cell." type)
         [~'c]
         (remove-building ~'c ~(keyword type)))

       (defn ~unbuild
         ~(format "Alters world state by unbuilding a %s from the given cell." type)
         ([~'c]
            (dosync (alter-cell ~'c ~remove)))
         ([~'x ~'y]
            (~unbuild (cell ~'x ~'y))))

       (defn- ~pred
         ~(format "returns whether or not this cell is of type %s." type)
         ([~'c]
            (= (:type ~'c) ~(keyword type)))
         ([~'x ~'y]
            (~pred (cell ~'x ~'y))))

       (defn ~all
         ~(format "returns all cells with building type %s." type)
         []
         (all-cells-type ~(keyword type)))))) 

(defbuilding spawn :vehicle)
(defbuilding supply :resource)
(defbuilding depot :resource)
(defbuilding mixer)



(defn building-type
  "Returns the building type for the given cell."
  [c]
  (when-not (= :none (:type c))
    (:type c)))

(defn resource-type
  "Returns the type of resource handled on this cell."
  [c]
  (:resource c))
