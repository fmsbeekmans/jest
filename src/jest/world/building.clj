(ns jest.world.building
  "Functions for adding, removing and searching for buildings."
  (:use clojure.core.incubator
        jest.util
        jest.world.cell))

(defn building-type
  "Returns the building type for the given cell."
  [c]
  (:building-type c))

(defn unbuilt?
  "Returns true if nothing is built on this cell, false otherwise."
  [c]
  (not (building-type c)))

(defn all-cells-type
  "returns all cells in the currently bound world grid with the given building type"
  [type]
  (all-cells #(= (building-type %1) type)))

(defn add-building
  "adds a building to the given cell"
  [c type & other-fields]
  {:pre [(not (building-type c))]}
  (apply assoc c
         :building-type type
         other-fields))

(defn- remove-building
  "removes a building from the given cell"
  ([c & extra-fields]
     {:pre [(building-type c)]}
     (apply assoc c
            :building-type nil
            (interleave extra-fields (repeat (count extra-fields) nil)))))

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
       (defn ~pred
         ~(format "returns whether or not this cell is of type %s." type)
         ([~'c]
            (= (building-type ~'c) ~(keyword type)))
         ([~'x ~'y]
            (~pred (cell ~'x ~'y))))

       (defn- ~add
         ~(format "adds a %s to the given cell." type)
         [~'c ~@field-syms]
         {:pre  [(unbuilt? ~'c)]
          :post [(~pred ~'%)]}
         (add-building ~'c ~(keyword type) ~@field-args))

       (defn ~build
         ~(format "Alters world state by building a %s to the given cell.\n" type)
         [~'c ~@field-syms]
         (dosync (alter-cell ~'c ~add ~@field-syms)))

       (defn- ~remove
         ~(format "removes a %s from the given cell." type)
         [~'c]
         {:pre  [(~pred ~'c)]
          :post [(unbuilt? ~'%)]}
         (remove-building ~'c ~@fields))

       (defn ~unbuild
         ~(format "Alters world state by unbuilding a %s from the given cell." type)
         [~'c]
         (dosync (alter-cell ~'c ~remove)))

       (defn ~all
         ~(format "returns all cells with building type %s." type)
         []
         (all-cells-type ~(keyword type))))))

(defbuilding spawn :vehicle-type)
(defbuilding supply :resource-type)
(defbuilding depot :resource-type)
(defbuilding mixer)

(defn resource-type
  "Returns the type of resource handled on this supply or depot."
  [c]
  {:pre [(or (supply? c)
             (depot? c))]}
  (:resource-type c))

(defn vehicle-type
  "Returns the vehicle type spawned at this spawn."
  [c]
  {:pre [(spawn? c)]}
  (:vehicle-type c))
