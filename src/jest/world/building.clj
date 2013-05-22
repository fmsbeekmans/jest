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
  ([c type color]
     {:pre [(= (:type c) :none)]}
     (assoc c
       :type type
       :resource color))
  ([c type]
     (add-building c type :none)))

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
  "Creates helper functions for building construction.
'type' is the name of the building type (as a symbol).
'resource?' specifies whether or not this building type is associated with a
particular resource."
  [type resource?]
  {:private true}
  (let [add (symbol (str "add-" type))
        remove (symbol (str "remove-" type))
        build (symbol (str "build-" type))
        unbuild (symbol (str "unbuild-" type))
        pred (symbol (str type "?"))
        all (symbol (str "all-" (plural (str type))))
        optional-resource-argument (if resource? '(resource) '())
        optional-documentation-addition (if resource?
                                          (format " 'resource' specifies what resource is associated with this %s."
                                                  type)
                                          "")]
    `(do
       (defn- ~add
         ~(format "adds a %s to the given cell.%s" type optional-documentation-addition)
         [~'c ~@optional-resource-argument]
         (add-building ~'c ~(keyword type) ~@optional-resource-argument))

       (defn ~build
         ~(format "Alters world state by building a %s to the given cell.\n%s" type optional-documentation-addition)
         ([~'c ~@optional-resource-argument]
            (dosync (alter-cell ~'c ~add ~@optional-resource-argument)))
         ([~'x ~'y ~@optional-resource-argument]
            (~build (cell ~'x ~'y) ~@optional-resource-argument)))


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

(defbuilding spawn false)
(defbuilding supply true)
(defbuilding depot true)
(defbuilding mixer false)

(defn building-type
  "Returns the building type for the given cell."
  [c]
  (when-not (= :none (:type c))
    (:type c)))

