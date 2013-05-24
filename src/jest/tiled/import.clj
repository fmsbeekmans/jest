(ns jest.tiled.import
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world :as world]
            [jest.util.json-validation :as json-val]
            [clojure.core.incubator :refer [-?>]]))

;; Concept
; A JSON description of the tiled-map is converted to a clojure map,
; with keywords for keys.

(defn- read-json
  [json-path]
  (json/read-str (slurp json-path)
                 :key-fn keyword))

(def meta-schema-url (clojure.java.io/resource "meta.schema"))
(def level-schema-url (clojure.java.io/resource "level.schema"))

(let [schema-validator
         (-?> meta-schema-url
              read-json
              json-val/validator
              json-val/boolean-validator)
      wrapper (or schema-validator
                  (fn [_] nil))]
  (defn- valid-schema? [json-schema]
    (wrapper json-schema)))

(let [level-validator
         (-?> level-schema-url
              read-json
              valid-schema?
              json-val/validator
              json-val/boolean-validator)
      wrapper (or level-validator
                  (fn [_] nil))]
  (defn- valid-level? [json-schema]
    (wrapper json-schema)))

(def tileset
  "Wrapper for the used tileset that facilitates on-the-fly changes"
  (atom []))

(defn- initialize-world
  "Initializes a world according with the correct width and height"
  [tiled-world]
  (let [w (:width tiled-world)
        h (:height tiled-world)]
    (initialize-world w h)))

(defn- extract-tileset-info
  "Returns required information from the tiled-map for creating Brick tilesets"
  [tiled-world]
  (let [tileset (first (:tilesets tiled-world))]
    {:name (:name tileset)
     :image (:image tileset)
     :tileheight (:tileheight tileset)
     :tilewidth (:tilewidth tileset)
     :dict (:properties tileset)}))
