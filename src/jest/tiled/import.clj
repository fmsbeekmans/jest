(ns jest.tiled.import
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world :as world]
            [jest.util :as util]
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

;;; ToDo
; The tileset map should be loaded from the json files, with an offset being
; managed by the loader function. If the first tileset has 4 tiles, the indices
; in the second tileset image-array should be 4 higher than indicated in the
; json meta-data.

; After this, a merged map has to be returned and a function should be returned
; which accepts an array of data and returns a TiledLayer for use in the brick
; engine

(defn- two-step-map
  [m1 m2]
  (into {}
        (for [ [k v] m1
               :when (contains? m2 v)]
          [k (m2 v)])))

;;; ToDo
; Which one of these functions is more idiomatic
; (or more efficient)
(defn- offset-image-vec
  [image-vec offset]
  (zipmap (map (util/ncomp inc offset) (range))
          image-vec))

(defn- offset-image-vec*
  [image-vec offset]
  (let [offset-f (util/ncomp inc offset)]
    (into {}
          (map-indexed (fn [i v] [(offset-f i) v]) image-vec))))

(defn- offset-properties-map
  [properties-map offset]
  (let [offset-f (util/ncomp inc offset)
        keyword->int #(Integer/valueOf (name %1))]
    (zipmap (map (comp offset-f keyword->int)
                 (keys properties-map))
            (map keyword (vals properties-map)))))

(defn- parse-tilesets
  [tilesets]
  (loop [sets tilesets
         offset 0
         images {}
         props {}]
    (if (seq sets)
      (let [current-tileset (first sets)
            image-path (:image current-tileset)
            image-vec [] ;; TODO replace [] with a call to load-images
            prop (:properties current-tileset)]
        (recur (rest sets)
               (+ offset (count images))
               (into images (offset-image-vec image-vec offset))
               (into props (offset-properties-map prop offset))))
      [images props])))

(defn- initialize-world
  "Initializes a world according with the correct width and height"
  [tiled-world]
  (let [w (:width tiled-world)
        h (:height tiled-world)]
    (initialize-world w h)))

(defn- extract-tileset-info
  "Returns required information from the tiled-map for creating Brick tilesets"
  [tileset]
  {:name (:name tileset)
   :image (:image tileset)
   :tileheight (:tileheight tileset)
   :tilewidth (:tilewidth tileset)
   :dict (:properties tileset)})
