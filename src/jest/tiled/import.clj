(ns jest.tiled.import
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world.building :as building]
            [jest.world.path :as path]
            [jest.world :as world]
            [jest.util :as util]
            [jest.tiled.validation :as validation]
            [brick.image :as image]))

;; V0.1; Only support for roads, no

(defn- workable-world
  []
  (for [x (range (cell/world-width))
        y (range (cell/world-height))]
    (cell/cell [x y])))

(defn- place-building [c d]
  (let [parts (clojure.string/split (name d) #"-")
        type (first parts)
        r (map keyword (rest parts))]
    (apply (partial ( building/get-build-function (keyword type)) c) r)))

(defn- place-paths [type c d]
  (path/build-path c d type))

(defn- parse [f cells lookup-fn layer]
  (map f cells (map lookup-fn (:data layer))))

(defmulti parse-layer
  "Updates the loaded world with the data supplied in the layer"
  (fn [layer _ _]
    (println "in selector")
    (keyword (:name layer))))

(defmethod parse-layer :background [layer lookup cells]
  (parse cell/set-background cells lookup layer))

(defmethod parse-layer :buildings [layer lookup cells]
  (parse place-building cells lookup layer))

(defmethod parse-layer :road [layer lookup cells]
  (parse (partial place-paths :road) cells lookup layer))

(defmethod parse-layer :rails [layer lookup cells]
  (parse (partial place-paths :rails) cells lookup layer))

(defmethod parse-layer :canal [layer lookup cells]
  (parse (partial place-paths :canal) cells lookup layer))

(defmethod parse-layer :default [_ _ _]
  (println "default handler"))

(defn layer-selector [_ layer _]
  (println layer)
  (keyword (layer :name)))

(defn- parse-tilesets
  "Parses the the tilesets entry in a valid level, returning index->image and
   index->keyword dictionaries"
  [tilesets]
  (loop [tilesets tilesets
         images {}
         props {}]
    (if (seq tilesets)
      (let [current-tileset (first tilesets)
            prop (:properties current-tileset)
            image-path (clojure.java.io/resource (:image current-tileset))
            image-vec (image/load-images
                       (image/path->PImage image-path)
                       [(:tilewidth current-tileset)
                        (:tileheight current-tileset)])]
        (recur (rest tilesets)
               (into images (util/offset-vec image-vec (count images)))
               (into props (util/offset-map prop (count images)))))
      [images props])))


(defn- initialize-world
  "Initializes a world according with the correct width and height"
  [tiled-world]
  (let [w (:width tiled-world)
        h (:height tiled-world)]
    (cell/initialize-world w h)))

;; TODO
;; what should we be able to put in a world state?
(defn parse-world [json-data]
  (let [tilesets (parse-tilesets (:tilesets json-data))
        lookup (second tilesets)
        lookup-layer #(map lookup (:data %))
        layers (:layers json-data)]
    (initialize-world json-data)
    ;(configure renderer)
    (let [cells (workable-world)]
      (doseq [layer layers]
        (parse-layer layer lookup cells)))))
