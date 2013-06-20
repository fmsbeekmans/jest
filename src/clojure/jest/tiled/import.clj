(ns jest.tiled.import
  "Functions for importing TiLeD game-maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world.building :as building]
            [jest.visualize.visualize :as visualize]
            [jest.world.path :as path]
            [jest.world :as world]
            [jest.util :as util]
            [jest.tiled.validation :as validation]
            [brick.image :as image]))

(defn- workable-world
  "Returns an ordered copy of the loaded world"
  []
  (for [y (range (world/world-height))
        x (range (world/world-width))]
    (world/cell [x y])))

(defn- place-building
  "Parses keyword d an places the correct building in cell c"
  [c d]
  (if (keyword? d)
    (let [parts (clojure.string/split (name d) #"-")
          type (first parts)
          r (map keyword (rest parts))]
      (apply (partial ( building/get-build-function (keyword type)) c) r))))

(defn- place-paths
  "Parses keyword d and constructs the correct path type in cell c"
  [type c d]
  (if (keyword? d)
    (let [parts (clojure.string/split (name d) #"-")
          dir (keyword (last parts))]
      (path/build-path c dir type))))

(defn- parse
  "Apply a function to all cells with the given lookup-fn and the
  data-section of layer"
  [f cells lookup-fn layer]
  (doall
   (map f cells (map lookup-fn (:data layer)))))

(defn layer-selector
  "Extracts the type of layer in valid levels"
  [layer _ _]
  (keyword (layer :name)))

(defmulti parse-layer
  "Updates the loaded world through cells with the data supplied in the layer"
  #'layer-selector)

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

(defmethod parse-layer :default [_ _ _])


(defn parse-tilesets
  "Parses the the tilesets entry in a valid level, returning index->image and
   index->keyword dictionaries"
  [tilesets]
  (loop [tilesets tilesets
         images {}
         props {}]
    (if (seq tilesets)
      (let [current-tileset (first tilesets)
            prop (:properties current-tileset)
            image-vec (image/load-images
                       (image/path->PImage (:image current-tileset))
                       [(:tilewidth current-tileset)
                        (:tileheight current-tileset)])]
        (recur (rest tilesets)
               (into images (util/offset-vec image-vec (count images)))
               (into props (util/offset-map prop (count images)))))
      [images props])))


(defn- initialize-world
  "Initializes a world according with the correct width and height extracted
  from tiled-world"
  [tiled-world]
  (let [w (:width tiled-world)
        h (:height tiled-world)]
    (cell/initialize-world w h)))

(defn parse-world
  "Parses json-data in an attempt to create a consistent game world."
  [json-data]
  (let [tilesets (parse-tilesets (:tilesets json-data))
        lookup (second tilesets)
        lookup-layer #(map lookup (:data %))
        layers (:layers json-data)]
    (initialize-world json-data)
    (visualize/setup
     (let [lookup-map (util/remap-maps (first tilesets)
                                       (second tilesets))]
       (fn [k]
         (lookup-map k (lookup-map :default)))))
    (let [cells (workable-world)]
      (doseq [layer layers]
        (parse-layer layer lookup cells)))))
