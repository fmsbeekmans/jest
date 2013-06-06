(ns jest.tiled.import
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world.building :as building]
            [jest.world :as world]
            [jest.util :as util]
            [jest.tiled.validation :as validation]
            [brick.image :as image]))

;; V0.1; Only support for roads, no

(defn- lookup-table
  [m]
  (fn [k]
    (or (get m k) :none)))

(defn- workable-world
  []
  (for [x (range (cell/world-width))
        y (range (cell/world-height))]
    (cell/cell [x y])))

(defn- parse-background [cells data]
  (map cell/set-background cells data))

(defn- building-helper [k]
  (let [parts (clojure.string/split (name k) #"-")
        builder (ns-resolve *ns* (symbol (str "building/build-" (first parts))))
        resource-type (keyword (second parts))]
    #(builder %1 resource-type)))

(defn- parse-buildings [cells data]
  (let [build-selector (memoize building-helper)
        create-buildings
        (fn [c d]
          (let [b (build-selector d)]
            (b c)))]
    (map create-buildings cells data)))

(defn layer-type [n l]
  (= n (keyword (:name l))))

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
        lookup (lookup-table (second tilesets))
        lookup-layer #(map lookup (:data %))
        layers (:layers json-data)]
    (initialize-world json-data)
    ;(configure renderer)
    (let [cells (workable-world)]
      (if-let [background-layer (first (filter (partial layer-type :background)
                                       layers))]
                 (parse-background cells (lookup-layer background-layer)))

      (if-let [building-layer (first (filter (partial layer-type :buildings)
                                             layers))]
                 (parse-buildings cells (lookup-layer building-layer))))))
