(ns jest.tiled.import
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world :as world]
            [jest.util :as util]
            [jest.tiled.validation :as validation]
            [brick.image :as image]))

(defn- parse-tilesets
  "Parses the the tilesets entry in a valid level, returning index->image and
   index->keyword dictionaries"
  [tilesets]
  (loop [tilesets tilesets
         offset 0
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
               (+ offset (count images))
               (into images (util/offset-vec image-vec offset))
               (into props (util/offset-map prop offset))))
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
