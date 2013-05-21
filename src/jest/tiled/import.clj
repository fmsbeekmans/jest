(ns jest.tiled.import
  "Functions for importing tiled maps in the JSON format"
  (:require [clojure.data.json :as json]
            [jest.world.cell :as cell]
            [jest.world :as world]))

;; Concept
; A JSON description of the tiled-map is converted to a clojure map,
; with keywords for keys.

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
