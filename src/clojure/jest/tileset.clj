(ns jest.tileset
  (:require [brick.image :as image]
            [jest.visualize.visualize :as visualize]
            [jest.util :as util]
            [clojure.core.incubator :refer [-?>]]))

(defn- map-int-key->kw [m]
  (zipmap (map (comp keyword str) (keys m))
                   (vals m)))

(defn- map-val->kw [m]
  (zipmap (keys m)
          (map keyword (vals m))))


(defn parse-tileset [tileset-json]
  (let [{:keys [w h image dict]} tileset-json]
     (let [images (image/load-images (image/path->PImage image) [w h])
           images-map (map-int-key->kw (util/offset-vec images 0))
           lookup-map (util/remap-maps images-map (map-val->kw dict))]
       (fn [k]
         (lookup-map k (lookup-map :default))
         ))))


(defn load-tileset [tileset-name]
  (if-let [level
        (-?> tileset-name
             clojure.java.io/resource
             util/read-json
             parse-tileset
             )]
    level))
