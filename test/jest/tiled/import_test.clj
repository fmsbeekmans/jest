(ns jest.tiled.import-test
  (:use midje.sweet
        jest.testutils
        [clojure.test :only [deftest]])
  (:require [jest.tiled.import :as import]
            [brick.image :as image]
            [jest.util :as util]))


(def simple-level
  {:height 4
   :width 4
   :tileheight 32
   :tilewidth 32
   :tilesets []
   :layers []})

(defn test-dict [n f]
  (let [r (range n)
        keywordize (comp keyword str)
        ks (map keywordize r)]
    (zipmap (map f ks) ks)))

(fact "Parsing a valid tileset should return a proper image-list and dictionary"
 (let [sub-images [..sub-image0..
                   ..sub-image1..
                   ..sub-image2..
                   ..sub-image3..]]
   (import/parse-tilesets
    [{:image ..image-path..
      :tileheight ..th..
      :tilewidth ..tw..
      :imageheight ..ih..
      :imagewidth ..iw..
      :properties (test-dict 4 identity)
      :name "test-tileset"}])
   =>
   [(util/offset-vec sub-images 0) (test-dict 4 #(Integer/valueOf (name %1)))]
   (provided
    (clojure.java.io/resource ..image-path..) => ..image-url..
    (image/load-images
     (image/path->PImage ..image-url..) [..tw.. ..th..]) => sub-images)))

(fact "Parsing an empty tileset should return an empty image-list an dictionary"
 (import/parse-tilesets []) => [{} {}])

(fact "Running the layer selector on a layer without a name returns nil"
 (import/layer-selector {} anything anything) => nil)

(fact "Running the layer selector on a layer with a keyword name returns the keyword"
 (import/layer-selector {:name :background} anything anything) => :background)

(fact "Running the layer selector on a layer with a string name returns the keywordized string"
 (import/layer-selector {:name "grass"} anything anything) => :grass)
