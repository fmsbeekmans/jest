(ns jest.tiled.import-test
  (:use midje.sweet
        jest.testutils
        [clojure.test :only [deftest]])
  (:require [jest.tiled.import :as import]
            [brick.image :as image]
            [jest.visualize.visualize :as visualize]
            [jest.world.cell :as cell]
            [jest.world :as world]
            [jest.world.path :as path]
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

(fact "Parsing a valid tileset should return a proper
image-list and dictionary"
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
   [(util/offset-vec sub-images 1)
    (test-dict 4 #(Integer/valueOf (name %1)))]
   (provided
    (image/load-images
     (image/path->PImage ..image-path..)
     [..tw.. ..th..])
      => sub-images)))

(fact "Parsing an empty tileset should return
an empty image-list an dictionary"
 (import/parse-tilesets []) => [{} {}])

(fact "Running the layer selector on a layer without a name returns nil"
 (import/layer-selector {} anything anything) => nil)

(fact "Running the layer selector on a layer with a keyword name
returns the keyword"
 (import/layer-selector {:name :background} anything anything)
   => :background)

(fact "Running the layer selector on a layer with a string name
returns the keywordized string"
 (import/layer-selector {:name "grass"} anything anything)
   => :grass)

(cell/with-initialized-temp-world [0 0]
  (fact "workable world should return an empty seq if the world is empty"
    (#'import/workable-world) => '()))

(cell/with-initialized-temp-world [2 2]
  (let [c world/cell]
    (fact "workable world should return an empty seq if the world is empty"
      (#'import/workable-world) => (map c [[0 0]
                                                 [1 0]
                                                 [0 1]
                                                 [1 1]]))

    (fact "place-building should place a
truck spawner in case of valid buildings"
      (do
        (#'import/place-building (c [0 0]) :spawn-truck)
        (let [t (c [0 0])]
          (:building-type t) => :spawn
          (:vehicle-type t) => :truck)))
    (fact "place-paths should construct paths in both
the current and adjacent cell"
      (let [from #(c [0 0])
            d :east
            o-d (path/opposite-dirs d)
            to #(c (#'world/calculate-coord (from) d))
            out-selector (comp d :paths)
            in-selector (comp o-d :paths)]
        (out-selector (from)) => nil
        (in-selector (to)) => nil
        (do
          (#'import/place-paths ..some-path.. (from) d)
          (:direction (out-selector (from))) => d
          (:type (out-selector (from))) => ..some-path..
          (:inout (out-selector (from))) => :out
          (:direction (in-selector (to))) => o-d
          (:type (in-selector (to))) => ..some-path..
          (:inout (in-selector (to))) => :in)))))

(fact "initialize world should create a world with
the correct properties"
  (let [i #'import/initialize-world
        ww world/world-width
        wh world/world-height
        mw (fn [w h] {:width w :height h})]
    (i (mw 0 0))
    (ww) => 0
    (wh) => 0
    (i (mw 10 10))
    (ww) => 10
    (wh) => 10))

(cell/with-initialized-temp-world [2 2]
  (fact "parse should correctly alter the cells"
    (let [f (fn [c d] (:coord c))
          cells (#'import/workable-world)
          lookup-fn identity
          layer {:data (range 4)}]
      (#'import/parse f cells lookup-fn layer)
        => (seq [[0 0]
                 [1 0]
                 [0 1]
                 [1 1]]))))

(fact "the world-parser should correctly call the lower-level functions"
  (let [world {:tilesets ..tile-sets..
               :layers [{:name :background :data [..some-bg-key..]}]
               :width 4
               :height 4}]
    (import/parse-world world) => nil
    (provided
     (import/parse-tilesets ..tile-sets..)
     => [{} {..some-bg-key.. ..some-bg..}]
;;TODO ..some-fn.. is not a wildcard, the expression below does not work!
;     (visualize/setup ..some-fn..) => nil
     )
    (world/world-width) => 4
    (world/world-height) => 4
    (:background ( world/cell [0 0])) => ..some-bg..))


(cell/with-initialized-temp-world [2 2]
  (fact "parser should correctly dispatch to background mm"
    (let [l {:name :background :data [..bg1..
                                      ..bg2..
                                      ..bg3..
                                      ..bg4..]}
          cells (#'import/workable-world)
          get-bg (comp :background world/cell)]
      (import/parse-layer l identity cells)
      (get-bg [0 0]) => ..bg1..
      (get-bg [1 0]) => ..bg2..
      (get-bg [0 1]) => ..bg3..
      (get-bg [1 1]) => ..bg4..)))

(cell/with-initialized-temp-world [2 1]
  (fact "parser should correctly dispatch to road mm"
    (let [l {:name :road :data [:road-east]}
          cells (#'import/workable-world)
          get-road (comp :east :paths world/cell)]
      (import/parse-layer l identity cells)
      (get-road [0 0]) => truthy)))

(cell/with-initialized-temp-world [2 1]
  (fact "parser should correctly dispatch to canal mm"
    (let [l {:name :canal :data [:canal-east]}
          cells (#'import/workable-world)
          get-canal (comp :east :paths world/cell)]
      (import/parse-layer l identity cells)
      (get-canal [0 0]) => truthy)))

(cell/with-initialized-temp-world [2 1]
  (fact "parser should correctly dispatch to rails mm"
    (let [l {:name :rails :data [:rails-east]}
          cells (#'import/workable-world)
          get-rails (comp :east :paths world/cell)]
      (import/parse-layer l identity cells)
      (get-rails [0 0]) => truthy)))

(cell/with-initialized-temp-world [2 1]
  (fact "parser should correctly dispatch to buildings mm"
    (let [l {:name :buildings :data [:spawn-truck]}
          cells (#'import/workable-world)
          get-building (comp :building-type world/cell)]
      (import/parse-layer l identity cells)
      (get-building [0 0]) => truthy)))

(cell/with-initialized-temp-world [0 0]
  (fact "parser should correctly dispatch to default mm"
    (let [l {:name ..something..}]
      (import/parse-layer l identity ..some-cells..) => nil)))
