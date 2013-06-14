(ns jest.world.route-test
  (:use midje.sweet
        jest.testutils
        [jest.world.cell :only [with-initialized-temp-world]]
        [jest.world :only [cell all-cells]])
  (:require [jest.world.route :as route]
            [jest.world.path :as path]))

(fact "all-routes should return an empty seq if there are no valid routes"
  (route/all-routes {}) => '()
  (route/all-routes {:paths nil}) => '()
  (route/all-routes {:paths {:north #{}}}) => '())

(fact "all-routes should return the correct tuples for cells that
do have routes"
  (route/all-routes
   {:paths {:south {:type :road :routes #{:red :green}}
            :west {:type :canal :routes #{:blue}}}}) =>
                             (just '([:south :road #{:red :green}]
                                       [:west :canal #{:blue}])
                                   :in-any-order))

(fact "add-route should add routes to empty paths"
  (#'route/add-route {} ..some-route..) => {:routes #{..some-route..}}
  (#'route/add-route {:routes nil} ..some-route..)
    => {:routes #{..some-route..}}
  (#'route/add-route {:routes #{}} ..some-route..)
    => {:routes #{..some-route..}}
  (#'route/add-route {:routes #{}} ..some-route..)
    => {:routes #{..some-route..}}
  (#'route/add-route {:routes #{}} ..some-route..)
    => {:routes #{..some-route..}})

(fact "add-route should add routes to paths that aren't
already included in the path"
  (let [p {:routes #{..existing-route..}}]
        (#'route/add-route p ..new-route..)
          =>{:routes #{..new-route.. ..existing-route..}}
        (provided
         (jest.color/hue-difference ..new-route.. ..existing-route..) => 1)))

(fact "remove-route should remove existing routes from a path"
  (#'route/remove-route {:routes #{..some-route..}} ..some-route..)
    => {:routes #{}}
  (provided
   (jest.color/hue-difference ..some-route.. ..some-route..) => 0))

(with-initialized-temp-world [2 1]
  (fact "A route can be added and removed from paths on cells"
    (let [ce #(cell [0 0])
          d :east
          t :road
          co 1]
      (path/build-path (ce) d t)
      (route/all-routes (ce)) => '()
      (route/build-route (ce) d co)
      (route/all-routes (ce)) => (seq [[d t #{co}]])
      (route/unbuild-route (ce) d co)
      (route/all-routes (ce)) => '())))
