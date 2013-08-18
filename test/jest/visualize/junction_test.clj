(ns jest.visualize.junction-test
  (:use [midje.sweet :only [fact roughly]])
  (:use jest.visualize.junction))

(defn- road-mocker [inout dir]
  {:inout inout :direction dir})
(def rm road-mocker)

(fact "Sorting by direction should go from north to east"
      (sort-by direction-order [:west :east :north :south]) =>
      [:north :west :south :east]
      (sort-by direction-order [:north :west :south :east :south]) =>
      [:north :west :south :south :east])

(fact "Matching roads should return the correct index-keyword"
      (match-roads [(rm :out :north)]) => :road-n
      (match-roads [(rm :in :north)]) => :road-end-s
      (match-roads [(rm :out :north) (rm :in :west)]) => :turn-nw
      (match-roads [(rm :out :north) (rm :out :west) (rm :in :south)]) =>
      :cross-t-e
      (match-roads [(rm :out :north) (rm :out :west)
                   (rm :out :south) (rm :out :east)]) => :cross)
