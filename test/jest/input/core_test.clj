(ns jest.input.core_test
  (:use midje.sweet)
  (:require [jest.input.core :as ic]
            [jest.visualize.visualize :as v]
            [jest.world :as w]))

(defmacro tiling-fact [doc & body]
  `(fact ~doc
         (ic/reset-pointers!)
         (ic/reset-input-handlers!)
         ~@body
         (ic/reset-input-handlers!)
         (ic/reset-pointers!)
         (against-background
          (v/sketch-size) => [100 100]
          (w/world-size) => [10 10]
          (v/tl) => [0 0]
          (v/br) => [1 1])))

(tiling-fact "pixel->tile correctly calculates a tile based on a pixel"
             (v/pixel->tile 0 0) => [0 0]
             (v/pixel->tile 12 22) => [1 2]
             (v/pixel->tile 200 180) => [9 9]
             (v/pixel->tile -12 -15) => [0 0])

(tiling-fact "The pointer function returns a pointer"
             (ic/receive-down 1 [12 15])
             (ic/receive-down 2 [22 53])
             (ic/receive-down 3 [56 78])
             (ic/pointer 1) => [1 1]
             (ic/pointer 2) => [2 5]
             (ic/pointer 3) => [5 7]
             (ic/pointer 4) => nil
             (ic/receive-up 2)
             (ic/pointer 2) => nil)

(tiling-fact "The all-pointers function returns all known pointers"
             (ic/all-pointers) => nil
             (ic/receive-down 1 [23 45])
             (ic/all-pointers) => (just [[1 [2 4]]])
             (ic/receive-down 2 [56 67])
             (ic/all-pointers) => (just [[1 [2 4]] [2 [5 6]]] :in-any-order)
             (ic/receive-down 3 [66 67])
             (ic/all-pointers) => (just [[1 [2 4]] [2 [5 6]] [3 [6 6]]] :in-any-order)
             (ic/receive-up 2)
             (ic/all-pointers) => (just [[1 [2 4]] [3 [6 6]]] :in-any-order))


(tiling-fact "when receive-down is called, the on-down handler is called"
             (let [result (promise)
                   on-down (fn [id p]
                             (deliver result [ id p]))]
               (ic/set-input-handler! :on-down on-down)
               (ic/receive-down 1 [12 22])
               @result => [1 [1 2]])) 

(tiling-fact "when receive-up is called, the on-up handler is called"
             (let [result (promise)
                   on-up (fn [id p]
                             (deliver result [ id p]))]
               (ic/set-input-handler! :on-up on-up)
               (ic/receive-down 1 [12 22])
               (ic/receive-up 1)
               @result => [1 [1 2]])) 

(tiling-fact "when receive-move is called, the on-move handler is called only when the move would cross a tile border."
             (let [result (atom [])
                   on-move (fn [id p1 p2]
                             (swap! result conj p2))]
               (ic/set-input-handler! :on-move on-move)
               (ic/receive-down 1 [12 22])
               (ic/receive-move 1 [13 22])
               @result => []
               (ic/receive-move 1 [22 23])
               @result => [[2 2]]
               (ic/receive-move 1 [25 31])
               (ic/receive-move 1 [23 43])
               (ic/receive-move 1 [38 47])
               @result => [[2 2] [2 3] [2 4] [3 4]])) 

(tiling-fact "when receive-move is called with a pixel that is not in an
adjacent cell, call the on-move callback repeatedly for adjacent cells."
             (let [result (atom [])
                   on-move (fn [id p1 p2]
                             (swap! result conj p2))]
               (ic/set-input-handler! :on-move on-move)
               (ic/receive-down 1 [0 0])
               (ic/receive-move 1 [72 53])

               ;are all results adjacent?
               (map #(Math/abs (apply + (map - %1 %2)))
                    (conj (butlast @result) [0 0])
                    @result) => (n-of 1 12)

               ;is the last the desired tile?
               (last @result) => [7 5]))
