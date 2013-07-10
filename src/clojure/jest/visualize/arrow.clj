(ns jest.visualize.arrow
  "Functions to facilitate the visualisation of the world state."
  (:use jest.util)
  (:require [brick.drawable :as drawable]
            [quil.core :as quil])
  (:require [jest.color :as color]))

(defn arrow [cx cy angle colors]
  (let [amount (count colors)]
    (drawable/->Floating
     (reify drawable/Drawable
       (draw [this [w h]]
         (quil/with-translation [(* w 5/8)
                                 (/ h 2)]
           (quil/push-style)
           (quil/stroke 255)
           ;;(quil/smooth)
           (let [len (/ w 4)
                 fat (/ h 12)]
             (quil/stroke-weight (/ len 12))
             (quil/line 0 0 len 0)
             (quil/line len 0 (- len fat) (quil/ceil (- fat)))
             (quil/line len 0 (- len fat) (quil/ceil fat))
             (quil/color-mode :hsb)
             (loop [ls (drawable/ranges (max amount fat) len)
                    cs colors]
               (if (and (seq ls) (seq cs))
                 (let [[offset l] (first ls)
                       endp (+ offset l)
                       c (first cs)]
                   (apply quil/stroke (color/hue->hsb c))
                   (quil/line offset 0 (- endp fat) (quil/ceil (- fat)))
                   (quil/line offset 0 (- endp fat) (quil/ceil fat))
                   (recur (rest ls)
                          (rest cs)))))
             (quil/color-mode :rgb))

           (quil/pop-style))))
     [cx cy] 1 angle)))
