(ns jest.visualize.score-screen
  (:require [brick.drawable :as d]
            [quil.core :refer [text push-style pop-style fill rect]])
  (:require [jest.score :refer [current-score]]))

(defn score-screen
  []
  (reify d/Drawable
    (draw [this [w h]]
      (push-style)
      (fill 0 192)
      (rect 0 0 w h)
      (fill 255)
      (text (str "You have Won! Score: " @current-score)
            (int (- (/ w 2) 100))
            (int (- (/ h 2) 10))
            200
            20
            )
      (pop-style))))
