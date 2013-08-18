(ns jest.input.quil
  "Use the quil mouse input as touch input"
  (:use [jest.input.core :only [receive-down receive-up receive-move]]
        [jest.visualize.input :only [set-input-handler!]]
        [quil.core :only [mouse-x mouse-y]]))

(defn- on-down-handler []
  (receive-down 1 [(mouse-x) (mouse-y)]))
(defn- on-up-handler []
  (receive-up 1))
(defn- on-move-handler []
  (receive-move 1 [(mouse-x) (mouse-y)]))

(defn setup-quil-mouse-input
  "Ensures that mouse events in the game window will be passed on to the hardware-agnostic input layer."
  []
  (set-input-handler! :on-down on-down-handler)
  (set-input-handler! :on-up on-up-handler)
  (set-input-handler! :on-move on-move-handler))
