(ns jest.visualize.input
  "input handlers for a quil window that can be set from elsewhere.")

(defonce handlers
  #^{:doc"The mapping from event to handler."}
  (atom  {:on-down (fn [])
          :on-up (fn [])
          :on-move (fn [])
          :on-key-typed (fn [])}))

(defn set-input-handler!
  "Set the input handlers"
  [on fn]
  (swap! handlers assoc on fn))

(defn- call-handler
  "Call the handler for the given event."
  [on]
  ((@handlers on)))

(defn on-down-handler
  "Call the on-down handler"
  []
  (call-handler :on-down))

(defn on-up-handler
  "Call the on-up handler"
  []
  (call-handler :on-up))

(defn on-move-handler
  "Call the on-move handler"
  []
  (call-handler :on-move))

(defn on-key-typed-handler
  "Call the on-key-typed handler"
  []
  (call-handler :on-key-typed))
