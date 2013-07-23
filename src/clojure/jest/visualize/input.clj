(ns jest.visualize.input
  "input handlers for a quil window that can be set from elsewhere.")

(defonce handlers (atom  {:on-down (fn [])
                          :on-up (fn [])
                          :on-move (fn [])
                          :on-key-typed (fn [])}))

(defn set-input-handler! [on fn]
  (swap! handlers assoc on fn))

(defn- call-handler [on]
  ((@handlers on)))

(defn on-down-handler []
  (call-handler :on-down))

(defn on-up-handler []
  (call-handler :on-up))

(defn on-move-handler []
  (call-handler :on-move))

(defn on-key-typed-handler []
  (call-handler :on-key-typed))
