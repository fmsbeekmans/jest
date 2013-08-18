(ns jest.behavior.callback
  "Callbacks for important game events.")

(def ^:private done-callback (atom (fn [])))

(defn set-done-callback
  "Sets a callback to be called when a level completes (all depots are filled)."
  [f]
  (reset! done-callback f))

(defn reset-done-callback
  "Unsets the level completion callback."
  []
  (reset! done-callback (fn [])))

(defn on-done
  "To be called when the level completes, to ensure the callback runs."
  []
  (@done-callback))
