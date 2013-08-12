(ns jest.behavior.callback)

(def ^:private done-callback (atom (fn [])))

(defn set-done-callback [f]
  (reset! done-callback f))

(defn reset-done-callback []
  (reset! done-callback (fn [])))

(defn on-done []
  (@done-callback))
