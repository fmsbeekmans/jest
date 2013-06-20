(ns jest.visualize.input
  "input handlers for a quil window that can be set from elsewhere.")

(defn on-down-handler [])
(defn on-up-handler [])
(defn on-move-handler [])

(def handlers {:on-down #'on-down-handler
               :on-up #'on-up-handler
               :on-move #'on-move-handler})

(defn set-input-handler! [on fn]
  (alter-var-root (handlers on) (constantly fn)))
