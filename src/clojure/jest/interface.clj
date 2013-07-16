(ns jest.interface
  (:require [seesaw.bind :as b])
  (:use seesaw.mig
        seesaw.core))

(defn- fn-button-helper [f]
  (partial button :listen [:action (fn [_] (f))]))

(defn fn-button [f t]
  ((fn-button-helper f) :text t))

(defn watcher-label [w t tf]
  (let [vis #(str t ": " %1)
        l (label (vis "---"))]
    (b/bind w (b/transform (comp vis (or tf identity))) l)
    l))


(defn temp-gui [contents]
  (-> (frame :title "Hello Swing"
             :size [500 :by 500]
             :content (flow-panel :align :left :items contents))
      show!)
)

(defn create-control-panel [config]
  (let [{:keys [fn-s watches levels]} config]
    (let [fn-buttons (for [{:keys [f t]} fn-s]
                       (fn-button f t))
          watches (for [{:keys [w t tf]} watches]
                    (watcher-label w t tf))]
      (concat fn-buttons watches))))

(def x (atom 0))
