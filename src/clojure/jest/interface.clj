(ns jest.interface
  (:require [seesaw.bind :as b])
  (:use seesaw.mig
        seesaw.core))

(defn- fn-button-helper [f]
  (partial button :listen [:action (fn [_] (f))]))

(defn fn-button [f t]
  ((fn-button-helper f) :text t))

(defn- watcher-label [w t]
  (let [tr #(str t ": " %1)
        l (label (tr "---"))]
    (b/bind w (b/transform tr) l)
    l))



(defn temp-gui [contents]
  (-> (frame :title "Hello Swing"
             :size [500 :by 500]
             :content (mig-panel :items (map vector contents)))
      show!)
)

(defn create-control-panel [config]
  (let [{:keys [fn-s watches levels]} config]
    (let [fn-buttons (for [{:keys [f t]} fn-s]
                       (fn-button f t))
          watches (for [{:keys [w t]} watches]
                    (watcher-label w t))]
      (concat fn-buttons watches))))

(def x (atom 0))
