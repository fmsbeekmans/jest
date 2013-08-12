(ns jest.interface
  (:require [seesaw.bind :as b])
  (:require [jest.level :as level])
  (:require [me.raynes.fs :as fs])
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

(defn level-fn [level]
  (partial level/load-world (fs/absolute-path level)))

(defn levels [dir]
  (let [level-files (filter #(= (fs/extension %) ".level") (file-seq dir))]
    (map level-fn level-files)))

(def level-dir (fs/file "levels"))
(defn level-conf [i f]
  {:f f :t (str "Level " i)})

(def conf {:fn-s (vec (map-indexed level-conf (levels level-dir)))})


;; (def ex-conf
;;   {:fn-s [{:f pause! :t "Pause"}
;;            {:f resume! :t "Resume"}
;;            {:f (level-helper [:tutorial 0]) :t "Level 0"}
;;            {:f (level-helper [:tutorial 1]) :t "Level 1"}]
;;    :watches [{:w world-sketch, :t "World Sketch"}
;;              {:w world-bricklet, :t "World bricklet"}]})

;; (defn demo-conf []
;;   {:fn-s (vec (concat [{:f pause! :t "Pause"}
;;                        {:f resume! :t "Resume"}
;;                        {:f start-spawning :t "Start spawning"}
;;                        {:f stop-spawning :t "Stop spawning"}]
;;                       (map-indexed
;;                        (fn [i f] {:f f :t (str "Level " i)})
;;                        (:tutorial levels))))
;;    :watches (vec (concat (if-let [wb @world-bricklet]
;;                            [{:w (:command-queue wb) :t "Command-queue" :tf count}])))})

;; usage: (temp-gui (create-control-panel (demo-conf)))



