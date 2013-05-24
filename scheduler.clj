(ns scheduler
  (:import java.util.Date
           [java.util.concurrent Executors TimeUnit]))

(defonce ^:private timer-data (atom nil))
(defonce ^:private thread-pool (atom nil))
(defonce ^:private tasks (atom {}))
  

(defn- time-millis []
  (.getTime (Date.)))

(defn- calculate-game-time []
  {:pre [@timer-data]}
  (let [[start-time paused] @timer-data]
    (or paused
        (- (time-millis) start-time))))

(defn- calculate-real-time [game-time]
  (+ (first @timer-data) game-time))

(defn- calculate-delay [game-time]
  (- (calculate-real-time game-time) (time-millis)))

(def game-time
  (reify clojure.lang.IDeref
    (deref [_] (calculate-game-time))))

(defn start []
  {:pre [(not @timer-data)
         (not @thread-pool)]}
  (reset! timer-data [(time-millis) nil])
  (reset! thread-pool (Executors/newScheduledThreadPool 10))
  nil)

(defn stop []
  {:pre [@timer-data
         @thread-pool]}
  (reset! timer-data nil)
  (.shutdownNow @thread-pool)
  (reset! thread-pool nil)
  (reset! tasks nil)
  nil)

(defn paused? []
  {:pre [@timer-data]}
  (boolean (second @timer-data)))

(defn- unschedule-wrapper [task time]
  (fn []
    (task)
    (swap! tasks
           (fn [tasks]
             (let [updated-task-list (remove #(= task (first %))
                                             (tasks time))]
               (if (empty? updated-task-list)
                 (dissoc tasks time)
                 (assoc tasks time updated-task-list)))))))

(defn- register-with-scheduler [task time]
  (.schedule @thread-pool
             (unschedule-wrapper task time)
             (calculate-delay time)
             TimeUnit/MILLISECONDS))

(defn- collect-task-map [f]
  (fn [tasks]
    (into {}
          (for [[time task-list] tasks]
            [time (doall (for [[task future] task-list]
                           (f time [task future])))]))))

(defn- unregister-all []
  {:pre [(paused?)]}
  (swap! tasks
         (collect-task-map (fn [_ [task future]]
                             (.cancel future false)
                             [task nil]))))

(defn- reregister-all []
  {:pre [(not (paused?))]}
  (swap! tasks
         (collect-task-map (fn [time [task _]]
                             [task (register-with-scheduler task time)]))))
  
(defn pause []
  {:pre [@timer-data
         (not (second @timer-data))]}
  (swap! timer-data (fn [[time paused]]
                      [time @game-time]))
  (unregister-all)
  @game-time)

(defn resume []
  {:pre [@timer-data
         (second @timer-data)]}
  (let [resume-time @game-time]
    (swap! timer-data (fn [[time paused]]
                    [(- (time-millis) paused)
                     nil]))
    (reregister-all)
    resume-time))

(defn delay
  ([dt]
     (+ @game-time dt))
  ([dt time-unit]
     (delay (* dt
               (case time-unit
                 :minutes 60000
                 :seconds 1000
                 :milliseconds 1)))))

(defn schedule [task time]
  {:pre [@timer-data]}
  (swap! tasks
         update-in [time] conj [task
                                (register-with-scheduler task time)])
  nil)
