(ns jest.scheduler
  "A pausable event scheduler and game clock."
  (:import java.util.Date
           [java.util.concurrent Executors TimeUnit]))

(defonce ^:private timer-data (atom nil))
(defonce ^:private thread-pool (atom nil))
(defonce ^:private tasks (atom {}))

(defn get-thread-pool ^java.util.concurrent.ScheduledExecutorService []
  @thread-pool)

;; An agent is used here because anything sent to an agent from within a
;; transaction will only run if the transaction succeeds, and will only run once
(def ^:private schedule-agent (agent nil))
(set-error-handler! schedule-agent clojure.stacktrace/print-stack-trace)
(set-error-mode! schedule-agent :continue)

(defn- time-millis []
  (.getTime (Date.)))

(defn calculate-game-time
  "Calculates the current game time"
  []
  (if @timer-data
    (let [[start-time paused] @timer-data]
      (or paused
          (- (time-millis) start-time)))
    0))

(defn- calculate-real-time
  "Maps a game time to real time."
  [game-time]
  {:pre [(number? game-time)]}
  (+ (first @timer-data) game-time))

(defn- calculate-delay
  "Calculates the difference between the given game time and the current real
   time."
  ^long [game-time]
  (- (calculate-real-time game-time) (time-millis)))

(def ^{:doc "A derefable variable specifying the current game time, in
            milliseconds. Only valid after the scheduler has started."}
  game-time
  (reify clojure.lang.IDeref
    (deref [_] (calculate-game-time))))


(defn start!
  "Starts the scheduler. After calling start, game-time will become
  valid and schedule may be called."
  [] {:pre [(not @timer-data)
         (not @thread-pool)]}
  (io!
   (reset! timer-data [(time-millis) nil])
   (reset! thread-pool (Executors/newScheduledThreadPool 10)))
  nil)

(defn stop!
  "Stops the scheduler. After calling stop, game-time will become
  invalid and schedule may not be called anymore."
  [] {:pre
  [@timer-data @thread-pool]}
  (io!
   (reset! timer-data nil)
   (.shutdownNow (get-thread-pool))
   (reset! thread-pool nil)
   (reset! tasks nil))
  nil)

(defn started?
  "Returns true if the scheduler has been started, false otherwise."
  []
  (boolean @timer-data))

(defn scheduler-reset!
  "Resets the scheduler to a stopped state, regardless of what it was in before"
  []
  (if (started?)
    (stop!)))

(defn paused?
  "Returns true if the scheduler has been paused, false otherwise."
  []
  (and (started?)
       (boolean (second @timer-data))))

(defn- unschedule-wrapper
  "Wraps a function to remove itself from the task list after running."
  ^Runnable [task time]
  (fn []
    (task)
    (swap! tasks
           (fn [tasks]
             (let [updated-task-list (remove #(= task (first %))
                                             (tasks time))]
               (if (empty? updated-task-list)
                 (dissoc tasks time)
                 (assoc tasks time updated-task-list)))))))

(defn- register-with-scheduler
  "Registers a function with the Java scheduler."
  [task time]
  (.schedule (get-thread-pool)
             (unschedule-wrapper task time)
             (calculate-delay time)
             TimeUnit/MILLISECONDS))

(defn- collect-task-map
  "maps over all tasks and returns the result of running a function on them,
   grouped by time. The function receives 2 arguments: task time and a vector of
   2 elements, the task function and the Future object returned by the Java
   scheduler."
  [f]
  (fn [tasks]
    (into {}
          (for [[time task-list] tasks]
            [time (doall (for [[task future] task-list]
                           (f time [task future])))]))))

(defn- unregister-all
  "Unregisters all tasks from the Java scheduler."
  []
  {:pre [(paused?)]}
  (swap! tasks
         (collect-task-map (fn [_ [task ^java.util.concurrent.Future future]]
                             (.cancel future false)
                             [task nil]))))

(defn- reregister-all
  "Reregisters all tasks with the Java scheduler."
  []
  {:pre [(not (paused?))]}
  (swap! tasks
         (collect-task-map (fn [time [task _]]
                             [task (register-with-scheduler task time)]))))

(defn pause!
  "Pauses the scheduler. The game-time clock will freeze, and none of the
  scheduled tasks will run until the scheduler is resumed again."
  []
  {:pre [@timer-data
         (not (second @timer-data))]}
  (io!
   (swap! timer-data (fn [[time paused]]
                       [time @game-time]))
   (unregister-all)
   @game-time))

(defn resume!
  "Resumes the scheduler. The game-time clock starts running again, and the
   scheduled tasks will run at their scheduled game times."
  []
  {:pre [@timer-data
         (second @timer-data)]}
  (let [resume-time @game-time]
    (io!
     (swap! timer-data (fn [[time paused]]
                         [(- (time-millis) paused)
                          nil]))
     (reregister-all)
     resume-time)))

(defn offset
  "Calculates a future game time dt time-units in the future. By default,
  time-units is :milliseconds. Other allowed values are :seconds and :minutes."
  ([dt]
     (+ @game-time dt))
  ([dt time-unit]
     (offset (* dt
                (case time-unit
                  :minutes 60000
                  :seconds 1000
                  :milliseconds 1)))))

(defn schedule
  "Schedules the given task at the specified game time. This may only
  be called if the scheduler has started. This function is safe to
  call from a transaction."
  [task time] {:pre [@timer-data]}
  (send schedule-agent
        (fn [_]
          (swap! tasks
                 update-in [time] conj
                 (if (paused?)
                   [task nil]
                   [task
                    (register-with-scheduler task time)]))))
  nil)
