(ns jest.input.wm-touch
  (:require [jest.input.core :refer [receive-down receive-up receive-move]]
            [jest.visualize.visualize :refer [world-sketch]]
            [jest.visualize.util :refer [get-frame-offset get-pane-offset]]
            [clojure.core.incubator :refer [defmacro-]])
  (:import IUser IKernel IAWTCallback Hooks$IGetMsgProc Hooks$ICallWndProc TouchInput
           [com.sun.jna Native Pointer Callback]
           [com.sun.jna.win32 StdCallLibrary StdCallLibrary$StdCallCallback]
           [com.sun.jna.platform.win32 WinUser WinDef$HWND WinDef$WPARAM WinDef$LPARAM BaseTSD$LONG_PTR WinDef$LRESULT WinNT$HANDLE]
           java.util.Date))

(defn- setup-native [] 
  (def nativeuser (Native/loadLibrary "user32" IUser))
  (def nativekernel (Native/loadLibrary "kernel32" IKernel)))

(defn- get-awt-window []
  (.FindWindowA nativeuser "SunAwtToolkit" "theAwtToolkitWindow"))

(def ^{:private true
       :doc "Message identifier corresponding with WM_AWT_INVOKE_VOID_METHOD."}
  invoke-void-method-msg 0x982a)

(defn- invoke-void-method [f]
  (let [result (promise)
        callback (reify IAWTCallback
                   (callback [this]
                     (deliver result
                              (f))))]
    (.SendMessageA nativeuser
                   (get-awt-window)
                   invoke-void-method-msg
                   callback
                   (WinDef$LPARAM.))
    @result))

(defn- register-touch-window [handle]
  (.RegisterTouchWindow nativeuser
                        handle
                        0))
        
(defn- handle [component]
  (-> (.. component getPeer getHWnd)
      Pointer.
      WinDef$HWND.))


(def ^:private hooks {:wh-getmessage [3 'Hooks$IGetMsgProc]
                      :wh-callwnd [4 'Hooks$ICallWndProc]})

(defn- hook-num [key]
  (first (hooks key)))

(defn- hook-type [key]
  (second (hooks key)))


(defn- window-thread [handle]
  (.GetWindowThreadProcessId nativeuser
                             handle
                             nil))

(defmacro- create-callback [hook f]
  `(reify ~(hook-type hook)
     (~'callback [this# nCode# wParam# msg#]
       (let [result# (~f (.message msg#) (.wParam msg#) (.lParam msg#))]
         (if (and (integer? result#)
                  (>= result# 0))
           (WinDef$LRESULT. result#)
           (.CallNextHookEx nativeuser
                            nil
                            nCode#
                            wParam#
                            msg#))))))

(defmacro- set-hook! [handle hook f]
  `(let [callback# (create-callback ~hook ~f)]
     [(.SetWindowsHookExA nativeuser
                          (hook-num ~hook)
                          callback#
                          nil
                          (window-thread ~handle))
      callback#]))

(defn- remove-hook! [hook]
  (.UnhookWindowsHookEx nativeuser hook))

(defn- low-word [num]
  (bit-and 0xFFFF num))

(defn- as-handle [param]
  (WinNT$HANDLE. (.toPointer param)))

(def ^:private wmtouch 0x240)

(defn- make-touch-array [size]
  (.toArray (TouchInput.) size))

(def ^:private touch-array (make-touch-array 6))

(defn- get-touch-array
  "Returns a touch array that is at least as big as size.
   Reuses previous touch array if possible"
  [size]
  (if (>= (count touch-array)
          size)
    touch-array
    (alter-var-root #'touch-array
                    (constantly (make-touch-array size)))))

(defn- get-filled-touch-array
  "Returns a touch array with touch input retrieved."
  [count handle]
  (let [inputs (get-touch-array count)
        size (.size (first inputs))]
    (.GetTouchInputInfo nativeuser handle count inputs size)
    inputs))

(defn- decode-dwflags [dwflags]
  (cond (bit-test dwflags 2) :up
        (bit-test dwflags 1) :down
        (bit-test dwflags 0) :move))

(defn- handle-touch-event [code wparam lparam]
  (when (= wmtouch code)
    (let [touch-count (-> wparam
                          .intValue
                          low-word)
          handle (as-handle lparam)
          inputs (get-filled-touch-array touch-count handle)]
      (dotimes [i touch-count]
        (let [input (aget inputs i)
              x (int (/ (.x input) 100))
              y (int (/ (.y input) 100))
              [x y] (map -
                         [x y]
                         (map +
                              (get-frame-offset @world-sketch)
                              (get-pane-offset @world-sketch)))
              id (.dwID input)
              event-type (decode-dwflags (.dwFlags input))]
          (case event-type
            :up (receive-up id)
            :down (receive-down id [x y])
            :move (receive-move id [x y])
            nil))))))

(defn- get-quil-sketch-handle []
  (handle @world-sketch))

(defonce ^:private touch-input-state (atom nil))

(defn setup-wm-touch-input! []
  {:pre [(not @touch-input-state)
         @world-sketch]}
  (setup-native)
  (reset! touch-input-state
          (invoke-void-method (fn []
                                (let [handle (get-quil-sketch-handle)
                                      proxy-fn handle-touch-event]
                                        ;#(handle-touch-event %1 %2 %3)
                                  (register-touch-window handle)
                                  [(set-hook! handle :wh-getmessage proxy-fn)
                                   (set-hook! handle :wh-callwnd proxy-fn)])))))

(defn teardown-wm-touch-input! []
  {:pre [@touch-input-state]}
  (let [[[getmessage-hook _]
         [callwnd-hook _]] @touch-input-state]
    (invoke-void-method (fn []
                          (remove-hook! getmessage-hook)
                          (remove-hook! callwnd-hook))))
  (reset! touch-input-state nil))

(defn wm-touch-setup? []
  (boolean @touch-input-state))

(defn ensure-wm-touch-input-setup! []
  (if (wm-touch-setup?)
    (teardown-wm-touch-input!))
  (setup-wm-touch-input!))
