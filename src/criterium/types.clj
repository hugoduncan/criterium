(ns criterium.types
  "Types used by criterium")


;;; Java Management interface
(defprotocol StateChanged
  "Interrogation of differences in a state."
  (state-changed?
   [state]
   "Check to see if a state delta represents no change")
  (state-delta
   [state-1 state-2]
   "Return a state object for the difference between two states"))


(defrecord JvmClassLoaderState
    [loaded-count unloaded-count]
  StateChanged
  (state-changed?
   [state]
   (not (and (zero? (:loaded-count state)) (zero? (:unloaded-count state)))))
  (state-delta
   [state-1 state-2]
   (let [vals (map - (vals state-1) (vals state-2))]
     (JvmClassLoaderState. (first vals) (second vals)))))


(defrecord JvmCompilationState
    [compilation-time]
  StateChanged
  (state-changed?
   [state]
   (not (zero? (:compilation-time state))))
  (state-delta
   [state-1 state-2]
   (let [vals (map - (vals state-1) (vals state-2))]
     (JvmCompilationState. (first vals)))))
