(ns criterium.agent.core
  "Native agent api implementation."
  (:import
   [com.sun.tools.attach
    VirtualMachine]
   [criterium.agent
    Agent]
   [java.lang.management
    ManagementFactory]))

;;; Native Agent

(def ^:internal records
  "Allocation tracing result."
  (atom []))

(defn- data-fn
  "Callback function for native agent."
  ([object]
   (cond
     (instance? criterium.agent.Allocation object)
     (let [a ^criterium.agent.Allocation object]
       (when (and (not= (.call_method a) "allocation_start_marker")
                  (not= (.call_method a) "allocation_finish_marker"))
         (swap! records conj
                {:object-type  (.object_type a)
                 :object_size  (.object_size a)
                 :call-class   (.call_class a)
                 :call-method  (.call_method a)
                 :call-file    (.call_file a)
                 :call-line    (.call_line a)
                 :alloc-class  (.alloc_class a)
                 :alloc-method (.alloc_method a)
                 :alloc-file   (.alloc_file a)
                 :alloc-line   (.alloc_line a)
                 :thread       (.thread a)
                 :freed        (.freed a)})))
     :else
     (prn :received object (type object))))
  ([a b c d e f g h]
   (when (and (not= c "allocation_start_marker")
              (not= c "allocation_finish_marker"))
     (swap! records conj
            {:object-type a
             :call-class  b
             :call-method c
             :file        d
             :size        (Long/parseLong e)
             :thread      (Long/parseLong f)
             :line        (Long/parseLong g)
             :freed       (Long/parseLong h)}))))

(Agent/set_handler data-fn);

(def ^:private commands
  {:ping                      0
   :sync-state                1
   :start-allocation-tracing  10
   :stop-allocation-tracing   11
   :report-allocation-tracing 12})

(def ^:private states
  {-1 :not-attached
   0  :passive
   10 :allocation-tracing-starting
   11 :allocation-tracing-active
   15 :allocation-tracing-stopping
   16 :allocation-tracing-flushing
   17 :allocation-tracing-flushed})

(defn ^:internal agent-command [cmd]
  (Agent/command (commands cmd)))

;; (let [klass  Agent
;;       handle (.findStatic
;;               (java.lang.invoke.MethodHandles/publicLookup)
;;               Agent
;;               "getState"
;;               ;; Long/TYPE
;;               (java.lang.invoke.MethodType/fromMethodDescriptorString
;;                "()J"
;;                (.getContextClassLoader (Thread/currentThread)))
;;               )
;;       args   (make-array Object 0)]

;;   (def h handle)

;;   (let [v ^long (.invoke  ^java.lang.invoke.DirectMethodHandle$StaticAccessor
;;                           h nil)]
;;     v)

;;   (defn ^:internal agent-state []
;;     (prn :val (.invoke handle args))
;;     (states
;;      (.invoke handle args)
;;      ;;(. state klass)
;;      )))

;; (let [agent Agent]
;;   (defn ^:internal agent-state []
;;     (states (. agent getState)
;;             ;;(. state klass)
;;             )))

;; Direct linking is used here to avoid var lookups, which can cause garbage
;; which we want to avoid in the sample collection path.
(binding [*compiler-options* (assoc *compiler-options* :direct-linking true)]

  (defn ^:internal agent-state** ^long [agent]
    (. ^Agent agent getState)
    ;;(. state klass)
    )

  (def agent-state* (partial agent-state** (new Agent)))

  (defn ^:internal agent-state []
    (get states (agent-state*)))

  (comment
    (agent-command :ping))

  (dotimes [_ 1000] (agent-state))

  (defn allocation-start-marker []
    (Agent/allocation_start_marker))

  (defn allocation-tracing-active?
    []
    (= (agent-state) :allocation-tracing-active))

  (defn attached?
    []
    (not= (agent-state) :not-attached))

  (defn ^:internal allocation-tracing-start! []
    (agent-command :start-allocation-tracing)
    (assert (= (agent-state) :allocation-tracing-starting))
    ;; (make-array Object (* 512 1024))             ; flush this
    (System/gc)
    (System/gc)
    (System/gc)
    (allocation-start-marker)
    (loop [i 1000000]
      (allocation-start-marker)
      (when (and (pos? i) (not (allocation-tracing-active?)))
        (recur (unchecked-dec i))))
    (when (not= (agent-state) :allocation-tracing-active)
      (println "Warning allocation tracing failed to start promptly")))

  (defn ^:internal allocation-tracing-stop! []
    (agent-command :stop-allocation-tracing)
    (Agent/allocation_finish_marker)
    ;; (Agent/allocation_marker)
    (System/gc)
    (loop [i 1000000]
      (when (and (pos? i)
                 (do
                   (agent-command :sync-state)
                   (not= (agent-state) :allocation-tracing-flushed)))
        ;; (when (zero? ^long (mod i 100))
        ;;   (System/gc))
        (Agent/allocation_finish_marker)
        (System/gc)
        (recur (unchecked-dec i))))
    (when (not= (agent-state) :allocation-tracing-flushed)
      (println "Warning allocation tracing failed to stop promptly"))))

(defn with-allocation-tracing-enabled [body]
  `(let [res# (try
                (allocation-tracing-start!)
                ~@body
                (finally
                  (allocation-tracing-stop!)))]
     (reset! records [])
     (agent-command :report-allocation-tracing)
     [@records res#]))

(defn with-allocation-tracing-disabled [body]
  `[nil (do ~@body)])

;; (trace-allocation)

(defn allocation-on-thread?
  [thread-id]
  (fn allocation-on-thread?
    [record]
    (= thread-id (:thread record))))

(defn allocation-freed?
  "predicate for the allocation record having been freed."
  [record]
  (pos? (long (:freed record))))

(defn allocations-summary [records]
  (let [freed (filterv allocation-freed? records)]
    {:num-allocated   (count records)
     :num-freed       (count freed)
     :allocated-bytes (reduce + (map :object_size records))
     :freed-bytes     (reduce + (map :object_size freed))}))

;;; Object Size Agent

(comment
  ;; TODO re-enable these
  (defn pid
    "Return the PID of the current JVM.
    This may not work on all JVM/OS instances."
    []
    (re-find #"\d+" (.getName (ManagementFactory/getRuntimeMXBean))))

  (defn- load-agent*
    "Attach the javaagent from a jar file to the current JVM."
    [^String jar-path]
    (.loadAgent (VirtualMachine/attach (pid)) jar-path))

  (defn- find-jar
    "Find the agent jar"
    ;; TODO use tools.deps.alpha
    []
    "criterium-agent.jar")

  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (defn load-agent
    "Load the agent"
    []
    (load-agent* (find-jar)))

  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (defn object-size
    "Return the approximate size of an object in bytes."
    ^long [_x]
    #_(agent/object-size x)
    0))
