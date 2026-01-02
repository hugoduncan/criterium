(ns r-validation.r
  "R connection helper for validation tests.

  Provides functions to connect to R via Rserve and gracefully skip tests
  when R is unavailable. Uses clojisr for R interop.

  The clojisr library is loaded lazily to prevent connection errors at
  namespace load time when R/Rserve is not available."
  (:require
   [clojure.string :as str]))

(def ^:private connection-state
  "Holds the R connection state: :untested, :available, or :unavailable"
  (atom :untested))

(def ^:private unavailable-reason
  "Holds the reason R is unavailable when connection fails"
  (atom nil))

(defn- require-clojisr!
  "Lazily require clojisr namespace. Returns true if successful."
  []
  (try
    (require 'clojisr.v1.r)
    true
    (catch Exception e
      (reset! unavailable-reason
              (str "Failed to load clojisr: " (.getMessage e)))
      false)))

(defn r-available?
  "Check if R/Rserve is available.

  Returns true if R can be connected to, false otherwise.
  Caches the result after first check."
  []
  (case @connection-state
    :available true
    :unavailable false
    :untested
    (if (require-clojisr!)
      (try
        (let [init-fn (requiring-resolve 'clojisr.v1.r/init)]
          (init-fn)
          (reset! connection-state :available)
          true)
        (catch Exception e
          (reset! connection-state :unavailable)
          (reset! unavailable-reason (.getMessage e))
          false))
      (do
        (reset! connection-state :unavailable)
        false))))

(defn unavailable-reason-msg
  "Returns the reason R is unavailable, or nil if available."
  []
  @unavailable-reason)

(defn ensure-r!
  "Ensures R is available.

  Throws ex-info with :type :r-unavailable if R cannot be connected to.
  Use this at the start of validation tests."
  []
  (when-not (r-available?)
    (throw (ex-info "R/Rserve not available"
                    {:type :r-unavailable
                     :reason @unavailable-reason}))))

(defn skip-when-unavailable
  "Returns a skip message if R is unavailable, nil otherwise.

  Use with clojure.test metadata or conditional test execution."
  []
  (when-not (r-available?)
    (str "Skipping: R/Rserve not available"
         (when-let [reason @unavailable-reason]
           (str " (" reason ")")))))

(defn r-eval
  "Evaluate R code and return the result as Clojure data.

  Takes an R expression as a quoted form and returns the result
  converted to Clojure data structures.

  Throws if R is not available."
  [expr]
  (ensure-r!)
  (let [r-fn (requiring-resolve 'clojisr.v1.r/r)
        r->clj-fn (requiring-resolve 'clojisr.v1.r/r->clj)]
    (r->clj-fn (r-fn expr))))

(defn r-eval-raw
  "Evaluate R code and return the raw R object.

  Use when you need to pass the result to further R operations.
  Throws if R is not available."
  [expr]
  (ensure-r!)
  (let [r-fn (requiring-resolve 'clojisr.v1.r/r)]
    (r-fn expr)))

(defn cleanup!
  "Clean up R session resources.

  Call this after validation tests complete to properly close
  the R connection."
  []
  (when (= :available @connection-state)
    (try
      (let [discard-fn (requiring-resolve 'clojisr.v1.r/discard-default-session)]
        (discard-fn))
      (catch Exception _
        nil))))

(defmacro with-r
  "Execute body with R available, or skip with message.

  If R is unavailable, prints skip message and returns nil.
  Otherwise executes body with R session available.

  Example:
    (with-r
      (is (= 3.0 (r-eval '(+ 1 2)))))"
  [& body]
  `(if-let [skip-msg# (skip-when-unavailable)]
     (do
       (println skip-msg#)
       nil)
     (do ~@body)))

(defn vec->r-str
  "Convert a Clojure vector of numbers to R's c() syntax.

  Returns a string like \"c(1, 2, 3)\" that can be used in R expressions."
  [v]
  (str "c(" (str/join ", " v) ")"))
