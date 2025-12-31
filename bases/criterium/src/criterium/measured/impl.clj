(ns criterium.measured.impl
  (:require
   [clojure.set]
   [criterium.jvm :as jvm]
   [criterium.util.blackhole :as blackhole]
   [criterium.util.helpers :as util]))

(defrecord Measured
  [^clojure.lang.IFn args-fn
   ^clojure.lang.IFn f
   expr-fn])

(alter-meta! #'->Measured assoc :private true)
(alter-meta! #'map->Measured assoc :private true)

(defn measured?
  "Predicate for x being a Measured."
  [x]
  (instance? Measured x))

(defn measured
  "Return a Measured for a function that can be benchmarked.

  The Measured is the basic unit of measurement. A Measured consists of
  a state generation funtion and a function to be measured.  The
  function to be measured takes the result of calling the state function
  and an eval-count as arguments, ie. `(f (args-fn) eval-count)`, and
  returns an `[elapsed-time expr-value]` tuple.

  The state function is used to prevent constant folding for constant
  inputs.

  The eval-count allows usage where the function is a wrapper that
  evaluates the subject expression multiple times.

  expr-fn, if specified, returns a symbolic representation of the measured,
  for inspection purposes (unused internally).
  "
  ^Measured
  [args-fn
   f
   & [expr-fn]]
  (->Measured args-fn f expr-fn))

(defn- s-expression?
  "Predicate for expr being an S-expression."
  [expr]
  (or (list? expr) (instance? clojure.lang.Cons expr)))

(defrecord FnCallExpr
    ;; a representation of an s-expression
  [op ; the operand
   arg-syms ; arguments as symbols
   arg-vals ; the symbolic value of of the arg-syms
   metamap ; metadata on the FnCallExpr
   ])

(defn- fn-call-expr?
  "Predicate for x being an instance of a FnCallExpr"
  [x]
  (instance? FnCallExpr x))

(defn- gen-arg-sym
  "Generate a symbol for an argument."
  []
  (gensym "arg"))

(defn ^:no-doc form-print
  "Return a symbolic expression for the argument."
  [x]
  (cond
    (symbol? x) x

    (fn-call-expr? x)
    (with-meta
      `(~(:op x) ~@(mapv form-print (:arg-syms x)))
      (:metamap x))))

(defn ^:no-doc factor-form
  "Factor form, extracting constant expressions.
  env is the macro's &env, used to identify local bindings."
  [form env]
  (let [subj (first form)
        tform (fn [x]
                (if (and (s-expression? x) (= subj (first x)))
                  (let [op (first x)
                        ;; Check if operator is a local binding
                        op-is-local? (and (symbol? op)
                                          (contains? env op))
                        ;; If local, generate arg-sym for operator
                        [op-sym op-arg-vals] (if op-is-local?
                                               (let [sym (gen-arg-sym)]
                                                 [sym {sym op}])
                                               [op {}])]
                    (reduce
                     (fn [res arg]
                       (if (fn-call-expr? arg)
                         (-> res
                             (update :arg-syms conj arg)
                             (update :arg-vals merge (:arg-vals arg)))
                         (let [arg-sym (gen-arg-sym)]
                           (-> res
                               (update :arg-syms conj arg-sym)
                               (update :arg-vals assoc arg-sym arg)))))
                     (->FnCallExpr
                      op-sym
                      []
                      op-arg-vals
                      (meta x))
                     (rest x)))
                  x))
        res (util/postwalk
             tform
             form)]
    {:expr (form-print res)
     :arg-vals (:arg-vals res)}))

(defn ^:no-doc factor-const [expr]
  (let [arg-sym (gen-arg-sym)]
    {:expr arg-sym
     :arg-vals {arg-sym expr}}))

(defn ^:no-doc factor-expr
  "Factor an expression, extracting constant expressions and local bindings.
  env is the macro's &env, used to identify local bindings."
  [expr env]
  (if (s-expression? expr)
    (factor-form expr env)
    (factor-const expr)))

(defn ^:no-doc cast-fn
  "Return a cast function givent a tag."
  [tag]
  (when (and (symbol? tag)
             (#{'long 'int 'double 'float} tag))
    tag))

(defn ^:no-doc binding-with-hint-or-cast
  "Return a binding pair to type hint or cast values."
  [arg-sym arg-meta]
  (let [tag (:tag arg-meta)]
    (if-let [f (cast-fn tag)]
      [arg-sym (list f arg-sym)]
      [(with-meta arg-sym arg-meta) arg-sym])))

(defn ^:internal ^:no-doc measured-expr-fn
  "Construct a function expression to measure the given expr.
  Captures the expression arguments into a state function."
  [arg-syms expr {:keys [arg-metas time-fn]}]
  (let [eval-count-sym (gensym "eval-count")
        time-fn (when time-fn
                  (with-meta
                    time-fn
                    {:tag 'clojure.lang.IFn$L}))]
    `(fn ~'measured
       [~arg-syms
        ;; explicitly not tagged as 'long, since this function is invoked
        ;; non-literally, so the calling value will always be an object.
        ~eval-count-sym]
       (let [~@(mapcat binding-with-hint-or-cast arg-syms arg-metas)
             ;; primitive loop coounter.  Decrement since we evaluate
             ;; once outside the loop.
             ~(with-meta eval-count-sym {:tag 'long})
             ~eval-count-sym ; convert to 'long
             n# (long (unchecked-dec ~eval-count-sym))
             start# ~(if time-fn
                       `(. ~time-fn invokePrim)
                       `(jvm/timestamp))
             val# ~expr] ; evaluate once to get a return value
         (loop [i# n#]
           (when (pos? i#)
             (blackhole/consume ~expr)
             (recur (long (unchecked-dec i#)))))
         (let [finish# ~(if time-fn
                          `(. ~time-fn invokePrim)
                          `(jvm/timestamp))]
           (blackhole/evaporate)
           [(unchecked-subtract finish# start#) val#])))))

(defn ^:no-doc merge-metas
  "Merge two sequences of maps.
  Sequences may be of differing lengths.  The returned length is the
  largest of the two input lengths."
  [m1 m2]
  (let [l1 (count m1)
        l2 (count m2)]
    (into (mapv merge m1 m2)
          (if (>= l1 l2)
            (drop l2 m1)
            (drop l1 m2)))))

(def ^:private TYPE-NAME-CONVERSIONS
  ;; converting is good for measureds that use these values directly
  ;; but causes wrapping if the values are returned from the measured.
  {'java.lang.Long 'long
   'java.lang.Integer 'int
   'java.lang.Double 'double
   'java.lang.Float 'float})

(defn ^:no-doc type-name-conversion [t]
  (TYPE-NAME-CONVERSIONS t t))

(defn ^:no-doc tag-meta [^Class t]
  (when t
    (let [type-name (-> (.getCanonicalName ^Class t)
                        symbol
                        type-name-conversion)]
      {:tag type-name})))

(declare collect-symbols)

(defn ^:no-doc capture-arg-types
  "Return a sequence of metadata maps with :tag type hints for arg expressions.
  For local bindings, extracts type from LocalBinding in env or preserves
  user-provided type hints. For non-locals, uses eval to detect types."
  [arg-syms arg-vals local-arg-syms env]
  (mapv (fn [arg-sym]
          (if (contains? local-arg-syms arg-sym)
            (let [arg-val (get arg-vals arg-sym)]
              (or
               ;; First try user-provided hint on the expression
               (when-let [tag (:tag (meta arg-val))]
                 {:tag tag})
               ;; Then try to get type from LocalBinding if arg-val is a symbol
               (when (symbol? arg-val)
                 (when-let [^clojure.lang.Compiler$LocalBinding lb (get env arg-val)]
                   (when (.hasJavaClass lb)
                     (tag-meta (.getJavaClass lb)))))
               ;; For compound expressions, find locals and use their type if uniform
               (let [local-syms (filter #(contains? env %) (collect-symbols arg-val))
                     local-types (keep (fn [sym]
                                         (let [^clojure.lang.Compiler$LocalBinding lb (get env sym)]
                                           (when (and lb (.hasJavaClass lb))
                                             (.getJavaClass lb))))
                                       local-syms)]
                 (when (and (seq local-types)
                            (apply = local-types)
                            (#{Long/TYPE Double/TYPE} (first local-types)))
                   (tag-meta (first local-types))))))
            ;; For non-locals, eval to get the type
            (tag-meta (type (eval (get arg-vals arg-sym))))))
        arg-syms))

(defn ^:no-doc collect-symbols
  "Collect all symbols used in an expression.
  Returns a set of symbols."
  [expr]
  (cond
    (symbol? expr) #{expr}
    (coll? expr) (into #{} (mapcat collect-symbols) expr)
    :else #{}))

(defn ^:no-doc locals-in-expr
  "Identify which symbols in expr are local bindings.
  Returns a set of symbols that are present in env."
  [expr env]
  (if env
    (let [local-names (set (keys env))
          expr-syms (collect-symbols expr)]
      (clojure.set/intersection expr-syms local-names))
    #{}))

(defn ^:no-doc local-arg-val?
  "Check if an arg-val is or contains a local binding.
  Returns true if the value is a symbol present in env, or if it's
  an expression containing any symbols present in env."
  [arg-val env]
  (let [local-names (set (keys env))
        expr-syms (collect-symbols arg-val)]
    (boolean (seq (clojure.set/intersection expr-syms local-names)))))

(defn ^:no-doc identify-local-args
  "Identify which arg-vals are local bindings.
  Returns a set of arg-syms (gensymed keys) whose values are locals."
  [arg-vals env]
  (if env
    (->> arg-vals
         (filter (fn [[_arg-sym arg-val]] (local-arg-val? arg-val env)))
         (map first)
         set)
    #{}))

(defn measured-expr*
  "Return a measured function for the given expression.

  The arguments are converted into a vector, which is used as an
  argument to the a function that wraps the expression.

  Any expr that is not a List is treated as a constant.  This is mainly
  for internal benchmarking.

  The env parameter is the macro's &env, used to identify local bindings.
  Local bindings are captured at the call-site and passed through the
  measurement pipeline alongside hoisted constants."
  [expr options env]
  (let [{:keys [expr arg-vals] :as _f} (factor-expr expr env)
        arg-syms (keys arg-vals)
        local-arg-syms (identify-local-args arg-vals env)
        arg-metas (capture-arg-types
                   arg-syms arg-vals local-arg-syms env)
        options (update
                 options
                 :arg-metas merge-metas arg-metas)]
    `(measured
      (fn ~'measured-args [] ~(vec (vals arg-vals)))
      ~(measured-expr-fn
        (vec arg-syms)
        expr
        options)
      (fn ~'measured-expr []
        ~(list 'quote
               `(do (let [~@arg-vals]
                      (time ~expr))))))))

(defn measured-callable
  ([f]
   `(measured
     (fn ~'measured-args [])
     ~(measured-expr-fn
       []
       `(~f)
       {})
     (fn ~'measured-expr []
       ~(list 'quote
              `(time (~f))))))
  ([args-f f]
   (let [args (gensym "args")]
     `(measured
       (fn ~'measured-args [] (~args-f))
       ~(measured-expr-fn
         [args]
         `(apply ~f [~args])
         {})
       (fn ~'measured-expr []
         ~(list 'quote
                `(time (~f))))))))
