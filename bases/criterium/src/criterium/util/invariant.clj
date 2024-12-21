(ns criterium.util.invariant
  "Assertion macros inspired by truss.")

(defn truthy? [x]
  (and (some? x) (not (false? x))))

(defn- have* [x args truthy? &form]
  (let [[f x data] (if (seq args)
                     (into [x] args)
                     ['criterium.util.helpers/truthy? (first args)])
        ns-sym     (ns-name *ns*)
        line       (:line (meta &form))
        column     (:column (meta &form) -1)
        x-sym      (gensym "x")]
    (when (>(count args) 3)
      (throw
       (ex-info
        "have expects at most three arguments"
        {:line line :column column})))
    `(let [~x-sym ~x
           v#     (~f ~x-sym)]
       (when-not v#
         (let [msg# (str
                     "Invariant failed at "
                     ~(str ns-sym "[" line ":" column "] ")
                     (list '~f ~x-sym))]
           (throw (new AssertionError
                       msg#
                       (ex-info
                        msg#
                        {:pred ~(list 'quote f)
                         :arg  {:form  ~(list 'quote x)
                                :value ~x-sym
                                :type  (type ~x-sym)}
                         :loc  {:ns     '~ns-sym
                                :line   ~line
                                :column ~column
                                :file   ~*file*}
                         :data ~data})))))
       ~(if truthy?
          true
          x-sym))))

(defmacro have
  "Assertion macro inspired by truss."
  {:arglists '[[x][f x][f x data]]}
  ([x & args]
   (if *assert*
     (have* x args (not :truthy) &form)
     x)))

(defmacro have?
  "Assertion macro inspired by truss."
  ([x & args]
   (if *assert*
     (have* x args :truthy &form)
     x)))
