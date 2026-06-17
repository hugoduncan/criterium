(ns criterium.utils.forms)

(def ^:no-doc max-unrolled-arity
  "Highest arity for which `unrolled-apply-form` / `unrolled-invoke` emit a
  direct call.  Higher arities fall back to `apply`."
  20)

(defn ^:no-doc unrolled-apply-form
  "Return a form that invokes `f-sym` with the elements of the collection
  bound to `args-sym`.

  Dispatches on the argument count so common arities are called directly
  (e.g. `(f (nth args 0) (nth args 1))`) rather than via `apply`, which
  allocates a seq on every call.  Arities above `max-unrolled-arity` fall back
  to `apply`.

  `f-sym` must be a symbol (it is repeated across the dispatch arms), so the
  caller should bind the function to a local first.  `args-sym` must evaluate
  to an indexed, counted collection (e.g. a vector)."
  [f-sym args-sym]
  `(case (count ~args-sym)
     ~@(mapcat
        (fn [n]
          [n (cons f-sym
                   (map (fn [i] `(nth ~args-sym ~i)) (range n)))])
        (range (inc (long max-unrolled-arity))))
     (apply ~f-sym ~args-sym)))

(defmacro unrolled-invoke
  "Invoke `f` with the elements of the `args` collection, unrolled by argument
  count to avoid the per-call seq allocation of `apply`.

  Arities above `max-unrolled-arity` fall back to `apply`.  `f` and `args` are
  each evaluated once.  `args` must be an indexed, counted collection."
  [f args]
  (let [f-sym    (gensym "f")
        args-sym (gensym "args")]
    `(let [~f-sym    ~f
           ~args-sym ~args]
       ~(unrolled-apply-form f-sym args-sym))))

(defmacro cond*
  "A cond variant that allows :let bindings visible to subsequent clauses.

  Example:
     (cond*
       (foo?) (handle-foo)
       :let [a 5]
       (> a 3) (handle-big a)
       :let [b (+ a 1)]
       (bar? b) (handle-bar b)
       :else (default-handler a b))"
  {:style/indent 1}
  [& clauses]
  (letfn [(process [clauses]
            (when (seq clauses)
              (let [guard     (first clauses)
                    remaining (next clauses)
                    body      (first remaining)]
                (when-not remaining
                  (throw
                   (IllegalArgumentException.
                    "cond* requires an even number of forms")))
                (if (= :let guard)
                  (if (vector? body)
                    `(let ~body
                       ~(process (next remaining)))
                    (throw
                     (IllegalArgumentException.
                      "cond* :let requires a binding vector")))
                  `(if ~guard ~body ~(process (next remaining)))))))]
    (process clauses)))
