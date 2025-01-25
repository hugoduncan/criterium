(ns criterium.util.forms)

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
