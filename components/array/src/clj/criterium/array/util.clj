(ns criterium.array.util
  "Utilities for defining typed array interfaces.

  Provides `definterface+`, an extension of Clojure's `definterface` that
  supports interface inheritance via `gen-interface`'s `:extends` option.")

(defmacro definterface+
  "Create a new Java interface, optionally extending other interfaces.

  Like `definterface`, but accepts an optional vector of interfaces to extend
  as the first form after the name.

  Examples:
    ;; Without extends (same as definterface)
    (definterface+ IFoo
      (^long bar [^double x]))

    ;; With single extend
    (definterface+ IBar [IFoo]
      (^double qux [^long y]))

    ;; With multiple extends
    (definterface+ IBaz [IFoo IBar]
      (baz []))"
  {:added    "0.5"
   :arglists '([name & sigs]
               [name [extends...] & sigs])}
  [name & forms]
  (let [[extends sigs] (if (and (seq forms) (vector? (first forms)))
                         [(first forms) (rest forms)]
                         [nil forms])
        tag            (fn [x] (or (:tag (meta x)) Object))
        psig           (fn [[mname [& args]]]
                         (vector mname (vec (map tag args)) (tag mname) (map meta args)))
        cname          (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))]
    `(do
       (gen-interface
        :name ~cname
        ~@(when extends [:extends (vec (map resolve extends))])
        :methods ~(vec (map psig sigs)))
       (import ~cname))))
