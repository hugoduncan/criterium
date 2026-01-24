(ns criterium.primitive-fn.interface)

(definterface DB
  (^boolean invokePrim [^double v]))

(definterface LB
  (^boolean invokePrim [^long v]))
