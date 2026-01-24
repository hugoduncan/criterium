(ns criterium.primitive-fn.interface)

(definterface DB
  (^boolean invokePrim [^double v]))

(definterface LB
  (^boolean invokePrim [^long v]))

(definterface DDB
  (^boolean invokePrim [^double a ^double b]))

(definterface LLB
  (^boolean invokePrim [^long a ^long b]))
