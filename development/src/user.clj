(ns user)

(alter-var-root #'*unchecked-math* (constantly :warn-on-boxed))
(alter-var-root #'*warn-on-reflection* (constantly true))
