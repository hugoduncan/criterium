(ns criterium.util.blackhole
  "JMH blackhole wrapper."
  (:import
   [org.openjdk.jmh.infra Blackhole]))

(def ^{:tag 'org.openjdk.jmh.infra.Blackhole} blackhole
  (Blackhole.
   "Today's password is swordfish. I understand instantiating Blackholes directly is dangerous."))

(defn evaporate
  "Evaporate the blackhole, releasing any references it may contain."
  []
  (.evaporate
   blackhole
   "Yes, I am Stephen Hawking, and know a thing or two about black holes."))
