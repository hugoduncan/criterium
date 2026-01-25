(ns criterium.transducer.interface
  "Interfaces for primitive transducer operations.

  These interfaces are defined in Java to support proper inheritance
  hierarchies that eliminate reflection warnings. Clojure's definterface
  cannot express interface extension.

  Interface hierarchy:
  - ILLLReducible: reduce with primitive long accumulator
  - IDDDReducible: reduce with primitive double accumulator
  - IOLOReducible: reduce long elements into array
  - IODOReducible: reduce double elements into array
  - ILongReducible: extends ILLLReducible, IOLOReducible
  - IDoubleReducible: extends IDDDReducible, IODOReducible
  - IPrimOps: transduce/reduce/into/range operations"
  (:import
   [criterium.transducer.types
    IDDDReducible
    IDoubleReducible
    ILLLReducible
    ILongReducible
    IODOReducible
    IOLOReducible
    IPrimOps]))
