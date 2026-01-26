package criterium.transducer.interfaces;

import clojure.lang.IFn;

/**
 * Interface for reducible long sources with cross-type object accumulator.
 *
 * <p>Supports reduction where long elements are transformed to double values
 * by a transducer, then accumulated into an object result. The transducer
 * creates an OLO wrapper that internally converts long→double then calls
 * the ODO final reducing function.
 *
 * <p>Use case: Computing floating-point statistics from integer counts and
 * accumulating results into an object, such as variance calculations.
 */
public interface IOLDOReducible {
  /**
   * Reduces over long elements into an object accumulator via cross-type transducer.
   *
   * <p>The source iterates over long elements. The transducer-wrapped function
   * receives longs, converts them to doubles internally, and accumulates into
   * the object result.
   *
   * @param f a transducer-wrapped function taking (Object acc, long elem)
   * @param init the initial accumulator value
   * @return the final accumulated value
   */
  Object reduceLong(IFn.OLO f, Object init);
}
