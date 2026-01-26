package criterium.transducer.interfaces;

import clojure.lang.IFn;

/**
 * Interface for reducible double sources with cross-type object accumulator.
 *
 * <p>Supports reduction where double elements are transformed to long values
 * by a transducer, then accumulated into an object result. The transducer
 * creates an ODO wrapper that internally converts double→long then calls
 * the OLO final reducing function.
 *
 * <p>Use case: Computing bin indices from double values and accumulating counts
 * into an array, such as histogram binning in Knuth's algorithm.
 */
public interface IODLOReducible {
  /**
   * Reduces over double elements into an object accumulator via cross-type transducer.
   *
   * <p>The source iterates over double elements. The transducer-wrapped function
   * receives doubles, converts them to longs internally, and accumulates into
   * the object result.
   *
   * @param f a transducer-wrapped function taking (Object acc, double elem)
   * @param init the initial accumulator value
   * @return the final accumulated value
   */
  Object reduceDouble(IFn.ODO f, Object init);
}
