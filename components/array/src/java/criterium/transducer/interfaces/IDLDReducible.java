package criterium.transducer.interfaces;

import clojure.lang.IFn;

/**
 * Interface for reducible sources that produce long values with a double accumulator.
 *
 * <p>Supports cross-type reduction: iterating over long elements while accumulating
 * a double result. The reducing function receives (double acc, long elem) and
 * returns double.
 *
 * <p>Use case: Computing statistics over counts (longs) that produce floating-point
 * results, such as Knuth histogram log-posterior calculations.
 */
public interface IDLDReducible {
  /**
   * Reduces over long elements with a primitive double accumulator.
   *
   * @param f a function taking (double acc, long elem) and returning double
   * @param init the initial accumulator value
   * @return the final accumulated value
   */
  double reduce(IFn.DLD f, double init);
}
