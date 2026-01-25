package criterium.transducer.interfaces;

import clojure.lang.IFn;

/**
 * Interface for reducible sources that produce double values.
 *
 * <p>Supports reduction with a primitive double accumulator, avoiding boxing
 * overhead during high-performance benchmarking operations.
 */
public interface IDDDReducible {
  /**
   * Reduces over elements with a primitive double accumulator.
   *
   * @param f a function taking (double acc, double elem) and returning double
   * @param init the initial accumulator value
   * @return the final accumulated value
   */
  double reduce(IFn.DDD f, double init);
}
