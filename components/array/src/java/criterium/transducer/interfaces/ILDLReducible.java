package criterium.transducer.interfaces;

import clojure.lang.IFn;

/**
 * Interface for reducible sources that produce double values with a long accumulator.
 *
 * <p>Supports cross-type reduction: iterating over double elements while accumulating
 * a long result. The reducing function receives (long acc, double elem) and
 * returns long.
 *
 * <p>Use case: Counting or classifying double values into discrete categories.
 */
public interface ILDLReducible {
  /**
   * Reduces over double elements with a primitive long accumulator.
   *
   * @param f a function taking (long acc, double elem) and returning long
   * @param init the initial accumulator value
   * @return the final accumulated value
   */
  long reduce(IFn.LDL f, long init);
}
