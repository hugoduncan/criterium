package criterium.transducer.types;

import clojure.lang.IFn;

/**
 * Interface for reducible sources that produce long values.
 *
 * <p>Supports reduction with a primitive long accumulator, avoiding boxing
 * overhead during high-performance benchmarking operations.
 */
public interface ILLLReducible {
  /**
   * Reduces over elements with a primitive long accumulator.
   *
   * @param f a function taking (long acc, long elem) and returning long
   * @param init the initial accumulator value
   * @return the final accumulated value
   */
  long reduce(IFn.LLL f, long init);
}
