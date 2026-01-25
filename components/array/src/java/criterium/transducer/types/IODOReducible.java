package criterium.transducer.types;

import clojure.lang.IFn;
import criterium.array.types.IDoubleArray;

/**
 * Interface for reducible sources that can reduce into a double array.
 *
 * <p>Supports reduction where primitive double values are accumulated into
 * an IDoubleArray, typically used for collecting benchmark samples.
 */
public interface IODOReducible {
  /**
   * Reduces over double elements into an array accumulator.
   *
   * @param f a function taking (Object acc, double elem) and returning Object
   * @param init the initial array accumulator
   * @return the final accumulated array
   */
  IDoubleArray reduceDouble(IFn.ODO f, IDoubleArray init);
}
