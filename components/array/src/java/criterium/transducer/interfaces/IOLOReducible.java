package criterium.transducer.interfaces;

import clojure.lang.IFn;
import criterium.array.interfaces.ILongArray;

/**
 * Interface for reducible sources that can reduce into a long array.
 *
 * <p>Supports reduction where primitive long values are accumulated into
 * an ILongArray, typically used for collecting benchmark samples.
 */
public interface IOLOReducible {
  /**
   * Reduces over long elements into an array accumulator.
   *
   * @param f a function taking (Object acc, long elem) and returning Object
   * @param init the initial array accumulator
   * @return the final accumulated array
   */
  ILongArray reduceLong(IFn.OLO f, ILongArray init);
}
