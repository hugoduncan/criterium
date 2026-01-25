package criterium.transducer.interfaces;

import criterium.array.interfaces.IDoubleArray;
import criterium.array.interfaces.ILongArray;

/**
 * Interface for primitive transducer operations.
 *
 * <p>Provides transduce, reduce, into, and range operations that work with
 * primitive types to avoid boxing overhead during benchmarking.
 */
public interface IPrimOps {
  /**
   * Transduces over a long source with a primitive long accumulator.
   *
   * @param xform the transducer
   * @param rf the reducing function
   * @param init the initial accumulator value
   * @param source the reducible source
   * @return the final accumulated value
   */
  long transduce(Object xform, Object rf, long init, ILLLReducible source);

  /**
   * Transduces over a double source with a primitive double accumulator.
   *
   * @param xform the transducer
   * @param rf the reducing function
   * @param init the initial accumulator value
   * @param source the reducible source
   * @return the final accumulated value
   */
  double transduce(Object xform, Object rf, double init, IDDDReducible source);

  /**
   * Transduces over a long source into a long array.
   *
   * @param xform the transducer
   * @param rf the reducing function
   * @param init the initial array accumulator
   * @param source the reducible source
   * @return the final accumulated array
   */
  ILongArray transduce(Object xform, Object rf, ILongArray init, IOLOReducible source);

  /**
   * Transduces over a double source into a double array.
   *
   * @param xform the transducer
   * @param rf the reducing function
   * @param init the initial array accumulator
   * @param source the reducible source
   * @return the final accumulated array
   */
  IDoubleArray transduce(Object xform, Object rf, IDoubleArray init, IODOReducible source);

  /**
   * Reduces over a long source with a primitive long accumulator.
   *
   * @param rf the reducing function
   * @param init the initial accumulator value
   * @param source the reducible source
   * @return the final accumulated value
   */
  long reduce(Object rf, long init, ILLLReducible source);

  /**
   * Reduces over a double source with a primitive double accumulator.
   *
   * @param rf the reducing function
   * @param init the initial accumulator value
   * @param source the reducible source
   * @return the final accumulated value
   */
  double reduce(Object rf, double init, IDDDReducible source);

  /**
   * Transduces elements from source into target long array.
   *
   * @param target the target array to populate
   * @param xform the transducer
   * @param source the reducible source
   * @return the populated target array
   */
  ILongArray into(ILongArray target, Object xform, IOLOReducible source);

  /**
   * Transduces elements from source into target double array.
   *
   * @param target the target array to populate
   * @param xform the transducer
   * @param source the reducible source
   * @return the populated target array
   */
  IDoubleArray into(IDoubleArray target, Object xform, IODOReducible source);

  /**
   * Creates a long range from start (inclusive) to end (exclusive).
   *
   * @param start the start value (inclusive)
   * @param end the end value (exclusive)
   * @return a reducible long range
   */
  ILongReducible range(long start, long end);

  /**
   * Creates a double range from start (inclusive) to end (exclusive) with step.
   *
   * @param start the start value (inclusive)
   * @param end the end value (exclusive)
   * @param step the step size
   * @return a reducible double range
   */
  IDoubleReducible range(double start, double end, double step);
}
