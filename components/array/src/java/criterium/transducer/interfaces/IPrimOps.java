package criterium.transducer.interfaces;

import criterium.array.interfaces.IDoubleArray;
import criterium.array.interfaces.IDoubleFill;
import criterium.array.interfaces.ILongArray;
import criterium.array.interfaces.ILongFill;
import criterium.array.interfaces.IObjectFill;

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
   * Transduces over a long source with a primitive double accumulator.
   *
   * <p>Enables cross-type reduction: long elements transformed and accumulated
   * into a double result. Used for computing floating-point statistics over
   * integer counts.
   *
   * @param xform the transducer (must produce DLD reducing function)
   * @param rf the reducing function taking (double acc, double elem)
   * @param init the initial accumulator value
   * @param source the reducible long source
   * @return the final accumulated value
   */
  double transduce(Object xform, Object rf, double init, IDLDReducible source);

  /**
   * Transduces over a double source with a primitive long accumulator.
   *
   * <p>Enables cross-type reduction: double elements transformed and accumulated
   * into a long result. Used for counting or classifying double values.
   *
   * @param xform the transducer (must produce LDL reducing function)
   * @param rf the reducing function taking (long acc, long elem)
   * @param init the initial accumulator value
   * @param source the reducible double source
   * @return the final accumulated value
   */
  long transduce(Object xform, Object rf, long init, ILDLReducible source);

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
   * Transduces over a double source with cross-type object accumulator.
   *
   * <p>Enables cross-type reduction: double elements transformed to long values
   * by the transducer, accumulated into an object result. Used for computing
   * bin indices from double values and accumulating into count arrays.
   *
   * @param xform the transducer (must produce OLO reducing function via cross-map)
   * @param rf the reducing function taking (Object acc, long elem)
   * @param init the initial accumulator value
   * @param source the reducible double source
   * @return the final accumulated value
   */
  Object transduce(Object xform, Object rf, Object init, IODLOReducible source);

  /**
   * Transduces over a long source with cross-type object accumulator.
   *
   * <p>Enables cross-type reduction: long elements transformed to double values
   * by the transducer, accumulated into an object result. Used for computing
   * floating-point statistics from integer counts.
   *
   * @param xform the transducer (must produce ODO reducing function via cross-map)
   * @param rf the reducing function taking (Object acc, double elem)
   * @param init the initial accumulator value
   * @param source the reducible long source
   * @return the final accumulated value
   */
  Object transduce(Object xform, Object rf, Object init, IOLDOReducible source);

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
   * Reduces over a double source with an object accumulator.
   *
   * <p>Enables reduction where double elements are accumulated into an object
   * result. Used for computing statistics with mutable accumulators.
   *
   * @param rf the reducing function taking (Object acc, double elem)
   * @param init the initial accumulator value
   * @param source the reducible double source
   * @return the final accumulated value
   */
  Object reduce(Object rf, Object init, IODLOReducible source);

  /**
   * Reduces over a long source with an object accumulator.
   *
   * <p>Enables reduction where long elements are accumulated into an object
   * result. Used for computing statistics with mutable accumulators.
   *
   * @param rf the reducing function taking (Object acc, long elem)
   * @param init the initial accumulator value
   * @param source the reducible long source
   * @return the final accumulated value
   */
  Object reduce(Object rf, Object init, IOLDOReducible source);

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

  /**
   * Fills a double array with the specified value.
   *
   * @param arr the array to fill
   * @param value the value to fill with
   * @return the filled array
   */
  IDoubleFill fill(IDoubleFill arr, double value);

  /**
   * Fills a long array with the specified value.
   *
   * @param arr the array to fill
   * @param value the value to fill with
   * @return the filled array
   */
  ILongFill fill(ILongFill arr, long value);

  /**
   * Fills an object array with the specified value.
   *
   * @param arr the array to fill
   * @param value the value to fill with
   * @return the filled array
   */
  IObjectFill fill(IObjectFill arr, Object value);
}
