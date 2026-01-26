package criterium.array.interfaces;

/**
 * Interface for arrays that support filling with a long value.
 */
public interface ILongFill {
  /**
   * Fills the array with the specified value.
   *
   * @param value the value to fill with
   * @return this array for chaining
   */
  ILongFill lfill(long value);
}
