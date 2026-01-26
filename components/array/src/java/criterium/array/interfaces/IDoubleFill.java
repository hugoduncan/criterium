package criterium.array.interfaces;

/**
 * Interface for arrays that support filling with a double value.
 */
public interface IDoubleFill {
  /**
   * Fills the array with the specified value.
   *
   * @param value the value to fill with
   * @return this array for chaining
   */
  IDoubleFill dfill(double value);
}
