package criterium.array.interfaces;

/**
 * Interface for arrays that support filling with an Object value.
 */
public interface IObjectFill {
  /**
   * Fills the array with the specified value.
   *
   * @param value the value to fill with
   * @return this array for chaining
   */
  IObjectFill ofill(Object value);
}
