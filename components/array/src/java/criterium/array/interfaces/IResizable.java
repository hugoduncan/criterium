package criterium.array.interfaces;

/**
 * Interface for resizable array wrappers with a fixed capacity.
 *
 * <p>Enables arrays to be constructed with a maximum capacity and resized
 * down to a smaller size without reallocation. Useful for algorithms where
 * the final size isn't known upfront but has a known upper bound.
 *
 * <p>Resizing only shrinks; attempting to grow beyond capacity throws.
 */
public interface IResizable {
  /**
   * Resizes the array to a new size.
   *
   * <p>The new size must be between 0 and the capacity (inclusive).
   * This operation does not reallocate the underlying array.
   *
   * @param newSize the new size, must be 0 <= newSize <= capacity()
   * @return the new size
   * @throws IllegalArgumentException if newSize is negative or exceeds capacity
   */
  long resize(long newSize);

  /**
   * Returns the maximum capacity of the array.
   *
   * <p>This is the size the array was allocated with and represents
   * the upper bound for resize operations.
   *
   * @return the maximum capacity
   */
  long capacity();
}
