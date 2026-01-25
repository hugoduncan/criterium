package criterium.array.interfaces;

import clojure.lang.Keyword;

/**
 * Base interface for typed array wrappers.
 *
 * <p>Provides access to element type and length information for arrays
 * that wrap primitive Java arrays while avoiding boxing overhead.
 */
public interface ITypedArray {
  /**
   * Returns the element type of the array as a keyword.
   *
   * @return the keyword representing the element type (:double, :long, or :object)
   */
  Keyword elemType();

  /**
   * Returns the number of elements in the array.
   *
   * @return the array length
   */
  long length();
}
