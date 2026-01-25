package criterium.array.interfaces;

/**
 * Marker interface for long array wrappers.
 *
 * <p>Extends ITypedArray to indicate the array contains long primitives.
 * This enables type-safe dispatch in transducer and reduction operations.
 */
public interface ILongArray extends ITypedArray {
}
