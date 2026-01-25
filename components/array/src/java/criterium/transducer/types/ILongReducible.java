package criterium.transducer.types;

/**
 * Composite interface for long-based reducible sources.
 *
 * <p>Extends both ILLLReducible and IOLOReducible, enabling types that
 * implement this interface to be reduced with either a primitive long
 * accumulator or into a long array.
 *
 * <p>This inheritance hierarchy eliminates reflection warnings that occur
 * when Clojure's definterface is used, as definterface cannot express
 * interface extension.
 */
public interface ILongReducible extends ILLLReducible, IOLOReducible {
}
