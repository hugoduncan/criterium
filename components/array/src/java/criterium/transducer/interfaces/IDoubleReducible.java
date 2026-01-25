package criterium.transducer.interfaces;

/**
 * Composite interface for double-based reducible sources.
 *
 * <p>Extends both IDDDReducible and IODOReducible, enabling types that
 * implement this interface to be reduced with either a primitive double
 * accumulator or into a double array.
 *
 * <p>This inheritance hierarchy eliminates reflection warnings that occur
 * when Clojure's definterface is used, as definterface cannot express
 * interface extension.
 */
public interface IDoubleReducible extends IDDDReducible, IODOReducible {
}
