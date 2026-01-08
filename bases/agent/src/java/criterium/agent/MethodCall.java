package criterium.agent;

/**
 * Represents a node in the method call tree.
 *
 * Each node captures information about a method that was called during
 * tracing, including its location, call count, and child method calls.
 * The tree structure mirrors the call hierarchy observed during execution.
 */
public class MethodCall {
  /** JVM class signature, e.g., "Lmyapp/Core;" */
  public final String class_name;

  /** Method name, e.g., "process" */
  public final String method_name;

  /** Source file name, e.g., "Core.java", or null if unknown */
  public final String source_file;

  /** Line number in source, or -1 if unknown */
  public final long line_number;

  /** Number of times this method was called at this tree position */
  public final long call_count;

  /** Child method calls made from this method */
  public final MethodCall[] children;

  public MethodCall(String class_name,
                    String method_name,
                    String source_file,
                    long line_number,
                    long call_count,
                    MethodCall[] children) {
    this.class_name = class_name;
    this.method_name = method_name;
    this.source_file = source_file;
    this.line_number = line_number;
    this.call_count = call_count;
    this.children = children;
  }
}
