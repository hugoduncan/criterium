package criterium.agent;

public class Allocation {
  public final String object_type;
  public final long object_size;

  public final String call_class;
  public final String call_method;
  public final String call_file;
  public final long call_line;

  public final String alloc_class;
  public final String alloc_method;
  public final String alloc_file;
  public final long alloc_line;

  public final long thread;
  public final long freed;

  public Allocation(String object_type,
                    long object_size,
                    String call_class,
                    String call_method,
                    String call_file,
                    long call_line,
                    String alloc_class,
                    String alloc_method,
                    String alloc_file,
                    long alloc_line,
                    long thread,
                    long freed) {
    this.object_type = object_type;
    this.object_size = object_size;
    this.call_class = call_class;
    this.call_method = call_method;
    this.call_file = call_file;
    this.call_line = call_line;
    this.alloc_class = alloc_class;
    this.alloc_method = alloc_method;
    this.alloc_file = alloc_file;
    this.alloc_line = alloc_line;
    this.thread = thread;
    this.freed = freed;
  }
}
