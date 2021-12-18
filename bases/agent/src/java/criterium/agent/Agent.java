package criterium.agent;

import clojure.lang.IFn;

// Low level interface to native agent.

// This is in jaa so that the class is available on VM_START,
// and the agent can start with minimal capabilities and overhead.

public class Agent {
  public static long state = -1;
  public static IFn handler;

  // clojure has trouble getting the static field without garbage
  public long getState() {
    return state;
  }

  public static class AllocationStartMarker {};
  public static class AllocationFinishMarker {};

  public static AllocationStartMarker allocation_start_marker()  {
    return new AllocationStartMarker();
  }

  public static AllocationFinishMarker allocation_finish_marker()  {
    return new AllocationFinishMarker();
  }

  public static void set_handler(clojure.lang.IFn handler_fn) {
    handler = handler_fn;
  }

  public static void here()  {
    System.out.println("here");
  }

  // commands sent to the agent
  public static native void command(long cmd);

  // data sent from the agent
  public static void data1(Object object) {
    if (handler != null) {
      handler.invoke(object);
    } else {
      System.out.print(object);
    }
  }
  public static void
    data8(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
          Object h) {
    if (handler != null) {
      handler.invoke(a, b, c, d, e, f, g, h);
    }
  }
}
