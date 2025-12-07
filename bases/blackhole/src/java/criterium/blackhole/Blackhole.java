package criterium.blackhole;

import java.lang.ref.WeakReference;
import java.util.Random;

/**
 * Blackhole for preventing dead code elimination (DCE) in benchmarks.
 *
 * <p>Two modes are supported:
 *
 * <h3>Compiler Blackhole (JVM 17+)</h3>
 * <p>The empty static {@code consume()} methods can be registered as compiler
 * blackholes using: {@code -XX:CompileCommand=blackhole,criterium.blackhole.Blackhole::consume}
 * <p>The JIT compiler will drop calls to these methods but keep arguments alive,
 * providing zero-overhead DCE prevention.
 *
 * <h3>Runtime Blackhole (JVM 11-16)</h3>
 * <p>The instance {@code consumeRuntime()} methods use volatile field reads and
 * XOR-based impossible conditions to prevent DCE. This matches JMH's ~3ns overhead.
 *
 * <h3>Implementation Notes</h3>
 * <p>The class hierarchy (L0-L4) provides cache line padding to prevent false sharing.
 * This is critical for performance - without padding, volatile field access from
 * multiple threads causes cache line bouncing.
 *
 * <p>The padding uses boolean fields (128 per level = 128 bytes) placed before and
 * after the volatile working fields. Since superclass fields are not reordered with
 * subclass fields, this guarantees isolation.
 *
 * <p>Based on JMH's Blackhole implementation. See:
 * https://github.com/openjdk/jmh/blob/master/jmh-core/src/main/java/org/openjdk/jmh/infra/Blackhole.java
 */

abstract class BlackholeL0 {
  @SuppressWarnings("unused")
  private int markerBegin;
}

/**
 * Padding before the volatile fields.
 * 128 boolean fields = 128 bytes, exceeding typical 64-byte cache lines.
 */
abstract class BlackholeL1 extends BlackholeL0 {
  @SuppressWarnings("unused") private boolean p001, p002, p003, p004, p005, p006, p007, p008;
  @SuppressWarnings("unused") private boolean p011, p012, p013, p014, p015, p016, p017, p018;
  @SuppressWarnings("unused") private boolean p021, p022, p023, p024, p025, p026, p027, p028;
  @SuppressWarnings("unused") private boolean p031, p032, p033, p034, p035, p036, p037, p038;
  @SuppressWarnings("unused") private boolean p041, p042, p043, p044, p045, p046, p047, p048;
  @SuppressWarnings("unused") private boolean p051, p052, p053, p054, p055, p056, p057, p058;
  @SuppressWarnings("unused") private boolean p061, p062, p063, p064, p065, p066, p067, p068;
  @SuppressWarnings("unused") private boolean p071, p072, p073, p074, p075, p076, p077, p078;
  @SuppressWarnings("unused") private boolean p101, p102, p103, p104, p105, p106, p107, p108;
  @SuppressWarnings("unused") private boolean p111, p112, p113, p114, p115, p116, p117, p118;
  @SuppressWarnings("unused") private boolean p121, p122, p123, p124, p125, p126, p127, p128;
  @SuppressWarnings("unused") private boolean p131, p132, p133, p134, p135, p136, p137, p138;
  @SuppressWarnings("unused") private boolean p141, p142, p143, p144, p145, p146, p147, p148;
  @SuppressWarnings("unused") private boolean p151, p152, p153, p154, p155, p156, p157, p158;
  @SuppressWarnings("unused") private boolean p161, p162, p163, p164, p165, p166, p167, p168;
  @SuppressWarnings("unused") private boolean p171, p172, p173, p174, p175, p176, p177, p178;
}

/**
 * Volatile fields for runtime blackhole consumption.
 * Each volatile field has a non-volatile "tombstone" counterpart with a guaranteed
 * different value. The XOR comparison is always false but unprovable by the compiler.
 */
abstract class BlackholeL2 extends BlackholeL1 {
  // Volatile fields - force re-read on each access
  public volatile byte b1;
  public volatile boolean bool1;
  public volatile char c1;
  public volatile short s1;
  public volatile int i1;
  public volatile long l1;
  public volatile float f1;
  public volatile double d1;

  // Non-volatile tombstones - guaranteed different from volatile counterparts
  public byte b2;
  public boolean bool2;
  public char c2;
  public short s2;
  public int i2;
  public long l2;
  public float f2;
  public double d2;

  // Object consumption uses weak reference + thread-local random
  public volatile Object obj1;
  public volatile BlackholeL2 nullBait = null;
  public int tlr;
  public volatile int tlrMask;

  public BlackholeL2() {
    Random r = new Random(System.nanoTime());
    tlr = r.nextInt();
    tlrMask = 1;
    obj1 = new Object();

    // Initialize volatile/tombstone pairs with guaranteed different values
    b1 = (byte) r.nextInt();
    b2 = (byte) (b1 + 1);
    bool1 = r.nextBoolean();
    bool2 = !bool1;
    c1 = (char) r.nextInt();
    c2 = (char) (c1 + 1);
    s1 = (short) r.nextInt();
    s2 = (short) (s1 + 1);
    i1 = r.nextInt();
    i2 = i1 + 1;
    l1 = r.nextLong();
    l2 = l1 + 1;
    f1 = r.nextFloat();
    f2 = f1 + Math.ulp(f1);
    d1 = r.nextDouble();
    d2 = d1 + Math.ulp(d1);

    // Validate tombstones are different (should never fail)
    if (b1 == b2) throw new IllegalStateException("byte tombstones equal");
    if (bool1 == bool2) throw new IllegalStateException("boolean tombstones equal");
    if (c1 == c2) throw new IllegalStateException("char tombstones equal");
    if (s1 == s2) throw new IllegalStateException("short tombstones equal");
    if (i1 == i2) throw new IllegalStateException("int tombstones equal");
    if (l1 == l2) throw new IllegalStateException("long tombstones equal");
    if (f1 == f2) throw new IllegalStateException("float tombstones equal");
    if (d1 == d2) throw new IllegalStateException("double tombstones equal");
  }
}

/**
 * Padding after the volatile fields.
 * 128 boolean fields = 128 bytes, exceeding typical 64-byte cache lines.
 */
abstract class BlackholeL3 extends BlackholeL2 {
  @SuppressWarnings("unused") private boolean q001, q002, q003, q004, q005, q006, q007, q008;
  @SuppressWarnings("unused") private boolean q011, q012, q013, q014, q015, q016, q017, q018;
  @SuppressWarnings("unused") private boolean q021, q022, q023, q024, q025, q026, q027, q028;
  @SuppressWarnings("unused") private boolean q031, q032, q033, q034, q035, q036, q037, q038;
  @SuppressWarnings("unused") private boolean q041, q042, q043, q044, q045, q046, q047, q048;
  @SuppressWarnings("unused") private boolean q051, q052, q053, q054, q055, q056, q057, q058;
  @SuppressWarnings("unused") private boolean q061, q062, q063, q064, q065, q066, q067, q068;
  @SuppressWarnings("unused") private boolean q071, q072, q073, q074, q075, q076, q077, q078;
  @SuppressWarnings("unused") private boolean q101, q102, q103, q104, q105, q106, q107, q108;
  @SuppressWarnings("unused") private boolean q111, q112, q113, q114, q115, q116, q117, q118;
  @SuppressWarnings("unused") private boolean q121, q122, q123, q124, q125, q126, q127, q128;
  @SuppressWarnings("unused") private boolean q131, q132, q133, q134, q135, q136, q137, q138;
  @SuppressWarnings("unused") private boolean q141, q142, q143, q144, q145, q146, q147, q148;
  @SuppressWarnings("unused") private boolean q151, q152, q153, q154, q155, q156, q157, q158;
  @SuppressWarnings("unused") private boolean q161, q162, q163, q164, q165, q166, q167, q168;
  @SuppressWarnings("unused") private boolean q171, q172, q173, q174, q175, q176, q177, q178;
}

abstract class BlackholeL4 extends BlackholeL3 {
  @SuppressWarnings("unused")
  private int markerEnd;
}

public final class Blackhole extends BlackholeL4 {

  ///
  /// Static consume methods for compiler blackhole mode (JVM 17+)
  ///

  /**
   * Consume a boolean value, preventing DCE.
   */
  public static void consume(boolean value) {}

  /**
   * Consume a byte value, preventing DCE.
   */
  public static void consume(byte value) {}

  /**
   * Consume a char value, preventing DCE.
   */
  public static void consume(char value) {}

  /**
   * Consume a short value, preventing DCE.
   */
  public static void consume(short value) {}

  /**
   * Consume an int value, preventing DCE.
   */
  public static void consume(int value) {}

  /**
   * Consume a long value, preventing DCE.
   */
  public static void consume(long value) {}

  /**
   * Consume a float value, preventing DCE.
   */
  public static void consume(float value) {}

  /**
   * Consume a double value, preventing DCE.
   */
  public static void consume(double value) {}

  /**
   * Consume an Object value, preventing DCE.
   */
  public static void consume(Object value) {}

  ///
  /// Runtime blackhole methods for JVM < 17
  /// These use volatile reads and impossible conditions to prevent DCE.
  ///

  /**
   * Consume a byte value at runtime, preventing DCE.
   */
  public final void consumeRuntime(byte b) {
    byte b1 = this.b1; // volatile read
    byte b2 = this.b2;
    if ((b ^ b1) == (b ^ b2)) {
      // SHOULD NEVER HAPPEN
      nullBait.b1 = b; // implicit NPE
    }
  }

  /**
   * Consume a boolean value at runtime, preventing DCE.
   */
  public final void consumeRuntime(boolean bool) {
    boolean bool1 = this.bool1; // volatile read
    boolean bool2 = this.bool2;
    if ((bool ^ bool1) == (bool ^ bool2)) {
      // SHOULD NEVER HAPPEN
      nullBait.bool1 = bool; // implicit NPE
    }
  }

  /**
   * Consume a char value at runtime, preventing DCE.
   */
  public final void consumeRuntime(char c) {
    char c1 = this.c1; // volatile read
    char c2 = this.c2;
    if ((c ^ c1) == (c ^ c2)) {
      // SHOULD NEVER HAPPEN
      nullBait.c1 = c; // implicit NPE
    }
  }

  /**
   * Consume a short value at runtime, preventing DCE.
   */
  public final void consumeRuntime(short s) {
    short s1 = this.s1; // volatile read
    short s2 = this.s2;
    if ((s ^ s1) == (s ^ s2)) {
      // SHOULD NEVER HAPPEN
      nullBait.s1 = s; // implicit NPE
    }
  }

  /**
   * Consume an int value at runtime, preventing DCE.
   */
  public final void consumeRuntime(int i) {
    int i1 = this.i1; // volatile read
    int i2 = this.i2;
    if ((i ^ i1) == (i ^ i2)) {
      // SHOULD NEVER HAPPEN
      nullBait.i1 = i; // implicit NPE
    }
  }

  /**
   * Consume a long value at runtime, preventing DCE.
   */
  public final void consumeRuntime(long l) {
    long l1 = this.l1; // volatile read
    long l2 = this.l2;
    if ((l ^ l1) == (l ^ l2)) {
      // SHOULD NEVER HAPPEN
      nullBait.l1 = l; // implicit NPE
    }
  }

  /**
   * Consume a float value at runtime, preventing DCE.
   */
  public final void consumeRuntime(float f) {
    float f1 = this.f1; // volatile read
    float f2 = this.f2;
    if (f == f1 & f == f2) {
      // SHOULD NEVER HAPPEN
      nullBait.f1 = f; // implicit NPE
    }
  }

  /**
   * Consume a double value at runtime, preventing DCE.
   */
  public final void consumeRuntime(double d) {
    double d1 = this.d1; // volatile read
    double d2 = this.d2;
    if (d == d1 & d == d2) {
      // SHOULD NEVER HAPPEN
      nullBait.d1 = d; // implicit NPE
    }
  }

  /**
   * Consume an Object value at runtime, preventing DCE.
   * Uses weak reference + thread-local random to defeat escape analysis
   * while avoiding memory retention.
   */
  public final void consumeRuntime(Object obj) {
    int tlrMask = this.tlrMask; // volatile read
    int tlr = (this.tlr = (this.tlr * 1664525 + 1013904223));
    if ((tlr & tlrMask) == 0) {
      // SHOULD ALMOST NEVER HAPPEN IN MEASUREMENT
      this.obj1 = new WeakReference<>(obj);
      this.tlrMask = (tlrMask << 1) + 1;
    }
  }

  /**
   * Clear any captured object references.
   * Call this after measurement to prevent object retention.
   */
  public void evaporate() {
    obj1 = null;
  }
}
