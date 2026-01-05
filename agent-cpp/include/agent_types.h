#ifndef CRITERIUM_AGENT_TYPES_H
#define CRITERIUM_AGENT_TYPES_H

#include <jni.h>
#include <jvmti.h>
#include <array>

namespace criterium {

/// Agent state machine states.
/// Using jlong for JNI compatibility when syncing state to Java.
// NOLINTBEGIN(performance-enum-size)
enum States : jlong {
  passive = 0,
  allocation_tracing_starting = 10,
  allocation_tracing_active = 11,
  allocation_tracing_stopping = 15,
  allocation_tracing_flushing = 16,
  allocation_tracing_flushed = 17,
  allocation_tracing_reporting = 18,
  allocation_tracing_reported = 19,
  method_tracing_starting = 20,
  method_tracing_active = 21,
  method_tracing_stopping = 25,
  method_tracing_stopped = 26,
  method_tracing_reporting = 27,
  method_tracing_reported = 28,
};

/// Commands sent from Java to the agent.
enum Commands : jlong {
  ping = 0,
  sync_state = 1,
  start_allocation_tracing = 10,
  stop_allocation_tracing = 11,
  report_allocation_tracing = 12,
  start_method_tracing = 20,
  stop_method_tracing = 21,
  report_method_tracing = 22
};
// NOLINTEND(performance-enum-size)

/// Maximum stack frames to capture.
inline constexpr jint MAX_FRAMES = 1024;

} // namespace criterium

#endif // CRITERIUM_AGENT_TYPES_H
