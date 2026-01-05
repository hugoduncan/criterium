#ifndef CRITERIUM_STATE_TRANSITIONS_H
#define CRITERIUM_STATE_TRANSITIONS_H

#include "agent_types.h"
#include <cstring>

namespace criterium {

/// Marker class signatures for state transitions.
inline constexpr char const* ALLOCATION_START_MARKER =
    "Lcriterium/agent/Agent$AllocationStartMarker;";
inline constexpr char const* ALLOCATION_FINISH_MARKER =
    "Lcriterium/agent/Agent$AllocationFinishMarker;";

/// Pure functions for state machine transitions.
/// These can be tested without mocking JVMTI/JNI.
namespace state_transitions {

/// Returns true if the allocation is a start marker in the starting state.
inline bool is_start_marker_allocation(jlong state, const char* class_sig) {
  return state == allocation_tracing_starting &&
         std::strcmp(class_sig, ALLOCATION_START_MARKER) == 0;
}

/// Returns true if the allocation is a finish marker in the stopping state.
inline bool is_finish_marker_allocation(jlong state, const char* class_sig) {
  return state == allocation_tracing_stopping &&
         std::strcmp(class_sig, ALLOCATION_FINISH_MARKER) == 0;
}

/// Returns the next state after processing an allocation event.
/// Returns the current state if no transition occurs.
inline jlong next_state_after_allocation(jlong state, const char* class_sig) {
  if (is_finish_marker_allocation(state, class_sig)) {
    return allocation_tracing_flushing;
  }
  return state;
}

/// Returns the next state after processing an object free event.
/// @param state Current agent state
/// @param is_start_marker True if the freed object was marked as start marker
/// @param is_disable_marker True if the freed object was marked as disable
/// marker
inline jlong next_state_after_object_free(jlong state, bool is_start_marker,
                                          bool is_disable_marker) {
  if (is_start_marker && state == allocation_tracing_starting) {
    return allocation_tracing_active;
  }
  if (is_disable_marker && state == allocation_tracing_flushing) {
    return allocation_tracing_flushed;
  }
  return state;
}

/// Returns the next state after processing a command.
/// Note: Some commands (start, stop) cause immediate transitions.
/// Report command transitions through reporting to reported.
inline jlong next_state_for_command(jlong cmd) {
  switch (cmd) {
  case start_allocation_tracing:
    return allocation_tracing_starting;
  case stop_allocation_tracing:
    return allocation_tracing_stopping;
  case report_allocation_tracing:
    // Caller should set reporting, then reported after work is done
    return allocation_tracing_reporting;
  case sync_state:
  case ping:
  default:
    // These commands don't change state
    return -1; // Sentinel meaning "no state change"
  }
}

} // namespace state_transitions
} // namespace criterium

#endif // CRITERIUM_STATE_TRANSITIONS_H
