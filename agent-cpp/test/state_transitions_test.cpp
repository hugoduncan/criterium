#include <gtest/gtest.h>
#include "include/state_transitions.h"
#include "include/agent_types.h"

using namespace criterium;
using namespace criterium::state_transitions;

// Tests for pure state transition functions.
// These tests verify the core state machine logic without requiring any
// mocking of JVMTI/JNI operations, since the functions are pure.

/// is_start_marker_allocation tests

TEST(IsStartMarkerAllocation, ReturnsTrueInStartingStateWithStartMarker) {
  EXPECT_TRUE(is_start_marker_allocation(allocation_tracing_starting,
                                         ALLOCATION_START_MARKER));
}

TEST(IsStartMarkerAllocation, ReturnsFalseInStartingStateWithOtherClass) {
  EXPECT_FALSE(
      is_start_marker_allocation(allocation_tracing_starting, "Ljava/lang/Object;"));
}

TEST(IsStartMarkerAllocation, ReturnsFalseInActiveStateWithStartMarker) {
  EXPECT_FALSE(is_start_marker_allocation(allocation_tracing_active,
                                          ALLOCATION_START_MARKER));
}

TEST(IsStartMarkerAllocation, ReturnsFalseInPassiveState) {
  EXPECT_FALSE(is_start_marker_allocation(passive, ALLOCATION_START_MARKER));
}

TEST(IsStartMarkerAllocation, ReturnsFalseInStoppingState) {
  EXPECT_FALSE(is_start_marker_allocation(allocation_tracing_stopping,
                                          ALLOCATION_START_MARKER));
}

TEST(IsStartMarkerAllocation, ReturnsFalseWithFinishMarkerClass) {
  EXPECT_FALSE(is_start_marker_allocation(allocation_tracing_starting,
                                          ALLOCATION_FINISH_MARKER));
}

/// is_finish_marker_allocation tests

TEST(IsFinishMarkerAllocation, ReturnsTrueInStoppingStateWithFinishMarker) {
  EXPECT_TRUE(is_finish_marker_allocation(allocation_tracing_stopping,
                                          ALLOCATION_FINISH_MARKER));
}

TEST(IsFinishMarkerAllocation, ReturnsFalseInStoppingStateWithOtherClass) {
  EXPECT_FALSE(
      is_finish_marker_allocation(allocation_tracing_stopping, "Ljava/lang/Object;"));
}

TEST(IsFinishMarkerAllocation, ReturnsFalseInActiveStateWithFinishMarker) {
  EXPECT_FALSE(is_finish_marker_allocation(allocation_tracing_active,
                                           ALLOCATION_FINISH_MARKER));
}

TEST(IsFinishMarkerAllocation, ReturnsFalseInFlushingState) {
  EXPECT_FALSE(is_finish_marker_allocation(allocation_tracing_flushing,
                                           ALLOCATION_FINISH_MARKER));
}

TEST(IsFinishMarkerAllocation, ReturnsFalseWithStartMarkerClass) {
  EXPECT_FALSE(is_finish_marker_allocation(allocation_tracing_stopping,
                                           ALLOCATION_START_MARKER));
}

/// next_state_after_allocation tests

TEST(NextStateAfterAllocation, TransitionsToFlushingOnFinishMarker) {
  EXPECT_EQ(next_state_after_allocation(allocation_tracing_stopping,
                                        ALLOCATION_FINISH_MARKER),
            allocation_tracing_flushing);
}

TEST(NextStateAfterAllocation, StaysInCurrentStateForRegularAllocation) {
  EXPECT_EQ(next_state_after_allocation(allocation_tracing_active,
                                        "Ljava/lang/Object;"),
            allocation_tracing_active);
}

TEST(NextStateAfterAllocation, StaysInStartingStateForStartMarker) {
  // Start marker allocation doesn't change state - the free event does
  EXPECT_EQ(next_state_after_allocation(allocation_tracing_starting,
                                        ALLOCATION_START_MARKER),
            allocation_tracing_starting);
}

TEST(NextStateAfterAllocation, StaysInStoppingForRegularClass) {
  EXPECT_EQ(next_state_after_allocation(allocation_tracing_stopping,
                                        "Ljava/lang/Object;"),
            allocation_tracing_stopping);
}

/// next_state_after_object_free tests

TEST(NextStateAfterObjectFree, TransitionsToActiveOnStartMarkerFree) {
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_starting, true, false),
      allocation_tracing_active);
}

TEST(NextStateAfterObjectFree, StaysInStartingIfNotStartMarker) {
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_starting, false, false),
      allocation_tracing_starting);
}

TEST(NextStateAfterObjectFree, TransitionsToFlushedOnDisableMarkerFree) {
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_flushing, false, true),
      allocation_tracing_flushed);
}

TEST(NextStateAfterObjectFree, StaysInFlushingIfNotDisableMarker) {
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_flushing, false, false),
      allocation_tracing_flushing);
}

TEST(NextStateAfterObjectFree, StaysInActiveOnRegularFree) {
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_active, false, false),
      allocation_tracing_active);
}

TEST(NextStateAfterObjectFree, StartMarkerInActiveStateNoTransition) {
  // Start marker freed when already active - no effect
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_active, true, false),
      allocation_tracing_active);
}

TEST(NextStateAfterObjectFree, DisableMarkerInActiveStateNoTransition) {
  // Disable marker freed when in active state - no effect
  EXPECT_EQ(
      next_state_after_object_free(allocation_tracing_active, false, true),
      allocation_tracing_active);
}

/// next_state_for_command tests

TEST(NextStateForCommand, StartCommandReturnsStarting) {
  EXPECT_EQ(next_state_for_command(start_allocation_tracing),
            allocation_tracing_starting);
}

TEST(NextStateForCommand, StopCommandReturnsStopping) {
  EXPECT_EQ(next_state_for_command(stop_allocation_tracing),
            allocation_tracing_stopping);
}

TEST(NextStateForCommand, ReportCommandReturnsReporting) {
  EXPECT_EQ(next_state_for_command(report_allocation_tracing),
            allocation_tracing_reporting);
}

TEST(NextStateForCommand, PingReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(ping), -1);
}

TEST(NextStateForCommand, SyncStateReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(sync_state), -1);
}

TEST(NextStateForCommand, UnknownCommandReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(99999), -1);
}
