#include <gtest/gtest.h>
#include "include/state_transitions.h"
#include "include/agent_types.h"

using namespace criterium;
using namespace criterium::method_tracing_transitions;

// Tests for pure method tracing state transition functions.
// These tests verify the core state machine logic without requiring any
// mocking of JVMTI/JNI operations, since the functions are pure.

/// next_state_for_command tests

TEST(MethodTracingNextStateForCommand, StartCommandReturnsStarting) {
  EXPECT_EQ(next_state_for_command(start_method_tracing),
            method_tracing_starting);
}

TEST(MethodTracingNextStateForCommand, StopCommandReturnsStopping) {
  EXPECT_EQ(next_state_for_command(stop_method_tracing),
            method_tracing_stopping);
}

TEST(MethodTracingNextStateForCommand, ReportCommandReturnsReporting) {
  EXPECT_EQ(next_state_for_command(report_method_tracing),
            method_tracing_reporting);
}

TEST(MethodTracingNextStateForCommand, AllocationCommandReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(start_allocation_tracing), -1);
}

TEST(MethodTracingNextStateForCommand, PingReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(ping), -1);
}

TEST(MethodTracingNextStateForCommand, SyncStateReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(sync_state), -1);
}

TEST(MethodTracingNextStateForCommand, UnknownCommandReturnsNoChange) {
  EXPECT_EQ(next_state_for_command(99999), -1);
}

/// is_method_tracing_state tests

TEST(IsMethodTracingState, ReturnsTrueForMethodTracingStarting) {
  EXPECT_TRUE(is_method_tracing_state(method_tracing_starting));
}

TEST(IsMethodTracingState, ReturnsTrueForMethodTracingActive) {
  EXPECT_TRUE(is_method_tracing_state(method_tracing_active));
}

TEST(IsMethodTracingState, ReturnsTrueForMethodTracingStopping) {
  EXPECT_TRUE(is_method_tracing_state(method_tracing_stopping));
}

TEST(IsMethodTracingState, ReturnsTrueForMethodTracingStopped) {
  EXPECT_TRUE(is_method_tracing_state(method_tracing_stopped));
}

TEST(IsMethodTracingState, ReturnsTrueForMethodTracingReporting) {
  EXPECT_TRUE(is_method_tracing_state(method_tracing_reporting));
}

TEST(IsMethodTracingState, ReturnsTrueForMethodTracingReported) {
  EXPECT_TRUE(is_method_tracing_state(method_tracing_reported));
}

TEST(IsMethodTracingState, ReturnsFalseForPassive) {
  EXPECT_FALSE(is_method_tracing_state(passive));
}

TEST(IsMethodTracingState, ReturnsFalseForAllocationTracingStates) {
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_starting));
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_active));
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_stopping));
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_flushing));
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_flushed));
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_reporting));
  EXPECT_FALSE(is_method_tracing_state(allocation_tracing_reported));
}

/// is_method_tracing_active tests

TEST(IsMethodTracingActive, ReturnsTrueForActiveState) {
  EXPECT_TRUE(is_method_tracing_active(method_tracing_active));
}

TEST(IsMethodTracingActive, ReturnsFalseForStartingState) {
  EXPECT_FALSE(is_method_tracing_active(method_tracing_starting));
}

TEST(IsMethodTracingActive, ReturnsFalseForStoppingState) {
  EXPECT_FALSE(is_method_tracing_active(method_tracing_stopping));
}

TEST(IsMethodTracingActive, ReturnsFalseForStoppedState) {
  EXPECT_FALSE(is_method_tracing_active(method_tracing_stopped));
}

TEST(IsMethodTracingActive, ReturnsFalseForReportingState) {
  EXPECT_FALSE(is_method_tracing_active(method_tracing_reporting));
}

TEST(IsMethodTracingActive, ReturnsFalseForReportedState) {
  EXPECT_FALSE(is_method_tracing_active(method_tracing_reported));
}

TEST(IsMethodTracingActive, ReturnsFalseForPassive) {
  EXPECT_FALSE(is_method_tracing_active(passive));
}

/// next_state_after_events_enabled tests

TEST(NextStateAfterEventsEnabled, TransitionsFromStartingToActive) {
  EXPECT_EQ(next_state_after_events_enabled(method_tracing_starting),
            method_tracing_active);
}

TEST(NextStateAfterEventsEnabled, StaysInActiveState) {
  EXPECT_EQ(next_state_after_events_enabled(method_tracing_active),
            method_tracing_active);
}

TEST(NextStateAfterEventsEnabled, StaysInPassiveState) {
  EXPECT_EQ(next_state_after_events_enabled(passive), passive);
}

TEST(NextStateAfterEventsEnabled, StaysInStoppingState) {
  EXPECT_EQ(next_state_after_events_enabled(method_tracing_stopping),
            method_tracing_stopping);
}

/// next_state_after_events_disabled tests

TEST(NextStateAfterEventsDisabled, TransitionsFromStoppingToStopped) {
  EXPECT_EQ(next_state_after_events_disabled(method_tracing_stopping),
            method_tracing_stopped);
}

TEST(NextStateAfterEventsDisabled, StaysInStoppedState) {
  EXPECT_EQ(next_state_after_events_disabled(method_tracing_stopped),
            method_tracing_stopped);
}

TEST(NextStateAfterEventsDisabled, StaysInPassiveState) {
  EXPECT_EQ(next_state_after_events_disabled(passive), passive);
}

TEST(NextStateAfterEventsDisabled, StaysInActiveState) {
  EXPECT_EQ(next_state_after_events_disabled(method_tracing_active),
            method_tracing_active);
}

/// next_state_after_report_complete tests

TEST(NextStateAfterReportComplete, TransitionsFromReportingToReported) {
  EXPECT_EQ(next_state_after_report_complete(method_tracing_reporting),
            method_tracing_reported);
}

TEST(NextStateAfterReportComplete, StaysInReportedState) {
  EXPECT_EQ(next_state_after_report_complete(method_tracing_reported),
            method_tracing_reported);
}

TEST(NextStateAfterReportComplete, StaysInPassiveState) {
  EXPECT_EQ(next_state_after_report_complete(passive), passive);
}

TEST(NextStateAfterReportComplete, StaysInActiveState) {
  EXPECT_EQ(next_state_after_report_complete(method_tracing_active),
            method_tracing_active);
}

TEST(NextStateAfterReportComplete, StaysInStoppedState) {
  EXPECT_EQ(next_state_after_report_complete(method_tracing_stopped),
            method_tracing_stopped);
}
