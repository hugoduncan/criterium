#include <gtest/gtest.h>
#include <gmock/gmock.h>
#include <tuple>
#include <vector>
#include "include/agent_types.h"
#include "include/agent_state.h"
#include "include/state_transitions.h"
#include "mocks.h"

using namespace criterium;
using namespace criterium::test;
using namespace criterium::state_transitions;
using ::testing::_;
using ::testing::Return;
using ::testing::NiceMock;
using ::testing::DoAll;
using ::testing::SetArgPointee;

// Tests for AgentState state machine behavior using mocked JVMTI/JNI ops.
// The pure state transition logic is tested in state_transitions_test.cpp.
// These tests verify that TestableStateMachine (which uses the shared
// pure functions) correctly interacts with JVMTI/JNI operations.
//
// State machine flow:
//   passive → allocation_tracing_starting (start command)
//           → allocation_tracing_active (start marker freed)
//           → allocation_tracing_stopping (stop command)
//           → allocation_tracing_flushing (finish marker seen)
//           → allocation_tracing_flushed (finish marker freed)
//           → allocation_tracing_reporting (report command)
//           → allocation_tracing_reported (report complete)

// Use shared marker constants from state_transitions.h
static constexpr char const* START_MARKER = ALLOCATION_START_MARKER;
static constexpr char const* FINISH_MARKER = ALLOCATION_FINISH_MARKER;
static constexpr char const* REGULAR_CLASS = "Ljava/lang/Object;";

/// Testable state machine that uses the same pure transition functions
/// as AgentState, ensuring tests and production code share the same logic.
/// This enables unit testing of state transitions without the full
/// agent dependencies (VMContext, AgentContext singletons).
class TestableStateMachine {
private:
  NiceMock<MockJvmtiOperations>& jvmti_ops_;
  [[maybe_unused]] NiceMock<MockJniOperations>& jni_ops_;
  jlong state_ = passive;
  std::vector<std::unique_ptr<AllocRec>> allocs_;
  std::map<jlong, AllocRec*> allocs_by_tag_;

public:
  TestableStateMachine(NiceMock<MockJvmtiOperations>& jvmti_ops,
                       NiceMock<MockJniOperations>& jni_ops)
      : jvmti_ops_(jvmti_ops), jni_ops_(jni_ops) {}

  jlong get_state() const { return state_; }

  void process_command(const Command& cmd) {
    auto new_state = next_state_for_command(cmd.cmd);
    if (new_state == -1) {
      // No state change for this command
      return;
    }

    if (cmd.cmd == start_allocation_tracing) {
      enable_allocation_tracing();
    } else if (cmd.cmd == stop_allocation_tracing) {
      state_ = new_state;
    } else if (cmd.cmd == report_allocation_tracing) {
      state_ = allocation_tracing_reporting;
      // allocation_tracing_report would be called here
      state_ = allocation_tracing_reported;
    }
  }

  void process_allocation_event(const char* class_sig, jlong tag) {
    auto starting = is_start_marker_allocation(state_, class_sig);
    auto stopping = is_finish_marker_allocation(state_, class_sig);

    auto rec = std::make_unique<AllocRec>(
        class_sig, 0, nullptr, nullptr, nullptr, -1,
        nullptr, nullptr, nullptr, -1, 0, tag);

    if (starting) {
      rec->start_marker = true;
    }

    if (stopping) {
      // Disable allocation sampling
      jvmti_ops_.set_event_notification_mode(
          JVMTI_DISABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, nullptr);
      rec->disable_marker = true;
      state_ = next_state_after_allocation(state_, class_sig);
    }

    allocs_by_tag_.emplace(rec->tag, rec.get());
    allocs_.push_back(std::move(rec));
  }

  void process_object_free_event(jlong tag) {
    auto it = allocs_by_tag_.find(tag);
    if (it == allocs_by_tag_.end()) {
      return;
    }

    AllocRec* rec = it->second;
    rec->freed = 1;

    auto new_state = next_state_after_object_free(state_, rec->start_marker,
                                                  rec->disable_marker);
    if (new_state != state_) {
      if (rec->disable_marker) {
        jvmti_ops_.set_event_notification_mode(
            JVMTI_DISABLE, JVMTI_EVENT_OBJECT_FREE, nullptr);
      }
      state_ = new_state;
    }
  }

  /// Returns allocation record for a given tag, or nullptr if not found.
  /// Used by tests to verify marker flags are set correctly.
  const AllocRec* get_alloc_by_tag(jlong tag) const {
    auto it = allocs_by_tag_.find(tag);
    return it != allocs_by_tag_.end() ? it->second : nullptr;
  }

  /// Returns count of tracked allocations.
  size_t get_alloc_count() const { return allocs_.size(); }

private:
  void enable_allocation_tracing() {
    state_ = allocation_tracing_starting;
    allocs_.clear();
    allocs_by_tag_.clear();
    jvmti_ops_.set_heap_sampling_interval(0);
    jvmti_ops_.set_event_notification_mode(
        JVMTI_ENABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, nullptr);
    jvmti_ops_.set_event_notification_mode(
        JVMTI_ENABLE, JVMTI_EVENT_OBJECT_FREE, nullptr);
  }
};

class AgentStateTest : public ::testing::Test {
protected:
  NiceMock<MockJvmtiOperations> mock_jvmti;
  NiceMock<MockJniOperations> mock_jni;
  std::unique_ptr<TestableStateMachine> state_machine;

  void SetUp() override {
    // Configure default mock behavior
    ON_CALL(mock_jvmti, set_heap_sampling_interval(_))
        .WillByDefault(Return(true));
    ON_CALL(mock_jvmti, set_event_notification_mode(_, _, _))
        .WillByDefault(Return(true));

    state_machine =
        std::make_unique<TestableStateMachine>(mock_jvmti, mock_jni);
  }
};

/// Test data for command-based state transitions.
struct CommandTransitionTest {
  const char* name;
  jlong initial_state;
  jlong command;
  jlong expected_state;
};

class CommandTransitionTestSuite
    : public AgentStateTest,
      public ::testing::WithParamInterface<CommandTransitionTest> {};

TEST_P(CommandTransitionTestSuite, TransitionsCorrectlyOnCommand) {
  const auto& test = GetParam();

  // Set initial state by processing appropriate commands
  if (test.initial_state != passive) {
    if (test.initial_state >= allocation_tracing_starting) {
      state_machine->process_command(Command{start_allocation_tracing});
    }
    if (test.initial_state >= allocation_tracing_active) {
      state_machine->process_allocation_event(START_MARKER, 1);
      state_machine->process_object_free_event(1);
    }
    if (test.initial_state >= allocation_tracing_stopping) {
      state_machine->process_command(Command{stop_allocation_tracing});
    }
    if (test.initial_state >= allocation_tracing_flushing) {
      state_machine->process_allocation_event(FINISH_MARKER, 2);
    }
    if (test.initial_state >= allocation_tracing_flushed) {
      state_machine->process_object_free_event(2);
    }
    if (test.initial_state >= allocation_tracing_reporting) {
      state_machine->process_command(Command{report_allocation_tracing});
    }
  }

  // Verify we're in the expected initial state
  ASSERT_EQ(state_machine->get_state(), test.initial_state)
      << "Failed to reach initial state for test: " << test.name;

  // Apply the command
  state_machine->process_command(Command{test.command});

  // Verify final state
  EXPECT_EQ(state_machine->get_state(), test.expected_state)
      << "State transition failed for: " << test.name;
}

INSTANTIATE_TEST_SUITE_P(
    CommandTransitions, CommandTransitionTestSuite,
    ::testing::Values(
        // Start tracing from passive
        CommandTransitionTest{"passive_to_starting_on_start_command",
                              passive, start_allocation_tracing,
                              allocation_tracing_starting},

        // Stop tracing from active
        CommandTransitionTest{"active_to_stopping_on_stop_command",
                              allocation_tracing_active,
                              stop_allocation_tracing,
                              allocation_tracing_stopping},

        // Report from flushed
        CommandTransitionTest{"flushed_to_reported_on_report_command",
                              allocation_tracing_flushed,
                              report_allocation_tracing,
                              allocation_tracing_reported},

        // Sync state has no effect on state value
        CommandTransitionTest{"starting_unchanged_on_sync_state",
                              allocation_tracing_starting,
                              sync_state,
                              allocation_tracing_starting},

        // Ping has no effect on state
        CommandTransitionTest{"passive_unchanged_on_ping",
                              passive, ping, passive}),
    [](const ::testing::TestParamInfo<CommandTransitionTest>& info) {
      return info.param.name;
    });

/// Test data for marker-based state transitions.
struct MarkerTransitionTest {
  const char* name;
  jlong initial_state;
  const char* marker_class;
  bool expect_free_event;  // Whether to process object_free after allocation
  jlong expected_state;
};

class MarkerTransitionTestSuite
    : public AgentStateTest,
      public ::testing::WithParamInterface<MarkerTransitionTest> {};

TEST_P(MarkerTransitionTestSuite, TransitionsCorrectlyOnMarker) {
  const auto& test = GetParam();
  const jlong test_tag = 100;

  // Set initial state
  if (test.initial_state >= allocation_tracing_starting) {
    state_machine->process_command(Command{start_allocation_tracing});
  }
  if (test.initial_state >= allocation_tracing_active) {
    state_machine->process_allocation_event(START_MARKER, 1);
    state_machine->process_object_free_event(1);
  }
  if (test.initial_state >= allocation_tracing_stopping) {
    state_machine->process_command(Command{stop_allocation_tracing});
  }
  if (test.initial_state >= allocation_tracing_flushing) {
    // Need to process finish marker allocation to reach flushing
    state_machine->process_allocation_event(FINISH_MARKER, 2);
  }
  if (test.initial_state >= allocation_tracing_flushed) {
    state_machine->process_object_free_event(2);
  }

  ASSERT_EQ(state_machine->get_state(), test.initial_state)
      << "Failed to reach initial state for test: " << test.name;

  // Process allocation event with the marker class
  state_machine->process_allocation_event(test.marker_class, test_tag);

  if (test.expect_free_event) {
    state_machine->process_object_free_event(test_tag);
  }

  EXPECT_EQ(state_machine->get_state(), test.expected_state)
      << "State transition failed for: " << test.name;
}

INSTANTIATE_TEST_SUITE_P(
    MarkerTransitions, MarkerTransitionTestSuite,
    ::testing::Values(
        // Start marker allocation followed by free → active
        MarkerTransitionTest{"starting_to_active_on_start_marker_freed",
                             allocation_tracing_starting,
                             START_MARKER, true,
                             allocation_tracing_active},

        // Start marker allocation without free → stays starting
        MarkerTransitionTest{"starting_unchanged_on_start_marker_not_freed",
                             allocation_tracing_starting,
                             START_MARKER, false,
                             allocation_tracing_starting},

        // Finish marker seen → flushing
        MarkerTransitionTest{"stopping_to_flushing_on_finish_marker_seen",
                             allocation_tracing_stopping,
                             FINISH_MARKER, false,
                             allocation_tracing_flushing},

        // Regular allocation doesn't change state
        MarkerTransitionTest{"active_unchanged_on_regular_allocation",
                             allocation_tracing_active,
                             REGULAR_CLASS, false,
                             allocation_tracing_active}),
    [](const ::testing::TestParamInfo<MarkerTransitionTest>& info) {
      return info.param.name;
    });

// Special test for flushing → flushed transition.
// This transition requires freeing the same finish marker that caused
// the stopping → flushing transition, so it can't use the table-driven
// approach which uses a new tag.
TEST_F(AgentStateTest, FlushingToFlushedOnFinishMarkerFreed) {
  // Reach flushing state - the finish marker is recorded with tag 2
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});
  state_machine->process_allocation_event(FINISH_MARKER, 2);

  ASSERT_EQ(state_machine->get_state(), allocation_tracing_flushing);

  // Free the finish marker (tag 2) - should transition to flushed
  state_machine->process_object_free_event(2);

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushed);
}

// Additional tests for edge cases and JVMTI/JNI call verification

TEST_F(AgentStateTest, StartCommandEnablesAllocationTracking) {
  EXPECT_CALL(mock_jvmti, set_heap_sampling_interval(0)).Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);

  state_machine->process_command(Command{start_allocation_tracing});

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
}

TEST_F(AgentStateTest, FinishMarkerSeenDisablesAllocationEvents) {
  // Set up: reach stopping state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});

  ASSERT_EQ(state_machine->get_state(), allocation_tracing_stopping);

  EXPECT_CALL(mock_jvmti,
              set_event_notification_mode(
                  JVMTI_DISABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);

  state_machine->process_allocation_event(FINISH_MARKER, 2);

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushing);
}

TEST_F(AgentStateTest, FinishMarkerFreedDisablesObjectFreeEvents) {
  // Set up: reach flushing state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});
  state_machine->process_allocation_event(FINISH_MARKER, 2);

  ASSERT_EQ(state_machine->get_state(), allocation_tracing_flushing);

  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_DISABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);

  state_machine->process_object_free_event(2);

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushed);
}

TEST_F(AgentStateTest, StartClearsExistingAllocations) {
  // First allocation tracing cycle
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(REGULAR_CLASS, 1);
  state_machine->process_allocation_event(REGULAR_CLASS, 2);

  // Start new cycle - should clear allocations
  state_machine->process_command(Command{start_allocation_tracing});

  // Process object free for old tag - should not cause crash or state change
  state_machine->process_object_free_event(1);
  state_machine->process_object_free_event(2);

  // State should still be starting (allocations were cleared)
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
}

TEST_F(AgentStateTest, UnknownTagFreeIsIgnored) {
  state_machine->process_command(Command{start_allocation_tracing});

  // Process free for tag that was never allocated
  state_machine->process_object_free_event(999);

  // State should be unchanged
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
}

/// Command Processing Tests
///
/// These tests focus on the behavior of individual commands beyond just
/// state transitions. They verify that commands trigger the correct
/// JVMTI/JNI operations and handle edge cases properly.

TEST_F(AgentStateTest, StopCommandOnlyChangesState) {
  // Set up: reach active state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_active);

  // Stop command should NOT make any JVMTI calls - it just changes state.
  // The actual disabling happens when the finish marker is seen.
  EXPECT_CALL(mock_jvmti, set_heap_sampling_interval(_)).Times(0);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(_, _, _)).Times(0);

  state_machine->process_command(Command{stop_allocation_tracing});

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_stopping);
}

TEST_F(AgentStateTest, ReportCommandTransitionsThroughReportingState) {
  // Set up: reach flushed state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});
  state_machine->process_allocation_event(FINISH_MARKER, 2);
  state_machine->process_object_free_event(2);

  ASSERT_EQ(state_machine->get_state(), allocation_tracing_flushed);

  // Report command should end in reported state
  state_machine->process_command(Command{report_allocation_tracing});

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_reported);
}

TEST_F(AgentStateTest, SyncStateDoesNotChangeState) {
  // Test sync_state in various states
  EXPECT_EQ(state_machine->get_state(), passive);
  state_machine->process_command(Command{sync_state});
  EXPECT_EQ(state_machine->get_state(), passive);

  state_machine->process_command(Command{start_allocation_tracing});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
  state_machine->process_command(Command{sync_state});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);

  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active);
  state_machine->process_command(Command{sync_state});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active);
}

TEST_F(AgentStateTest, PingCommandDoesNotChangeState) {
  EXPECT_EQ(state_machine->get_state(), passive);
  state_machine->process_command(Command{ping});
  EXPECT_EQ(state_machine->get_state(), passive);

  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_command(Command{ping});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
}

TEST_F(AgentStateTest, InvalidCommandDoesNotChangeState) {
  // Invalid command values should not crash and should not change state
  const jlong INVALID_COMMAND = 99999;

  EXPECT_EQ(state_machine->get_state(), passive);
  state_machine->process_command(Command{INVALID_COMMAND});
  EXPECT_EQ(state_machine->get_state(), passive);

  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_command(Command{INVALID_COMMAND});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
}

TEST_F(AgentStateTest, MultipleStartCommandsResetAllocations) {
  // First start
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);

  // Record some allocations
  state_machine->process_allocation_event(REGULAR_CLASS, 10);
  state_machine->process_allocation_event(REGULAR_CLASS, 11);
  state_machine->process_allocation_event(REGULAR_CLASS, 12);

  // Second start should reset - expect fresh JVMTI calls
  EXPECT_CALL(mock_jvmti, set_heap_sampling_interval(0)).Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);

  state_machine->process_command(Command{start_allocation_tracing});

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);

  // Old tags should be forgotten - freeing them should have no effect
  state_machine->process_object_free_event(10);
  state_machine->process_object_free_event(11);
  state_machine->process_object_free_event(12);

  // State should still be starting (not active, since we haven't processed
  // a new start marker)
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);
}

TEST_F(AgentStateTest, CommandsInWrongStateAreHandled) {
  // Stop command from passive state - should still change state
  // (real implementation may handle this differently, but state machine
  // doesn't prevent it)
  EXPECT_EQ(state_machine->get_state(), passive);
  state_machine->process_command(Command{stop_allocation_tracing});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_stopping);
}

TEST_F(AgentStateTest, ReportCommandFromWrongStateStillTransitions) {
  // Report from passive - state machine allows this transition
  EXPECT_EQ(state_machine->get_state(), passive);
  state_machine->process_command(Command{report_allocation_tracing});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_reported);
}

TEST_F(AgentStateTest, FullStateTransitionCycle) {
  // Test complete state machine cycle
  EXPECT_EQ(state_machine->get_state(), passive);

  state_machine->process_command(Command{start_allocation_tracing});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);

  state_machine->process_allocation_event(START_MARKER, 1);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting);

  state_machine->process_object_free_event(1);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active);

  // Some regular allocations
  state_machine->process_allocation_event(REGULAR_CLASS, 10);
  state_machine->process_allocation_event(REGULAR_CLASS, 11);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active);

  state_machine->process_command(Command{stop_allocation_tracing});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_stopping);

  state_machine->process_allocation_event(FINISH_MARKER, 2);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushing);

  state_machine->process_object_free_event(2);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushed);

  state_machine->process_command(Command{report_allocation_tracing});
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_reported);
}

/// Marker Detection Tests
///
/// These tests focus specifically on the detection of start and finish markers,
/// verifying that marker flags are set correctly and that markers are only
/// recognized in the appropriate states.

TEST_F(AgentStateTest, StartMarkerSetsStartMarkerFlag) {
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_TRUE(rec->start_marker)
      << "start_marker flag should be true for start marker allocation";
  EXPECT_FALSE(rec->disable_marker)
      << "disable_marker flag should be false for start marker";
}

TEST_F(AgentStateTest, FinishMarkerSetsDisableMarkerFlag) {
  // Reach stopping state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});

  state_machine->process_allocation_event(FINISH_MARKER, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_TRUE(rec->disable_marker)
      << "disable_marker flag should be true for finish marker allocation";
  EXPECT_FALSE(rec->start_marker)
      << "start_marker flag should be false for finish marker";
}

TEST_F(AgentStateTest, RegularAllocationHasNoMarkerFlags) {
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);

  state_machine->process_allocation_event(REGULAR_CLASS, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->start_marker)
      << "start_marker flag should be false for regular allocation";
  EXPECT_FALSE(rec->disable_marker)
      << "disable_marker flag should be false for regular allocation";
}

TEST_F(AgentStateTest, StartMarkerInActiveStateNoFlag) {
  // Reach active state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_active);

  // Another start marker allocation in active state should not set flag
  state_machine->process_allocation_event(START_MARKER, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->start_marker)
      << "start_marker flag should not be set when in active state";
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active)
      << "State should remain active";
}

TEST_F(AgentStateTest, FinishMarkerInActiveStateNoFlag) {
  // Reach active state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_active);

  // Finish marker in active state should not set disable flag
  state_machine->process_allocation_event(FINISH_MARKER, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->disable_marker)
      << "disable_marker flag should not be set when in active state";
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active)
      << "State should remain active";
}

TEST_F(AgentStateTest, StartMarkerInStoppingStateNoFlag) {
  // Reach stopping state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_stopping);

  // Start marker in stopping state should not set start flag
  state_machine->process_allocation_event(START_MARKER, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->start_marker)
      << "start_marker flag should not be set when in stopping state";
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_stopping)
      << "State should remain stopping (not triggered by start marker)";
}

TEST_F(AgentStateTest, FinishMarkerInStartingStateNoFlag) {
  state_machine->process_command(Command{start_allocation_tracing});
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_starting);

  // Finish marker in starting state should not set disable flag
  state_machine->process_allocation_event(FINISH_MARKER, 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->disable_marker)
      << "disable_marker flag should not be set when in starting state";
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting)
      << "State should remain starting";
}

TEST_F(AgentStateTest, MultipleStartMarkersOnlyFirstSetsFlag) {
  state_machine->process_command(Command{start_allocation_tracing});

  // First start marker sets flag
  state_machine->process_allocation_event(START_MARKER, 100);
  const auto* rec1 = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec1, nullptr);
  EXPECT_TRUE(rec1->start_marker);

  // Free the first marker to transition to active
  state_machine->process_object_free_event(100);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_active);

  // Second start marker should not set flag (now in active state)
  state_machine->process_allocation_event(START_MARKER, 101);
  const auto* rec2 = state_machine->get_alloc_by_tag(101);
  ASSERT_NE(rec2, nullptr);
  EXPECT_FALSE(rec2->start_marker)
      << "Second start marker should not set flag after transition";
}

TEST_F(AgentStateTest, MultipleFinishMarkersOnlyFirstSetsFlag) {
  // Reach stopping state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});

  // First finish marker sets flag and transitions to flushing
  state_machine->process_allocation_event(FINISH_MARKER, 100);
  const auto* rec1 = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec1, nullptr);
  EXPECT_TRUE(rec1->disable_marker);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_flushing);

  // Second finish marker should not set flag (now in flushing state)
  state_machine->process_allocation_event(FINISH_MARKER, 101);
  const auto* rec2 = state_machine->get_alloc_by_tag(101);
  ASSERT_NE(rec2, nullptr);
  EXPECT_FALSE(rec2->disable_marker)
      << "Second finish marker should not set flag after transition";
}

TEST_F(AgentStateTest, StartMarkerFreedButNotInStartingState) {
  // Allocate start marker before entering starting state
  // This is an edge case - in practice the marker is allocated after
  // the start command, but tests the robustness of state checks

  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_active);

  // Now allocate another start marker
  state_machine->process_allocation_event(START_MARKER, 100);
  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  // Flag not set because not in starting state
  EXPECT_FALSE(rec->start_marker);

  // Freeing it should not cause transition
  state_machine->process_object_free_event(100);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_active)
      << "Freeing start marker in active state should not cause transition";
}

TEST_F(AgentStateTest, FinishMarkerFreedButNotInFlushingState) {
  // Reach stopping state
  state_machine->process_command(Command{start_allocation_tracing});
  state_machine->process_allocation_event(START_MARKER, 1);
  state_machine->process_object_free_event(1);
  state_machine->process_command(Command{stop_allocation_tracing});
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_stopping);

  // Allocate finish marker but don't process free yet
  state_machine->process_allocation_event(FINISH_MARKER, 100);
  ASSERT_EQ(state_machine->get_state(), allocation_tracing_flushing);

  // Allocate another finish marker (without disable flag)
  state_machine->process_allocation_event(FINISH_MARKER, 101);
  const auto* rec = state_machine->get_alloc_by_tag(101);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->disable_marker)
      << "Second finish marker should not have disable flag";

  // Freeing the second one should not cause flushed transition
  state_machine->process_object_free_event(101);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushing)
      << "Freeing non-disable marker should not cause flushed transition";

  // Freeing the first (with disable flag) should cause transition
  state_machine->process_object_free_event(100);
  EXPECT_EQ(state_machine->get_state(), allocation_tracing_flushed);
}

TEST_F(AgentStateTest, MarkerClassSignatureMustMatchExactly) {
  state_machine->process_command(Command{start_allocation_tracing});

  // Similar but not exact class signature
  state_machine->process_allocation_event(
      "Lcriterium/agent/Agent$AllocationStartMarkerFake;", 100);

  const auto* rec = state_machine->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_FALSE(rec->start_marker)
      << "Non-matching class signature should not set marker flag";

  EXPECT_EQ(state_machine->get_state(), allocation_tracing_starting)
      << "State should remain starting";
}

TEST_F(AgentStateTest, MarkerRecordsAreTrackedLikeRegularAllocations) {
  state_machine->process_command(Command{start_allocation_tracing});

  // Start marker is tracked
  state_machine->process_allocation_event(START_MARKER, 1);
  EXPECT_EQ(state_machine->get_alloc_count(), 1u);

  state_machine->process_object_free_event(1);

  // Regular allocations
  state_machine->process_allocation_event(REGULAR_CLASS, 10);
  state_machine->process_allocation_event(REGULAR_CLASS, 11);
  EXPECT_EQ(state_machine->get_alloc_count(), 3u);

  state_machine->process_command(Command{stop_allocation_tracing});

  // Finish marker is tracked
  state_machine->process_allocation_event(FINISH_MARKER, 2);
  EXPECT_EQ(state_machine->get_alloc_count(), 4u);
}
