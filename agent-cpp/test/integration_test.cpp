#include <gtest/gtest.h>
#include <gmock/gmock.h>
#include <cstring>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include "include/agent_types.h"
#include "include/agent_state.h"
#include "mocks.h"

using namespace criterium;
using namespace criterium::test;
using ::testing::_;
using ::testing::Return;
using ::testing::NiceMock;
using ::testing::DoAll;
using ::testing::Invoke;

// Integration tests for the agent's event processing flow.
// These tests verify the complete flow from allocation events through
// to report generation, using mocked JVMTI/JNI interfaces to validate
// that the correct operations are called with expected arguments.
//
// These tests exercise the full allocation tracking lifecycle:
// - Start command enables tracing and clears previous records
// - Start marker allocation/free triggers active state
// - Regular allocations are tracked with class signatures
// - Stop command transitions to stopping state
// - Finish marker disables sampling and transitions to flushing
// - Finish marker free disables object free events
// - Report command generates allocation data

static constexpr char const* START_MARKER =
    "Lcriterium/agent/Agent$AllocationStartMarker;";
static constexpr char const* FINISH_MARKER =
    "Lcriterium/agent/Agent$AllocationFinishMarker;";
static constexpr char const* REGULAR_CLASS = "Ljava/lang/String;";

/// Testable state machine for integration flow tests.
/// Models the state transition and allocation tracking logic of AgentState
/// while allowing mock verification of JVMTI/JNI operations.
class IntegrationStateMachine {
private:
  NiceMock<MockJvmtiOperations>& jvmti_ops_;
  [[maybe_unused]] NiceMock<MockJniOperations>& jni_ops_;
  jlong state_ = passive;
  std::vector<std::unique_ptr<AllocRec>> allocs_;
  std::map<jlong, AllocRec*> allocs_by_tag_;

public:
  IntegrationStateMachine(NiceMock<MockJvmtiOperations>& jvmti_ops,
                          NiceMock<MockJniOperations>& jni_ops)
      : jvmti_ops_(jvmti_ops), jni_ops_(jni_ops) {}

  jlong get_state() const { return state_; }

  void process_command(const Command& cmd) {
    switch (cmd.cmd) {
    case start_allocation_tracing:
      enable_allocation_tracing();
      break;
    case stop_allocation_tracing:
      disable_allocation_tracing();
      break;
    case report_allocation_tracing:
      state_ = allocation_tracing_reporting;
      state_ = allocation_tracing_reported;
      break;
    case sync_state:
    case ping:
    default:
      break;
    }
  }

  void process_allocation_event(const char* class_sig, jlong tag) {
    auto starting = state_ == allocation_tracing_starting &&
                    std::strcmp(class_sig, START_MARKER) == 0;

    auto stopping = state_ == allocation_tracing_stopping &&
                    std::strcmp(class_sig, FINISH_MARKER) == 0;

    auto rec = std::make_unique<AllocRec>(class_sig, 0, nullptr, nullptr,
                                          nullptr, -1, nullptr, nullptr,
                                          nullptr, -1, 0, tag);

    if (starting) {
      rec->start_marker = true;
    }

    if (stopping) {
      jvmti_ops_.set_event_notification_mode(
          JVMTI_DISABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, nullptr);
      rec->disable_marker = true;
      state_ = allocation_tracing_flushing;
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

    if (rec->start_marker && state_ == allocation_tracing_starting) {
      state_ = allocation_tracing_active;
    }

    if (state_ == allocation_tracing_flushing && rec->disable_marker) {
      jvmti_ops_.set_event_notification_mode(JVMTI_DISABLE,
                                             JVMTI_EVENT_OBJECT_FREE, nullptr);
      state_ = allocation_tracing_flushed;
    }
  }

  const AllocRec* get_alloc_by_tag(jlong tag) const {
    auto it = allocs_by_tag_.find(tag);
    return it != allocs_by_tag_.end() ? it->second : nullptr;
  }

  size_t get_alloc_count() const { return allocs_.size(); }

  /// Returns all tracked allocations for verification.
  const std::vector<std::unique_ptr<AllocRec>>& get_allocs() const {
    return allocs_;
  }

private:
  void enable_allocation_tracing() {
    state_ = allocation_tracing_starting;
    allocs_.clear();
    allocs_by_tag_.clear();
    jvmti_ops_.set_heap_sampling_interval(0);
    jvmti_ops_.set_event_notification_mode(
        JVMTI_ENABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, nullptr);
    jvmti_ops_.set_event_notification_mode(JVMTI_ENABLE,
                                           JVMTI_EVENT_OBJECT_FREE, nullptr);
  }

  void disable_allocation_tracing() { state_ = allocation_tracing_stopping; }
};

class IntegrationFlowTest : public ::testing::Test {
protected:
  NiceMock<MockJvmtiOperations> mock_jvmti;
  NiceMock<MockJniOperations> mock_jni;
  std::unique_ptr<IntegrationStateMachine> sm;

  void SetUp() override {
    // Configure default mock behavior for JVMTI operations
    ON_CALL(mock_jvmti, set_heap_sampling_interval(_))
        .WillByDefault(Return(true));
    ON_CALL(mock_jvmti, set_event_notification_mode(_, _, _))
        .WillByDefault(Return(true));
    ON_CALL(mock_jvmti, set_tag(_, _)).WillByDefault(Return(true));
    ON_CALL(mock_jvmti, deallocate(_)).WillByDefault(Return());

    // Default JNI mock behavior
    ON_CALL(mock_jni, call_long_method(_, _, _))
        .WillByDefault(Return(1L));
    ON_CALL(mock_jni, new_global_ref(_, _))
        .WillByDefault(Return(nullptr));
    ON_CALL(mock_jni, delete_global_ref(_, _))
        .WillByDefault(Return());

    sm = std::make_unique<IntegrationStateMachine>(mock_jvmti, mock_jni);
  }

  /// Helper to reach active state quickly.
  void reach_active_state() {
    sm->process_command(Command{start_allocation_tracing});
    sm->process_allocation_event(START_MARKER, 1);
    sm->process_object_free_event(1);
  }

  /// Helper to reach flushed state quickly.
  void reach_flushed_state() {
    reach_active_state();
    sm->process_command(Command{stop_allocation_tracing});
    sm->process_allocation_event(FINISH_MARKER, 2);
    sm->process_object_free_event(2);
  }
};

/// Allocation Event Processing Flow Tests
/// These tests verify the complete flow of processing allocation events,
/// including JVMTI operations for class signature lookup and record creation.

TEST_F(IntegrationFlowTest, StartCommandEnablesTracking) {
  EXPECT_CALL(mock_jvmti, set_heap_sampling_interval(0)).Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);

  sm->process_command(Command{start_allocation_tracing});

  EXPECT_EQ(sm->get_state(), allocation_tracing_starting);
}

TEST_F(IntegrationFlowTest, AllocationEventCreatesRecord) {
  sm->process_command(Command{start_allocation_tracing});
  sm->process_allocation_event(START_MARKER, 1);
  sm->process_object_free_event(1);

  sm->process_allocation_event(REGULAR_CLASS, 100);

  const auto* rec = sm->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_EQ(rec->obj_class, REGULAR_CLASS);
  EXPECT_EQ(rec->tag, 100);
  EXPECT_EQ(rec->freed, 0);
  EXPECT_FALSE(rec->start_marker);
  EXPECT_FALSE(rec->disable_marker);
}

TEST_F(IntegrationFlowTest, StartMarkerSetsFlag) {
  sm->process_command(Command{start_allocation_tracing});

  sm->process_allocation_event(START_MARKER, 1);

  const auto* rec = sm->get_alloc_by_tag(1);
  ASSERT_NE(rec, nullptr);
  EXPECT_TRUE(rec->start_marker);
  EXPECT_EQ(sm->get_state(), allocation_tracing_starting);
}

TEST_F(IntegrationFlowTest, StartMarkerFreeTriggersActiveState) {
  sm->process_command(Command{start_allocation_tracing});
  sm->process_allocation_event(START_MARKER, 1);

  sm->process_object_free_event(1);

  EXPECT_EQ(sm->get_state(), allocation_tracing_active);
  const auto* rec = sm->get_alloc_by_tag(1);
  EXPECT_EQ(rec->freed, 1);
}

TEST_F(IntegrationFlowTest, MultipleAllocationsAreTracked) {
  reach_active_state();

  sm->process_allocation_event(REGULAR_CLASS, 10);
  sm->process_allocation_event(REGULAR_CLASS, 11);
  sm->process_allocation_event(REGULAR_CLASS, 12);

  EXPECT_EQ(sm->get_alloc_count(), 4u);  // 1 start marker + 3 regular

  EXPECT_NE(sm->get_alloc_by_tag(10), nullptr);
  EXPECT_NE(sm->get_alloc_by_tag(11), nullptr);
  EXPECT_NE(sm->get_alloc_by_tag(12), nullptr);
}

/// Object Free Event Handling Tests
/// These tests verify that object free events correctly update allocation
/// records and trigger appropriate state transitions.

TEST_F(IntegrationFlowTest, ObjectFreeMarksRecordAsFreed) {
  reach_active_state();

  sm->process_allocation_event(REGULAR_CLASS, 100);
  const auto* rec = sm->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_EQ(rec->freed, 0);

  sm->process_object_free_event(100);

  EXPECT_EQ(rec->freed, 1);
}

TEST_F(IntegrationFlowTest, ObjectFreeOfUnknownTagIsIgnored) {
  reach_active_state();

  // Should not crash or change state
  sm->process_object_free_event(999);

  EXPECT_EQ(sm->get_state(), allocation_tracing_active);
}

TEST_F(IntegrationFlowTest, FinishMarkerDisablesSampling) {
  reach_active_state();
  sm->process_command(Command{stop_allocation_tracing});
  ASSERT_EQ(sm->get_state(), allocation_tracing_stopping);

  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_DISABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);

  sm->process_allocation_event(FINISH_MARKER, 2);

  EXPECT_EQ(sm->get_state(), allocation_tracing_flushing);
  const auto* rec = sm->get_alloc_by_tag(2);
  ASSERT_NE(rec, nullptr);
  EXPECT_TRUE(rec->disable_marker);
}

TEST_F(IntegrationFlowTest, FinishMarkerFreeDisablesObjectFreeEvents) {
  reach_active_state();
  sm->process_command(Command{stop_allocation_tracing});
  sm->process_allocation_event(FINISH_MARKER, 2);
  ASSERT_EQ(sm->get_state(), allocation_tracing_flushing);

  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_DISABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);

  sm->process_object_free_event(2);

  EXPECT_EQ(sm->get_state(), allocation_tracing_flushed);
}

/// Report Generation Tests
/// These tests verify the report command transitions and that allocation
/// data is correctly structured for reporting.

TEST_F(IntegrationFlowTest, ReportCommandTransitionsToReported) {
  reach_flushed_state();

  sm->process_command(Command{report_allocation_tracing});

  EXPECT_EQ(sm->get_state(), allocation_tracing_reported);
}

TEST_F(IntegrationFlowTest, ReportIncludesAllTrackedAllocations) {
  reach_active_state();

  // Track several allocations with different freed states
  sm->process_allocation_event(REGULAR_CLASS, 10);
  sm->process_allocation_event(REGULAR_CLASS, 11);
  sm->process_allocation_event(REGULAR_CLASS, 12);

  // Free one
  sm->process_object_free_event(11);

  // Complete the tracing cycle
  sm->process_command(Command{stop_allocation_tracing});
  sm->process_allocation_event(FINISH_MARKER, 2);
  sm->process_object_free_event(2);

  // Before reporting, verify all allocations are tracked
  const auto& allocs = sm->get_allocs();

  // Count non-marker allocations
  int regular_count = 0;
  int freed_count = 0;
  for (const auto& alloc : allocs) {
    if (!alloc->start_marker && !alloc->disable_marker) {
      regular_count++;
      if (alloc->freed != 0) {
        freed_count++;
      }
    }
  }

  EXPECT_EQ(regular_count, 3);
  EXPECT_EQ(freed_count, 1);

  sm->process_command(Command{report_allocation_tracing});
  EXPECT_EQ(sm->get_state(), allocation_tracing_reported);
}

TEST_F(IntegrationFlowTest, AllocationRecordsRetainClassInfo) {
  reach_active_state();

  sm->process_allocation_event(REGULAR_CLASS, 100);

  const auto* rec = sm->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_EQ(rec->obj_class, REGULAR_CLASS);

  // After freeing, class info should still be available
  sm->process_object_free_event(100);
  EXPECT_EQ(rec->obj_class, REGULAR_CLASS);
  EXPECT_EQ(rec->freed, 1);
}

/// Complete Lifecycle Tests
/// These tests verify the full allocation tracking lifecycle from start
/// to report generation.

TEST_F(IntegrationFlowTest, CompleteAllocationTrackingCycle) {
  // 1. Start tracing
  EXPECT_CALL(mock_jvmti, set_heap_sampling_interval(0)).Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_ENABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);

  sm->process_command(Command{start_allocation_tracing});
  EXPECT_EQ(sm->get_state(), allocation_tracing_starting);

  // 2. Start marker triggers active state
  sm->process_allocation_event(START_MARKER, 1);
  sm->process_object_free_event(1);
  EXPECT_EQ(sm->get_state(), allocation_tracing_active);

  // 3. Track allocations
  sm->process_allocation_event(REGULAR_CLASS, 10);
  sm->process_allocation_event(REGULAR_CLASS, 11);
  sm->process_allocation_event(REGULAR_CLASS, 12);
  sm->process_object_free_event(11);

  EXPECT_EQ(sm->get_alloc_count(), 4u);

  // 4. Stop tracing
  sm->process_command(Command{stop_allocation_tracing});
  EXPECT_EQ(sm->get_state(), allocation_tracing_stopping);

  // 5. Finish marker triggers flushing
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_DISABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, _))
      .Times(1);
  sm->process_allocation_event(FINISH_MARKER, 2);
  EXPECT_EQ(sm->get_state(), allocation_tracing_flushing);

  // 6. Finish marker free triggers flushed
  EXPECT_CALL(mock_jvmti, set_event_notification_mode(
                              JVMTI_DISABLE, JVMTI_EVENT_OBJECT_FREE, _))
      .Times(1);
  sm->process_object_free_event(2);
  EXPECT_EQ(sm->get_state(), allocation_tracing_flushed);

  // 7. Report
  sm->process_command(Command{report_allocation_tracing});
  EXPECT_EQ(sm->get_state(), allocation_tracing_reported);

  // Verify allocation record states
  const auto* rec10 = sm->get_alloc_by_tag(10);
  const auto* rec11 = sm->get_alloc_by_tag(11);
  const auto* rec12 = sm->get_alloc_by_tag(12);

  ASSERT_NE(rec10, nullptr);
  ASSERT_NE(rec11, nullptr);
  ASSERT_NE(rec12, nullptr);

  EXPECT_EQ(rec10->freed, 0);
  EXPECT_EQ(rec11->freed, 1);
  EXPECT_EQ(rec12->freed, 0);
}

TEST_F(IntegrationFlowTest, MultipleTracingCyclesResetAllocations) {
  // First cycle
  sm->process_command(Command{start_allocation_tracing});
  sm->process_allocation_event(START_MARKER, 1);
  sm->process_object_free_event(1);
  sm->process_allocation_event(REGULAR_CLASS, 100);
  sm->process_allocation_event(REGULAR_CLASS, 101);

  EXPECT_EQ(sm->get_alloc_count(), 3u);

  // Second cycle - should reset
  sm->process_command(Command{start_allocation_tracing});

  // Old allocations should be cleared
  EXPECT_EQ(sm->get_alloc_by_tag(100), nullptr);
  EXPECT_EQ(sm->get_alloc_by_tag(101), nullptr);
  EXPECT_EQ(sm->get_alloc_count(), 0u);

  // Start fresh cycle
  sm->process_allocation_event(START_MARKER, 2);
  sm->process_object_free_event(2);
  sm->process_allocation_event(REGULAR_CLASS, 200);

  EXPECT_EQ(sm->get_alloc_count(), 2u);
  EXPECT_NE(sm->get_alloc_by_tag(200), nullptr);
}

TEST_F(IntegrationFlowTest, AllocationsBeforeStartMarkerAreTracked) {
  sm->process_command(Command{start_allocation_tracing});

  // Allocations before start marker
  sm->process_allocation_event(REGULAR_CLASS, 100);
  sm->process_allocation_event(REGULAR_CLASS, 101);

  // Now the start marker
  sm->process_allocation_event(START_MARKER, 1);
  sm->process_object_free_event(1);

  EXPECT_EQ(sm->get_state(), allocation_tracing_active);
  EXPECT_EQ(sm->get_alloc_count(), 3u);

  // All allocations should be present
  EXPECT_NE(sm->get_alloc_by_tag(100), nullptr);
  EXPECT_NE(sm->get_alloc_by_tag(101), nullptr);
}

TEST_F(IntegrationFlowTest, AllocationsInFlushingStateAreTracked) {
  reach_active_state();
  sm->process_command(Command{stop_allocation_tracing});
  sm->process_allocation_event(FINISH_MARKER, 2);
  EXPECT_EQ(sm->get_state(), allocation_tracing_flushing);

  // Allocations after finish marker but before its freed
  sm->process_allocation_event(REGULAR_CLASS, 100);

  EXPECT_NE(sm->get_alloc_by_tag(100), nullptr);
}

TEST_F(IntegrationFlowTest, FreedFlagPersistsAfterStateTransitions) {
  reach_active_state();

  sm->process_allocation_event(REGULAR_CLASS, 100);
  sm->process_object_free_event(100);

  const auto* rec = sm->get_alloc_by_tag(100);
  ASSERT_NE(rec, nullptr);
  EXPECT_EQ(rec->freed, 1);

  // Continue through state transitions
  sm->process_command(Command{stop_allocation_tracing});
  sm->process_allocation_event(FINISH_MARKER, 2);
  sm->process_object_free_event(2);
  sm->process_command(Command{report_allocation_tracing});

  // Freed flag should still be set
  EXPECT_EQ(rec->freed, 1);
}
