#include <gtest/gtest.h>
#include <atomic>
#include <chrono>
#include <thread>
#include <vector>
#include "include/message_queue.h"

using criterium::MessageQueue;

// Tests for MessageQueue, a thread-safe blocking queue.
// Contracts: push adds messages, pop blocks until available,
// stop signals termination while still draining pending messages.

class MessageQueueTest : public ::testing::Test {
protected:
  MessageQueue<int> queue;
};

TEST_F(MessageQueueTest, PushAndPopSingleMessage) {
  queue.push(42);

  int msg;
  bool result = queue.pop(msg);

  EXPECT_TRUE(result);
  EXPECT_EQ(msg, 42);
}

TEST_F(MessageQueueTest, PushAndPopMultipleMessages) {
  queue.push(1);
  queue.push(2);
  queue.push(3);

  int msg;
  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg, 1);

  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg, 2);

  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg, 3);
}

TEST_F(MessageQueueTest, MaintainsFifoOrder) {
  for (int i = 0; i < 100; ++i) {
    queue.push(i);
  }

  for (int i = 0; i < 100; ++i) {
    int msg;
    ASSERT_TRUE(queue.pop(msg));
    EXPECT_EQ(msg, i);
  }
}

TEST_F(MessageQueueTest, PopBlocksOnEmptyQueue) {
  std::atomic<bool> pop_started{false};
  std::atomic<bool> pop_completed{false};
  int received_msg = -1;

  std::thread consumer([&]() {
    int msg;
    pop_started = true;
    bool result = queue.pop(msg);
    if (result) {
      received_msg = msg;
    }
    pop_completed = true;
  });

  // Wait for consumer to start blocking
  while (!pop_started) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
  std::this_thread::sleep_for(std::chrono::milliseconds(10));

  // Consumer should still be blocked
  EXPECT_FALSE(pop_completed);

  // Push a message to unblock consumer
  queue.push(99);

  consumer.join();

  EXPECT_TRUE(pop_completed);
  EXPECT_EQ(received_msg, 99);
}

TEST_F(MessageQueueTest, StopUnblocksWaitingConsumer) {
  std::atomic<bool> pop_completed{false};
  std::atomic<bool> pop_result{true};

  std::thread consumer([&]() {
    int msg;
    pop_result = queue.pop(msg);
    pop_completed = true;
  });

  // Give consumer time to start blocking
  std::this_thread::sleep_for(std::chrono::milliseconds(10));

  EXPECT_FALSE(pop_completed);

  queue.stop();

  consumer.join();

  EXPECT_TRUE(pop_completed);
  EXPECT_FALSE(pop_result);  // Should return false when stopped and empty
}

TEST_F(MessageQueueTest, StopDrainsPendingMessages) {
  queue.push(1);
  queue.push(2);
  queue.push(3);

  queue.stop();

  // Should still be able to pop pending messages after stop
  int msg;
  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg, 1);

  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg, 2);

  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg, 3);

  // Now queue is empty and stopped, should return false
  EXPECT_FALSE(queue.pop(msg));
}

TEST_F(MessageQueueTest, StopReturnsFalseOnlyWhenEmpty) {
  queue.push(100);
  queue.stop();

  int msg;
  // First pop should succeed (message pending)
  bool first_result = queue.pop(msg);
  EXPECT_TRUE(first_result);
  EXPECT_EQ(msg, 100);

  // Second pop should fail (stopped and empty)
  bool second_result = queue.pop(msg);
  EXPECT_FALSE(second_result);
}

TEST_F(MessageQueueTest, MultipleProducersSingleConsumer) {
  const int num_producers = 4;
  const int messages_per_producer = 100;
  std::atomic<int> total_received{0};

  std::thread consumer([&]() {
    int msg;
    while (queue.pop(msg)) {
      total_received++;
    }
  });

  std::vector<std::thread> producers;
  for (int p = 0; p < num_producers; ++p) {
    producers.emplace_back([&, p]() {
      for (int i = 0; i < messages_per_producer; ++i) {
        queue.push(p * 1000 + i);
      }
    });
  }

  for (auto& producer : producers) {
    producer.join();
  }

  queue.stop();
  consumer.join();

  EXPECT_EQ(total_received, num_producers * messages_per_producer);
}

TEST_F(MessageQueueTest, SingleProducerMultipleConsumers) {
  const int num_messages = 100;
  const int num_consumers = 4;
  std::atomic<int> total_received{0};

  std::vector<std::thread> consumers;
  for (int c = 0; c < num_consumers; ++c) {
    consumers.emplace_back([&]() {
      int msg;
      while (queue.pop(msg)) {
        total_received++;
      }
    });
  }

  for (int i = 0; i < num_messages; ++i) {
    queue.push(i);
  }

  queue.stop();

  for (auto& consumer : consumers) {
    consumer.join();
  }

  EXPECT_EQ(total_received, num_messages);
}

TEST_F(MessageQueueTest, MultipleProducersMultipleConsumers) {
  const int num_producers = 4;
  const int num_consumers = 4;
  const int messages_per_producer = 50;
  std::atomic<int> total_received{0};

  std::vector<std::thread> consumers;
  for (int c = 0; c < num_consumers; ++c) {
    consumers.emplace_back([&]() {
      int msg;
      while (queue.pop(msg)) {
        total_received++;
      }
    });
  }

  std::vector<std::thread> producers;
  for (int p = 0; p < num_producers; ++p) {
    producers.emplace_back([&, p]() {
      for (int i = 0; i < messages_per_producer; ++i) {
        queue.push(p * 1000 + i);
      }
    });
  }

  for (auto& producer : producers) {
    producer.join();
  }

  queue.stop();

  for (auto& consumer : consumers) {
    consumer.join();
  }

  EXPECT_EQ(total_received, num_producers * messages_per_producer);
}

// Test with a more complex message type to verify move semantics
struct ComplexMessage {
  std::string data;
  int id;
};

TEST(MessageQueueComplexTest, HandlesComplexTypes) {
  MessageQueue<ComplexMessage> queue;

  queue.push(ComplexMessage{"hello", 1});
  queue.push(ComplexMessage{"world", 2});

  ComplexMessage msg;
  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg.data, "hello");
  EXPECT_EQ(msg.id, 1);

  ASSERT_TRUE(queue.pop(msg));
  EXPECT_EQ(msg.data, "world");
  EXPECT_EQ(msg.id, 2);
}
