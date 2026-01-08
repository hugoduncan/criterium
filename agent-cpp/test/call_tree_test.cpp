#include <gtest/gtest.h>
#include "include/call_tree.h"

using namespace criterium;

// Tests for CallTreeNode and ThreadCallState data structures.
// These tests verify the pure data structure operations without
// requiring any mocking of JVMTI/JNI operations.

/// CallTreeNode tests

TEST(CallTreeNodeConstruction, DefaultConstructorInitializesFields) {
  CallTreeNode node;

  EXPECT_EQ(node.class_name, "");
  EXPECT_EQ(node.method_name, "");
  EXPECT_EQ(node.source_file, "");
  EXPECT_EQ(node.line_number, -1);
  EXPECT_EQ(node.call_count, 0);
  EXPECT_TRUE(node.children.empty());
}

TEST(CallTreeNodeConstruction, ParameterizedConstructorSetsFields) {
  CallTreeNode node("Lmyapp/Core;", "process", "Core.java", 42);

  EXPECT_EQ(node.class_name, "Lmyapp/Core;");
  EXPECT_EQ(node.method_name, "process");
  EXPECT_EQ(node.source_file, "Core.java");
  EXPECT_EQ(node.line_number, 42);
  EXPECT_EQ(node.call_count, 0);
  EXPECT_TRUE(node.children.empty());
}

TEST(CallTreeNodeFindOrCreateChild, CreatesNewChildWhenNotFound) {
  CallTreeNode parent;

  CallTreeNode* child = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "Helper.java", 10);

  ASSERT_NE(child, nullptr);
  EXPECT_EQ(child->class_name, "Lmyapp/Helper;");
  EXPECT_EQ(child->method_name, "compute");
  EXPECT_EQ(child->source_file, "Helper.java");
  EXPECT_EQ(child->line_number, 10);
  EXPECT_EQ(parent.children.size(), 1u);
}

TEST(CallTreeNodeFindOrCreateChild, ReturnsExistingChildWhenFound) {
  CallTreeNode parent;

  // Create first child
  CallTreeNode* child1 = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "Helper.java", 10);
  child1->call_count = 5;

  // Find same child again
  CallTreeNode* child2 = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "Helper.java", 10);

  EXPECT_EQ(child1, child2);
  EXPECT_EQ(child2->call_count, 5);
  EXPECT_EQ(parent.children.size(), 1u);
}

TEST(CallTreeNodeFindOrCreateChild, MatchesOnClassAndMethodOnly) {
  CallTreeNode parent;

  // Create child with one source file
  CallTreeNode* child1 = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "Helper.java", 10);

  // Same class and method, different source file - should find existing
  CallTreeNode* child2 = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "OtherFile.java", 99);

  EXPECT_EQ(child1, child2);
  EXPECT_EQ(parent.children.size(), 1u);
}

TEST(CallTreeNodeFindOrCreateChild, CreatesDifferentChildForDifferentMethod) {
  CallTreeNode parent;

  CallTreeNode* child1 = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "Helper.java", 10);
  CallTreeNode* child2 = parent.find_or_create_child(
      "Lmyapp/Helper;", "process", "Helper.java", 20);

  EXPECT_NE(child1, child2);
  EXPECT_EQ(parent.children.size(), 2u);
}

TEST(CallTreeNodeFindOrCreateChild, CreatesDifferentChildForDifferentClass) {
  CallTreeNode parent;

  CallTreeNode* child1 = parent.find_or_create_child(
      "Lmyapp/Helper;", "compute", "Helper.java", 10);
  CallTreeNode* child2 = parent.find_or_create_child(
      "Lmyapp/Other;", "compute", "Other.java", 10);

  EXPECT_NE(child1, child2);
  EXPECT_EQ(parent.children.size(), 2u);
}

TEST(CallTreeNodeNodeCount, LeafNodeReturnsOne) {
  CallTreeNode leaf("Lmyapp/Leaf;", "method", "Leaf.java", 1);

  EXPECT_EQ(leaf.node_count(), 1u);
}

TEST(CallTreeNodeNodeCount, CountsAllNodesInSubtree) {
  CallTreeNode root;
  root.find_or_create_child("Lmyapp/A;", "a", "A.java", 1);
  root.find_or_create_child("Lmyapp/B;", "b", "B.java", 2);

  CallTreeNode* child_a = root.children[0].get();
  child_a->find_or_create_child("Lmyapp/C;", "c", "C.java", 3);

  // root + A + B + C = 4 nodes
  EXPECT_EQ(root.node_count(), 4u);
}

TEST(CallTreeNodeMaxDepth, LeafNodeReturnsOne) {
  CallTreeNode leaf("Lmyapp/Leaf;", "method", "Leaf.java", 1);

  EXPECT_EQ(leaf.max_depth(), 1u);
}

TEST(CallTreeNodeMaxDepth, ReturnsMaximumPathLength) {
  CallTreeNode root;
  CallTreeNode* a = root.find_or_create_child("Lmyapp/A;", "a", "A.java", 1);
  root.find_or_create_child("Lmyapp/B;", "b", "B.java", 2);

  CallTreeNode* c = a->find_or_create_child("Lmyapp/C;", "c", "C.java", 3);
  c->find_or_create_child("Lmyapp/D;", "d", "D.java", 4);

  // root -> A -> C -> D = depth 4
  // root -> B = depth 2
  // max = 4
  EXPECT_EQ(root.max_depth(), 4u);
}

/// ThreadCallState tests

TEST(ThreadCallStateConstruction, StartsEmpty) {
  ThreadCallState state;

  EXPECT_TRUE(state.empty());
  EXPECT_EQ(state.depth(), 0u);
  EXPECT_EQ(state.current(), nullptr);
}

TEST(ThreadCallStatePush, AddsNodeToStack) {
  ThreadCallState state;
  CallTreeNode node;

  state.push(&node);

  EXPECT_FALSE(state.empty());
  EXPECT_EQ(state.depth(), 1u);
  EXPECT_EQ(state.current(), &node);
}

TEST(ThreadCallStatePush, MultiplePushesIncreaseDepth) {
  ThreadCallState state;
  CallTreeNode node1, node2, node3;

  state.push(&node1);
  state.push(&node2);
  state.push(&node3);

  EXPECT_EQ(state.depth(), 3u);
  EXPECT_EQ(state.current(), &node3);
}

TEST(ThreadCallStatePop, RemovesNodeFromStack) {
  ThreadCallState state;
  CallTreeNode node1, node2;

  state.push(&node1);
  state.push(&node2);

  bool result = state.pop();

  EXPECT_TRUE(result);
  EXPECT_EQ(state.depth(), 1u);
  EXPECT_EQ(state.current(), &node1);
}

TEST(ThreadCallStatePop, ReturnsFalseWhenEmpty) {
  ThreadCallState state;

  bool result = state.pop();

  EXPECT_FALSE(result);
  EXPECT_TRUE(state.empty());
}

TEST(ThreadCallStatePop, PopToEmptyMakesStateEmpty) {
  ThreadCallState state;
  CallTreeNode node;

  state.push(&node);
  state.pop();

  EXPECT_TRUE(state.empty());
  EXPECT_EQ(state.current(), nullptr);
}

TEST(ThreadCallStateCurrent, ReturnsNullptrWhenEmpty) {
  ThreadCallState state;

  EXPECT_EQ(state.current(), nullptr);
}

TEST(ThreadCallStateCurrent, ReturnsTopOfStack) {
  ThreadCallState state;
  CallTreeNode node1, node2;

  state.push(&node1);
  EXPECT_EQ(state.current(), &node1);

  state.push(&node2);
  EXPECT_EQ(state.current(), &node2);

  state.pop();
  EXPECT_EQ(state.current(), &node1);
}

/// Integration tests for call tree building

TEST(CallTreeIntegration, SimulatesSimpleCallSequence) {
  // Simulate: main() -> helper() -> leaf()
  CallTreeNode root;
  ThreadCallState state;

  // Enter main
  CallTreeNode* main_node = root.find_or_create_child(
      "LMain;", "main", "Main.java", 10);
  main_node->call_count++;
  state.push(main_node);

  // Enter helper (called from main)
  CallTreeNode* current = state.current();
  CallTreeNode* helper_node = current->find_or_create_child(
      "LHelper;", "helper", "Helper.java", 20);
  helper_node->call_count++;
  state.push(helper_node);

  // Enter leaf (called from helper)
  current = state.current();
  CallTreeNode* leaf_node = current->find_or_create_child(
      "LLeaf;", "leaf", "Leaf.java", 30);
  leaf_node->call_count++;
  state.push(leaf_node);

  // Exit leaf, helper, main
  state.pop();
  state.pop();
  state.pop();

  // Verify structure
  EXPECT_EQ(root.children.size(), 1u);
  EXPECT_EQ(main_node->call_count, 1);
  EXPECT_EQ(main_node->children.size(), 1u);
  EXPECT_EQ(helper_node->call_count, 1);
  EXPECT_EQ(helper_node->children.size(), 1u);
  EXPECT_EQ(leaf_node->call_count, 1);
  EXPECT_EQ(leaf_node->children.size(), 0u);
  EXPECT_TRUE(state.empty());
}

TEST(CallTreeIntegration, SimulatesRepeatedCalls) {
  // Simulate: loop() calls work() 3 times
  CallTreeNode root;
  ThreadCallState state;

  // Enter loop
  CallTreeNode* loop_node = root.find_or_create_child(
      "LApp;", "loop", "App.java", 10);
  loop_node->call_count++;
  state.push(loop_node);

  // Call work 3 times
  for (int i = 0; i < 3; i++) {
    CallTreeNode* current = state.current();
    CallTreeNode* work_node = current->find_or_create_child(
        "LApp;", "work", "App.java", 20);
    work_node->call_count++;
    state.push(work_node);
    state.pop();
  }

  // Exit loop
  state.pop();

  // Verify: work should have call_count=3, but only 1 child node
  EXPECT_EQ(loop_node->children.size(), 1u);
  CallTreeNode* work_node = loop_node->children[0].get();
  EXPECT_EQ(work_node->call_count, 3);
  EXPECT_TRUE(state.empty());
}

TEST(CallTreeIntegration, SimulatesMultipleCallPaths) {
  // Simulate: main() calls both foo() and bar()
  CallTreeNode root;
  ThreadCallState state;

  // Enter main
  CallTreeNode* main_node = root.find_or_create_child(
      "LMain;", "main", "Main.java", 10);
  main_node->call_count++;
  state.push(main_node);

  // Call foo
  CallTreeNode* foo_node = state.current()->find_or_create_child(
      "LFoo;", "foo", "Foo.java", 20);
  foo_node->call_count++;
  state.push(foo_node);
  state.pop();

  // Call bar
  CallTreeNode* bar_node = state.current()->find_or_create_child(
      "LBar;", "bar", "Bar.java", 30);
  bar_node->call_count++;
  state.push(bar_node);
  state.pop();

  // Exit main
  state.pop();

  // Verify: main has two children
  EXPECT_EQ(main_node->children.size(), 2u);
  EXPECT_TRUE(state.empty());
}
