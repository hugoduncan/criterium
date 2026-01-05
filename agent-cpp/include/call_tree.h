#ifndef CRITERIUM_CALL_TREE_H
#define CRITERIUM_CALL_TREE_H

#include <jni.h>
#include <memory>
#include <string>
#include <vector>

namespace criterium {

/// A node in the method call tree.
/// Each node represents a method that was called, with metadata about the
/// method and a count of how many times it was invoked at this position
/// in the call tree.
struct CallTreeNode {
  std::string class_name;   // JVM class signature, e.g., "Lmyapp/Core;"
  std::string method_name;  // Method name, e.g., "process"
  std::string source_file;  // Source file name, e.g., "Core.java"
  jint line_number;         // Line number in source, or -1 if unknown
  jlong call_count;         // Number of times called at this tree position
  std::vector<std::unique_ptr<CallTreeNode>> children;

  CallTreeNode()
      : line_number(-1), call_count(0) {}

  CallTreeNode(std::string class_name, std::string method_name,
               std::string source_file, jint line_number)
      : class_name(std::move(class_name)),
        method_name(std::move(method_name)),
        source_file(std::move(source_file)),
        line_number(line_number),
        call_count(0) {}

  /// Find existing child with matching method signature, or create a new one.
  /// Returns pointer to the child node (owned by this node's children vector).
  CallTreeNode* find_or_create_child(const std::string& child_class,
                                     const std::string& child_method,
                                     const std::string& child_source,
                                     jint child_line) {
    // Search for existing child with same class and method
    for (auto& child : children) {
      if (child->class_name == child_class &&
          child->method_name == child_method) {
        return child.get();
      }
    }

    // Create new child
    auto new_child = std::make_unique<CallTreeNode>(
        child_class, child_method, child_source, child_line);
    CallTreeNode* ptr = new_child.get();
    children.push_back(std::move(new_child));
    return ptr;
  }

  /// Returns total number of nodes in this subtree (including this node).
  size_t node_count() const {
    size_t count = 1;
    for (const auto& child : children) {
      count += child->node_count();
    }
    return count;
  }

  /// Returns maximum depth of this subtree (1 for leaf node).
  size_t max_depth() const {
    size_t max_child_depth = 0;
    for (const auto& child : children) {
      max_child_depth = std::max(max_child_depth, child->max_depth());
    }
    return 1 + max_child_depth;
  }
};

/// Per-thread state for tracking position in the call tree during tracing.
struct ThreadCallState {
  std::vector<CallTreeNode*> stack;  // Current path through call tree

  /// Push a node onto the call stack.
  void push(CallTreeNode* node) {
    stack.push_back(node);
  }

  /// Pop a node from the call stack. Returns true if stack was non-empty.
  bool pop() {
    if (stack.empty()) {
      return false;
    }
    stack.pop_back();
    return true;
  }

  /// Returns current node (top of stack), or nullptr if stack is empty.
  CallTreeNode* current() const {
    return stack.empty() ? nullptr : stack.back();
  }

  /// Returns true if call stack is empty.
  bool empty() const {
    return stack.empty();
  }

  /// Returns current stack depth.
  size_t depth() const {
    return stack.size();
  }
};

} // namespace criterium

#endif // CRITERIUM_CALL_TREE_H
