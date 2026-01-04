#ifndef CRITERIUM_ALLOC_REC_H
#define CRITERIUM_ALLOC_REC_H

#include <algorithm>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace criterium {

// Allocation record for tracking object allocations.
// Uses int64_t for portability (maps to jlong in JNI).
struct AllocRec {
  std::string obj_class;
  int64_t obj_size;

  std::string call_class;
  std::string call_method;
  std::string call_file;
  int64_t call_line;

  std::string alloc_class;
  std::string alloc_method;
  std::string alloc_file;
  int64_t alloc_line;

  int64_t thread_id;
  int64_t freed{};

  int64_t tag;
  bool start_marker{};
  bool disable_marker{};

  AllocRec(const char* obj_class,
           int64_t obj_size,
           const char* call_class,
           const char* call_method,
           const char* call_file,
           int64_t call_line,
           const char* alloc_class,
           const char* alloc_method,
           const char* alloc_file,
           int64_t alloc_line,
           int64_t thread_id,
           int64_t tag)
    : obj_class(obj_class),
      obj_size(obj_size),
      call_class(call_class != nullptr ? call_class : ""),
      call_method(call_method != nullptr ? call_method : ""),
      call_file(call_file != nullptr ? call_file : "<no_file>"),
      call_line(call_line),
      alloc_class(alloc_class != nullptr ? alloc_class : ""),
      alloc_method(alloc_method != nullptr ? alloc_method : ""),
      alloc_file(alloc_file != nullptr ? alloc_file : "<no_file>"),
      alloc_line(alloc_line),
      thread_id(thread_id),
      tag(tag) {}
};

using AllocsT = std::vector<std::unique_ptr<AllocRec>>;

// Extracts all tags from a collection of allocation records.
// Works with any container of unique_ptr<T> where T has a `tag` member.
// Returns a vector of tag values in the same order as the input records,
// with initial capacity reserved based on input size.
template <typename T>
inline auto all_tags(const std::vector<std::unique_ptr<T>>& allocs) {
  auto tags = std::vector<decltype(std::declval<T>().tag)>();
  tags.reserve(allocs.size());
  std::transform(allocs.begin(),
                 allocs.end(),
                 std::back_inserter(tags),
                 std::mem_fn(&T::tag));
  return tags;
}

}  // namespace criterium

#endif  // CRITERIUM_ALLOC_REC_H
