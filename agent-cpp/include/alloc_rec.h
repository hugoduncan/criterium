#ifndef CRITERIUM_ALLOC_REC_H
#define CRITERIUM_ALLOC_REC_H

#include <jni.h>
#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace criterium {

// Allocation record for tracking object allocations.
struct AllocRec {
  std::string obj_class;
  jlong obj_size;

  std::string call_class;
  std::string call_method;
  std::string call_file;
  jlong call_line;

  std::string alloc_class;
  std::string alloc_method;
  std::string alloc_file;
  jlong alloc_line;

  jlong thread_id;
  jlong freed{};

  jlong tag;
  bool start_marker{};
  bool disable_marker{};

  static constexpr char const* no_file_name = "NO_SOURCE";

  // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
  AllocRec(char const* obj_class,
           jlong obj_size,
           char const* call_class,
           char const* call_method,
           char const* call_file,
           jlong call_line,
           char const* alloc_class,
           char const* alloc_method,
           char const* alloc_file,
           jlong alloc_line,
           jlong thread_id,
           jlong tag)
    : obj_class(obj_class),
      obj_size(obj_size),
      call_class(call_class == nullptr ? "" : call_class),
      call_method(call_method == nullptr ? "" : call_method),
      call_file(call_file == nullptr ? no_file_name : call_file),
      call_line(call_line),
      alloc_class(alloc_class == nullptr ? "" : alloc_class),
      alloc_method(alloc_method == nullptr ? "" : alloc_method),
      alloc_file(alloc_file == nullptr ? no_file_name : alloc_file),
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
