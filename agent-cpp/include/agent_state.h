#ifndef CRITERIUM_AGENT_STATE_H
#define CRITERIUM_AGENT_STATE_H

#include <jni.h>
#include <jvmti.h>
#include <array>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "agent_types.h"
#include "jni_operations.h"
#include "jvmti_operations.h"

namespace criterium {

/// Allocation event from JVMTI callback, queued for processing.
struct AllocationEvent {
  jobject object;
  jclass object_klass;
  jthread thread;
  jlong size;
  jlong tag;
  std::array<jvmtiFrameInfo, MAX_FRAMES> frames;
  jint frame_count;

  void delete_global_refs(JNIEnv* env, IJniOperations& jni_ops) const {
    jni_ops.delete_global_ref(env, object_klass);
    jni_ops.delete_global_ref(env, object);
    jni_ops.delete_global_ref(env, thread);
  }
};

/// Object free event from JVMTI callback.
struct ObjectFreeEvent {
  jlong tag;
};

/// Command sent from Java to the agent.
struct Command {
  jlong cmd;
};

/// Allocation record for tracked objects.
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

  AllocRec(char const* obj_class, jlong obj_size, char const* call_class,
           char const* call_method, char const* call_file, jlong call_line,
           char const* alloc_class, char const* alloc_method,
           char const* alloc_file, jlong alloc_line, jlong thread_id, jlong tag)
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

} // namespace criterium

#endif // CRITERIUM_AGENT_STATE_H
