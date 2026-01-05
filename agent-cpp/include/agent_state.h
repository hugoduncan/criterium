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
#include "alloc_rec.h"
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
  jthread calling_thread = nullptr;  // Thread that sent the command (as global ref)

  void delete_global_refs(JNIEnv* env, IJniOperations& jni_ops) const {
    if (calling_thread != nullptr) {
      jni_ops.delete_global_ref(env, calling_thread);
    }
  }
};

/// Method entry event from JVMTI callback, queued for processing.
struct MethodEntryEvent {
  jthread thread;
  jmethodID method;

  void delete_global_refs(JNIEnv* env, IJniOperations& jni_ops) const {
    jni_ops.delete_global_ref(env, thread);
  }
};

/// Method exit event from JVMTI callback, queued for processing.
struct MethodExitEvent {
  jthread thread;
  jmethodID method;

  void delete_global_refs(JNIEnv* env, IJniOperations& jni_ops) const {
    jni_ops.delete_global_ref(env, thread);
  }
};

} // namespace criterium

#endif // CRITERIUM_AGENT_STATE_H
